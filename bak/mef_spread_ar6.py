#!/usr/bin/env python3
"""
mef_spread.py -- Monte Carlo spread for MEF(P), re-centred on the NGFS central curves
(revision 2, 28 Sep 2026).

1. Pooled single-stage fit (equal weight per model) to NGFS Phase 5 (GCAM, MESSAGE downscaled,
   REMIND) + IPCC AR6 R10 (vetted scenarios with a carbon price), bootstrapped over models and
   scenarios (ngfs_mef_fit.bootstrap; models resampled with equal weight).
   AR6 regions are R10 aggregates: R10NORTH_AM, R10EUROPE, R10CHINA+, R10INDIA+.
2. Each draw d is expressed as multiplicative deviations from the pooled central fit,
   fP = P50_d / P50_pool and fk = k_d / k_pool, and applied to the NGFS central curve:
   single-stage P50 * fP, k * fk; two-stage (EU) P50_1, P50_2 * fP and k1, k2 * fk, s unchanged.
   The MC then carries the inter-model spread of the wider ensemble around the NGFS centre.
3. Writes inst/extdata/mef_shape_draws.csv (NGFS central without REMIND) and
   mef_shape_draws_all_models.csv (NGFS central with REMIND); draw 0 = the NGFS central fit.

Usage (from BiocharAG/, after mef_prepare_inputs.py and the two NGFS central fits):
  python data-raw/mef_spread.py --central ../GIS/raw/ngfs/final_v2 \\
      --central-all ../GIS/raw/ngfs/final_v2_all --out ../GIS/raw/mef_spread [--bootstrap 1000]
"""
from __future__ import annotations

import argparse
import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("mef", HERE / "ngfs_mef_fit.py")
mef = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mef)

COLS = ["region", "draw", "spec", "P50", "k", "P50_1", "k1", "P50_2", "k2", "s"]


def pooled_spread(n_boot: int, out: Path) -> tuple[dict, pd.DataFrame]:
    C = mef.CONFIG
    C["regions"] = {
        "USA": {"include": [r"(?i)^(USA|US|United States( of America)?|R10NORTH_AM)$"]},
        "China": {"include": [r"(?i)^(China|R10CHINA\+)$"]},
        "India": {"include": [r"(?i)^(India|IND|R10INDIA\+)$"]},
        "EU": {"include": [r"(?i)^(EU ?-?2[78]|EU-1[25]|R10EUROPE)$"], "aggregate": True},
    }
    C["region_overrides"] = {}
    C["region_exclude"] = None
    C["model_exclude"] = r"(?i)damage"          # REMIND kept: its disagreement is part of the spread
    C["specs"], C["spec_choice"], C["spec_force"] = ["one"], "one", {}
    C["bootstrap"] = n_boot
    ngfs = mef.load_file("../GIS/raw/ngfs/ngfs_central_input.parquet")
    ar6 = mef.load_file("../GIS/raw/ar6/ar6_vetted_input.parquet")
    df = pd.concat([ngfs, ar6], ignore_index=True)
    df = df[df["variable"].isin(mef.required_variables())]
    mef.run(df, out)
    fp = pd.read_csv(out / "fit_params.csv")
    pooled = {r["region"]: (r["P50"], r["k"]) for _, r in fp[(fp["scope"] == "POOLED") & fp["chosen"]].iterrows()}
    draws = pd.read_csv(out / "bootstrap_draws.csv")
    return pooled, draws


def recentre(central_dir: Path, pooled: dict, draws: pd.DataFrame) -> pd.DataFrame:
    fp = pd.read_csv(central_dir / "fit_params.csv")
    cen = fp[(fp["scope"] == "POOLED") & fp["chosen"]].set_index("region")
    rows = []
    for reg, c in cen.iterrows():
        base = {k: c[k] for k in COLS if k in c and k not in ("region", "draw")}
        rows.append({"region": reg, "draw": 0, **base})
        p50p, kp = pooled[reg]
        d = draws[draws["region"] == reg].reset_index(drop=True)
        for i, r in d.iterrows():
            fP, fk = r["P50"] / p50p, r["k"] / kp
            row = {"region": reg, "draw": i + 1, "spec": c["spec"]}
            if c["spec"] == "two":
                row.update(P50_1=c["P50_1"] * fP, P50_2=c["P50_2"] * fP, k1=c["k1"] * fk, k2=c["k2"] * fk, s=c["s"])
            else:
                row.update(P50=c["P50"] * fP, k=c["k"] * fk)
            rows.append(row)
    return pd.DataFrame(rows).reindex(columns=COLS)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--central", required=True)
    ap.add_argument("--central-all", required=True)
    ap.add_argument("--out", default="../GIS/raw/mef_spread")
    ap.add_argument("--bootstrap", type=int, default=1000)
    a = ap.parse_args()
    out = Path(a.out)
    pooled, draws = pooled_spread(a.bootstrap, out)
    print("Pooled NGFS + AR6 single-stage fits (P50, k):", {r: (round(p, 1), round(k, 2)) for r, (p, k) in pooled.items()})
    for central, fn in ((a.central, "mef_shape_draws.csv"), (a.central_all, "mef_shape_draws_all_models.csv")):
        tab = recentre(Path(central), pooled, draws)
        tab.to_csv(HERE.parent / "inst" / "extdata" / fn, index=False, float_format="%.5g")
        q = tab[tab["draw"] > 0].groupby("region").agg(
            P50_p05=("P50", lambda x: np.nanpercentile(x, 5) if x.notna().any() else np.nan),
            P50_p95=("P50", lambda x: np.nanpercentile(x, 95) if x.notna().any() else np.nan),
            P50_1_p05=("P50_1", lambda x: np.nanpercentile(x, 5) if x.notna().any() else np.nan),
            P50_1_p95=("P50_1", lambda x: np.nanpercentile(x, 95) if x.notna().any() else np.nan))
        print(fn); print(tab[tab["draw"] == 0].to_string(index=False)); print(q.round(1).to_string())


if __name__ == "__main__":
    main()
