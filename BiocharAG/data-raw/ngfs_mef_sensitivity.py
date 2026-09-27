#!/usr/bin/env python3
"""
ngfs_mef_sensitivity.py -- sensitivity runs for ngfs_mef_fit.py (brief step 8).

Runs the fit with one setting changed at a time and summarises, per region: the chosen
spec, P50/k (or the two-stage parameters), pooled RMSE, and the anchored MEF at $50, $100
and $200/t for a representative anchor (biomass-weighted median of the TEA's empirical
build-margin layer, life-cycle tCO2/MWh) at the region's current effective price.

Usage (from the folder holding the cached pull):
  python ngfs_mef_sensitivity.py --file iam_data_raw.parquet --out sens [--bootstrap 100]
"""
from __future__ import annotations

import argparse
import copy
import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("mef", HERE / "ngfs_mef_fit.py")
mef = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mef)

# Representative anchors: biomass-weighted median of <prefix>_ff_c_intensity.tif (x 3.6)
REP_ANCHOR = {"USA": 0.341, "EU": 0.038, "China": 0.378, "India": 0.574}

BASE = {}  # defaults from ngfs_mef_fit.CONFIG
RUNS = {
    "base":             {},
    "price_end":        {"price_timing": "end"},
    "ref_price_10":     {"ref_price_max": 10.0},
    "ref_price_25":     {"ref_price_max": 25.0},
    "weight_0":         {"weight_power": 0.0},
    "weight_1":         {"weight_power": 1.0},
    "no_boot_models":   {"bootstrap_models": False},
    "capacity_bm":      {"bm_method": "capacity"},
}


def summarise(out: Path, label: str, variant: str) -> list[dict]:
    fp = pd.read_csv(out / "fit_params.csv")
    rows = []
    for _, r in fp[(fp["scope"] == "POOLED") & fp["chosen"]].iterrows():
        th = {k: r[k] for k in ("P50", "k", "P50_1", "k1", "P50_2", "k2", "s") if k in r and pd.notna(r[k])}
        if "s" not in th:
            th["lam"] = 0.0
        reg = r["region"]
        p_now = mef.CONFIG["anchors"][reg]["p_now"]
        vals = {}
        for P in (50, 100, 200):
            m, mmax = mef.mef_curve(np.array([P], float), th, REP_ANCHOR[reg], p_now, mef.mef_floor())
            vals[f"mef_{P}"] = float(m[0])
        vals["mef_max"] = float(mmax)
        rows.append({"variant": variant, "run": label, "region": reg, "spec": r["spec"],
                     **{k: round(float(v), 3) for k, v in th.items() if k != "lam"},
                     "rmse": round(float(r["rmse"]), 3), "h_now": float(mef.H(p_now, 2030, th)),
                     **vals})
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--file", required=True)
    ap.add_argument("--out", default="sens")
    ap.add_argument("--bootstrap", type=int, default=100)
    a = ap.parse_args()
    df = mef.load_file(a.file)
    df = df[df["variable"].isin(mef.required_variables())]
    base_cfg = copy.deepcopy(mef.CONFIG)
    rows = []
    for variant, excl in {"all_models": r"(?i)damage", "no_remind": r"(?i)damage|REMIND"}.items():
        for label, change in RUNS.items():
            mef.CONFIG.clear()
            mef.CONFIG.update(copy.deepcopy(base_cfg))
            mef.CONFIG.update(model_exclude=excl, bootstrap=a.bootstrap, **change)
            out = Path(a.out) / variant / label
            try:
                mef.run(df, out)
                rows += summarise(out, label, variant)
            except SystemExit as e:
                print(f"{variant}/{label}: failed ({e})")
    res = pd.DataFrame(rows)
    res.to_csv(Path(a.out) / "sensitivity_summary.csv", index=False)
    pd.set_option("display.width", 250)
    print(res.drop(columns=["h_now"]).round(3).to_string(index=False))


if __name__ == "__main__":
    main()
