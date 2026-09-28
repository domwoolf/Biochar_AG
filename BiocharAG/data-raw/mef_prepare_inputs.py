#!/usr/bin/env python3
"""
mef_prepare_inputs.py -- build the IAMC inputs for the MEF(P) fits (revision 2, 28 Sep 2026).

1. NGFS central-fit input (ngfs_central_input.parquet):
   - GCAM 6.0 and REMIND-MAgPIE 3.3-4.8 from IAM_data.xlsx (native regions: USA, EU, China, India).
   - MESSAGEix-GLOBIOM 2.0 R12: native China region, plus the NGFS DOWNSCALED country data for the
     USA, India and the EU-27 (sum of member states) in place of the R12 substitutes (North America,
     South Asia, Western + Eastern Europe). Only 5-year steps are kept, matching the native data.
     The downscaled data has no CCS split, so each country's coal and gas generation is split using
     its parent R12 region's CCS share (by scenario and year). The EU-27 carbon price is the
     generation-weighted mean of member-state prices.
(An AR6-based Monte Carlo spread was tried and dropped on 28 Sep 2026: the spread is the NGFS bootstrap;
see bak/mef_spread_ar6.py.)

Usage (from BiocharAG/):  python data-raw/mef_prepare_inputs.py
Inputs:  ../GIS/raw/ngfs/iam_data_raw.parquet, message_downscaled_raw.parquet (xlsx caches),
"""
from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

NGFS = Path("../GIS/raw/ngfs")
MSG = "MESSAGEix-GLOBIOM 2.0-M-R12-NGFS"
MSG_REG = "MESSAGEix-GLOBIOM 2.0-R12"
SE = "Secondary Energy|Electricity"
YEARS = [str(y) for y in range(2020, 2105, 5)]

EU27 = ["AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
        "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"]
# MESSAGE R12 Eastern Europe members of the EU-27; the other members are in Western Europe
EEU = {"BGR", "HRV", "CZE", "EST", "HUN", "LVA", "LTU", "POL", "ROU", "SVK", "SVN"}
PARENT = {"USA": "North America", "IND": "South Asia",
          **{c: ("Eastern Europe" if c in EEU else "Western Europe") for c in EU27}}
TARGET = {"USA": "USA", "IND": "IND", **{c: "EU27" for c in EU27}}


def ccs_shares(native: pd.DataFrame) -> pd.DataFrame:
    """CCS share of coal and gas generation per parent R12 region, scenario and year."""
    m = native[native["Model"] == MSG]
    rows = []
    for fuel in ("Coal", "Gas"):
        tot = m[m["Variable"] == f"{SE}|{fuel}"].set_index(["Scenario", "Region"])[YEARS]
        ccs = m[m["Variable"] == f"{SE}|{fuel}|w/ CCS"].set_index(["Scenario", "Region"])[YEARS]
        sh = (ccs.reindex(tot.index).fillna(0.0) / tot.replace(0, np.nan)).clip(0, 1).fillna(0.0)
        sh["fuel"] = fuel
        rows.append(sh.reset_index())
    out = pd.concat(rows)
    out["Region"] = out["Region"].str.replace(f"{MSG_REG}|", "", regex=False)
    return out


def downscaled_message(native: pd.DataFrame, ccs_split: bool = True) -> pd.DataFrame:
    ds = pd.read_parquet(NGFS / "message_downscaled_raw.parquet")
    ds = ds[ds["Region"].isin(TARGET)]
    keep = ["Price|Carbon", SE] + [f"{SE}|{f}" for f in
                                   ("Coal", "Gas", "Oil", "Biomass", "Nuclear", "Hydro", "Wind", "Solar", "Geothermal")]
    ds = ds[ds["Variable"].isin(keep)][["Scenario", "Region", "Variable", "Unit"] + YEARS].copy()
    sh = ccs_shares(native)
    if not ccs_split:  # diagnostic: treat all coal and gas as unabated
        sh[YEARS] = 0.0

    # Split coal and gas with the parent region's CCS share
    parts = [ds[~ds["Variable"].isin([f"{SE}|Coal", f"{SE}|Gas"])]]
    for fuel in ("Coal", "Gas"):
        g = ds[ds["Variable"] == f"{SE}|{fuel}"].copy()
        g["parent"] = g["Region"].map(PARENT)
        s = sh[sh["fuel"] == fuel].rename(columns={"Region": "parent"})
        g = g.merge(s, on=["Scenario", "parent"], how="left", suffixes=("", "_sh"))
        share = g[[y + "_sh" for y in YEARS]].fillna(0.0).to_numpy()
        vals = g[YEARS].to_numpy(float)
        for suffix, frac in (("w/ CCS", share), ("w/o CCS", 1 - share)):
            out = g[["Scenario", "Region", "Unit"]].copy()
            out["Variable"] = f"{SE}|{fuel}|{suffix}"
            out[YEARS] = vals * frac
            parts.append(out)
    ds = pd.concat(parts, ignore_index=True)

    # Aggregate to target regions; price weighted by total generation
    ds["target"] = ds["Region"].map(TARGET)
    gen = ds[ds["Variable"] == SE].set_index(["Scenario", "Region"])[YEARS]
    price = ds[ds["Variable"] == "Price|Carbon"].set_index(["Scenario", "Region"])[YEARS]
    agg = []
    for tgt, g in ds.groupby("target"):
        ext = g[g["Variable"] != "Price|Carbon"].groupby(["Scenario", "Variable", "Unit"])[YEARS].sum(min_count=1).reset_index()
        pr = price[price.index.get_level_values("Region").map(TARGET) == tgt]
        w = gen.reindex(pr.index)
        pw = ((pr * w).groupby(level="Scenario").sum(min_count=1) / w.where(pr.notna()).groupby(level="Scenario").sum(min_count=1))
        pw = pw.reset_index()
        pw["Variable"], pw["Unit"] = "Price|Carbon", "US$2010/t CO2"
        a = pd.concat([ext, pw], ignore_index=True)
        a["Region"] = f"{MSG_REG}|{tgt}"
        agg.append(a)
    out = pd.concat(agg, ignore_index=True)
    out["Model"] = MSG
    return out[["Model", "Scenario", "Region", "Variable", "Unit"] + YEARS]


def main():
    import sys
    if "--no-ccs-split" in sys.argv:
        native = pd.read_parquet(NGFS / "iam_data_raw.parquet")[["Model", "Scenario", "Region", "Variable", "Unit"] + YEARS]
        keep_native = ~((native["Model"] == MSG) & (native["Region"] != f"{MSG_REG}|China"))
        pd.concat([native[keep_native], downscaled_message(native, ccs_split=False)],
                  ignore_index=True).to_parquet(NGFS / "ngfs_central_input_noccs.parquet")
        print("Wrote ngfs_central_input_noccs.parquet (diagnostic, no CCS split)")
        return
    native = pd.read_parquet(NGFS / "iam_data_raw.parquet")
    native = native[["Model", "Scenario", "Region", "Variable", "Unit"] + YEARS]
    keep_native = ~((native["Model"] == MSG) & (native["Region"] != f"{MSG_REG}|China"))
    central = pd.concat([native[keep_native], downscaled_message(native)], ignore_index=True)
    central.to_parquet(NGFS / "ngfs_central_input.parquet")
    print("NGFS central input:", central.shape, sorted(central.loc[central["Model"] == MSG, "Region"].unique()))


if __name__ == "__main__":
    main()
