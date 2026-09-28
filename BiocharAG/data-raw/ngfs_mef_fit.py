#!/usr/bin/env python3
"""
ngfs_mef_fit.py -- price-dependent marginal emission factor curves, MEF(P),
fitted to NGFS integrated-assessment scenarios.

WHAT IT DOES
  1. Pulls (pyam) or reads (CSV/XLSX export) NGFS scenario data: carbon price,
     electricity generation by technology, and optionally capacity and
     capacity additions.
  2. For each model x scenario x region x time step, computes the build-margin
     emission intensity of new generation (tCO2/MWh).
  3. Normalises that intensity by the model's own low-price build margin, so
     only the SHAPE of the decline with carbon price is learned. (IAM base-year
     intensities rarely match an empirical baseline, so levels are not
     transferred -- only the shape.)
  4. Fits Hill (log-logistic) curves per model and pooled (equal model
     weights), optionally with time drift or a second (coal->gas) stage, and
     selects a specification by AIC.
  5. Bootstraps over models and scenarios for uncertainty.
  6. Anchors each regional curve to YOUR empirical point (MEF_emp at P_now)
     and writes MEF(P) tables with 5-95% bands for use in the TEA.

CURVE
  H(P)    = 1 / (1 + (P/P50)^k)                   fraction of gap to floor remaining
  2-stage : H = s*H(P;P50_2,k2) + (1-s)*H(P;P50_1,k1)  with P50_1 < P50_2
            (s = fraction of the gap remaining on the intermediate plateau)
  drift   : P50(t) = P50 * exp(-lam * (t - T_REF))
  MEF(P)  = MEF_min + (MEF_max - MEF_min) * H(P)
  MEF_max = MEF_min + (MEF_emp - MEF_min) / H(P_now)   (curve passes through anchor)

USAGE
  python ngfs_mef_fit.py --selftest
  python ngfs_mef_fit.py --source pyam --db ngfs_phase_5 --cache ngfs_raw.csv --list-regions
  python ngfs_mef_fit.py --source pyam --db ngfs_phase_5 --cache ngfs_raw.csv --out results
  python ngfs_mef_fit.py --source file --file ngfs_export.csv --out results

  pyam : pip install pyam-iamc ; list database names with
         pyam.iiasa.Connection().valid_connections (use the latest NGFS phase)
  file : IAMC-format CSV/XLSX exported from the NGFS Scenario Explorer
         (Model, Scenario, Region, Variable, Unit, 2020, 2025, ...) or long format.

BEFORE A REAL RUN, EDIT CONFIG BELOW
  - "anchors"   : your empirical MEF and current effective carbon price per region
  - "ef_basis" / "mef_min" : must match the basis (direct vs life-cycle) of your baseline
  - run --list-regions and add "region_overrides" where the automatic mapping fails
  - "deflators" : convert IAM price units (usually US$2010) to your currency year

OUTPUTS (in --out)
  points_raw.csv, points_normalised.csv  build-margin points per model/scenario/step
  diagnostics.csv                        method used, variable coverage, emissions check
  fit_params.csv                         per-model and pooled fits, AIC, chosen spec
  bootstrap_draws.csv                    parameter draws (Monte Carlo input)
  shape_curves.csv                       H(P) with bands (usable before anchors are set)
  mef_curves.csv                         anchored MEF(P) with bands (regions with anchors)
  fit_<region>.png                       diagnostic plots
"""
from __future__ import annotations

import argparse
import json
import logging
import re
import sys
from pathlib import Path
from types import SimpleNamespace

import numpy as np
import pandas as pd
from scipy.optimize import least_squares

log = logging.getLogger("mef")

SE = "Secondary Energy|Electricity"
CAP = "Capacity|Electricity"
ADD = "Capacity Additions|Electricity"
WO = "|w/o CCS"

# =============================================================================
# CONFIGURATION
# =============================================================================
CONFIG = {
    # ---- IAMC variable names used in NGFS ----
    "price_var": "Price|Carbon",
    "elec_total_var": SE,
    "elec_emis_var": "Emissions|CO2|Energy|Supply|Electricity",

    # tech key: (IAMC suffix, direct EF, life-cycle EF)   [tCO2/MWh]
    # Life-cycle EFs = the IPCC values used for the TEA's empirical anchor
    # (BiocharAG/data-raw/generate_marginal_ci.R), so curve shape and anchor share
    # one basis. CCS rows have no IPCC value there; typical values are kept.
    # Biomass = 0: the MEF floor excludes negative values and bio life-cycle
    # emissions are handled explicitly in the TEA (the anchor uses 0 as well).
    "techs": {
        "coal":        ("Coal|w/o CCS",    0.95, 0.820),
        "coal_ccs":    ("Coal|w/ CCS",     0.10, 0.15),
        "gas":         ("Gas|w/o CCS",     0.40, 0.490),
        "gas_ccs":     ("Gas|w/ CCS",      0.04, 0.10),
        "oil":         ("Oil|w/o CCS",     0.75, 0.700),
        "biomass":     ("Biomass|w/o CCS", 0.00, 0.00),
        "biomass_ccs": ("Biomass|w/ CCS",  0.00, 0.00),
        "nuclear":     ("Nuclear",         0.00, 0.012),
        "hydro":       ("Hydro",           0.00, 0.024),
        "wind":        ("Wind",            0.00, 0.0115),
        "solar":       ("Solar",           0.00, 0.048),
        "geothermal":  ("Geothermal",      0.00, 0.038),
    },
    "ef_basis": "lifecycle",                          # "direct" | "lifecycle"
    "mef_min": {"direct": 0.0, "lifecycle": 0.02},    # floor, same basis as above

    # ---- build-margin estimation ----
    # "auto": capacity additions x implied capacity factor if reported for the
    # key techs, else positive generation increments; "average" = fleet average
    # intensity (lags the margin; diagnostic only).
    # Study decision (27 Sep 2026): "average". In GCAM and REMIND the build margin is already near
    # the floor at zero carbon price (new capacity is mostly renewable), so it carries little price
    # signal and most model-regions fail normalisation. The fleet-average intensity tracks the
    # decarbonisation of the whole system that a baseload plant displaces over its life; its SHAPE is
    # applied to the TEA's empirical build-margin anchors.
    "bm_method": "average",         # auto | capacity | increment | average  (override with --bm-method)
    "price_timing": "mid",          # price for a step: mean of ends ("mid") or "end"
    "min_new_frac": 0.005,          # drop steps where new gen < 0.5% of total gen
    "weight_power": 0.5,            # point weight = (new TWh) ** power
    "years": (2025, 2060),          # step end-years used

    # ---- normalisation reference (model's own low-price build margin) ----
    "ref_price_max": 15.0,          # steps with price <= this (target currency) ...
    "ref_year_max": 2035,           # ... and year <= this
    "t_ref": 2030,                  # reference year for time drift
    "t_now": 2025,                  # year of your empirical anchor

    # ---- currency: multiply IAM price by deflator[unit year] -> target year ----
    # US CPI-U ratios to 2024 (verify / replace with your deflator).
    "deflators": {2010: 1.44, 2015: 1.32, 2017: 1.28, 2020: 1.21, 2024: 1.00},

    # ---- filters ----
    # Physical-damage variants duplicate scenarios; REMIND excluded from the central fit (time and price
    # confounded; study lead sign-off 27 Sep 2026). Use --model-exclude "(?i)damage" to include REMIND.
    "model_exclude": r"(?i)damage|REMIND",
    "scenario_exclude": None,         # e.g. r"(?i)low demand"

    # ---- regions: regex on the LAST '|'-segment of the region name ----
    "regions": {
        "USA":   {"include": [r"(?i)^(USA|US|United States( of America)?)$"]},
        "China": {"include": [r"(?i)^(China|CHN|CHA|China( and| ?\+) ?Taiwan)$"]},
        "India": {"include": [r"(?i)^(India|IND)$"]},
        "EU":    {"include": [r"(?i)^(EU|EU ?-?2[78]|EU-1[25]|EUR|European Union.*)$"],
                  "aggregate": True},   # sum sub-regions (e.g. GCAM EU-12 + EU-15)
    },
    "region_exclude": r"(?i)excl|rest of|other|world",
    # Explicit mapping where regex fails: {region_key: {model_regex: [region names]}}
    "region_overrides": {
        # Revision 2 (28 Sep 2026): MESSAGE uses the NGFS downscaled country data for USA, IND and EU27
        # (built by mef_prepare_inputs.py), so the R12 substitutes (North America, South Asia, Western +
        # Eastern Europe) are no longer needed. For the native IAM file, re-add them here, e.g.
        # "USA": {r"MESSAGE": ["MESSAGEix-GLOBIOM 2.0-R12|North America"]},
    },

    # ---- YOUR empirical anchors (same currency year and EF basis!) ----
    # p_now: estimated current effective carbon prices, US$2024/tCO2 (explicit prices;
    # TO CHECK, see Article/TODO.md): EU ETS ~80; China national ETS ~12; US and
    # India no national price. mef_emp: the TEA anchors each grid cell on its own
    # empirical build margin (MEF_cell(P) = floor + (MCI_cell - floor) H(P)/H(P_now)),
    # so only p_now is needed here; mef_emp is left unset (shape curves only).
    "anchors": {
        "USA":   {"mef_emp": None, "p_now": 0.0},
        "EU":    {"mef_emp": None, "p_now": 80.0},
        "China": {"mef_emp": None, "p_now": 12.0},
        "India": {"mef_emp": None, "p_now": 0.0},
    },
    "clamp_below_now": False,       # True: MEF(P <= P_now) = MEF_emp

    # ---- fitting ----
    # The TEA has no year dimension, so the time-drift variant ("one_drift") is not used.
    "specs": ["one", "two"],
    "spec_choice": "auto",          # "auto" (AIC, >2 improvement to add complexity) or a spec
    # Per-region override of the AIC choice. Coal-to-gas switching (the first stage of a two-stage
    # curve) is limited in China and India by gas availability and LNG cost (study decision 27 Sep 2026).
    "spec_force": {"China": "one", "India": "one"},
    "loss": "soft_l1",
    "f_scale": 0.15,
    "bootstrap": 300,
    "bootstrap_models": True,       # resample models as well as scenarios
    "seed": 42,
    "price_grid": (0, 500, 5),      # output grid (target currency / tCO2)
    "curve_years": (2030, 2040, 2050),  # used only if the time-drift spec is chosen
}


# =============================================================================
# LOADING
# =============================================================================
IAMC_COLS = ["model", "scenario", "region", "variable", "unit", "year", "value"]


def required_variables() -> list[str]:
    v = {CONFIG["price_var"], CONFIG["elec_total_var"], CONFIG["elec_emis_var"]}
    for suffix, *_ in CONFIG["techs"].values():
        for base in (SE, CAP, ADD):
            v.add(f"{base}|{suffix}")
            if suffix.endswith(WO):
                root = suffix[: -len(WO)]
                v.update({f"{base}|{root}", f"{base}|{root}|w/ CCS"})
    return sorted(v)


def load_pyam(db: str, models: str = "*", scenarios: str = "*") -> pd.DataFrame:
    try:
        import pyam
    except ImportError:
        sys.exit("pyam not installed: pip install pyam-iamc  (or use --source file)")
    log.info("Querying IIASA database '%s' (this can take a few minutes)...", db)
    idf = pyam.read_iiasa(db, model=models, scenario=scenarios,
                          variable=required_variables(), region="*")
    df = idf.data.copy()
    df.columns = [c.lower() for c in df.columns]
    return df[IAMC_COLS]


def load_file(path: str | Path) -> pd.DataFrame:
    path = Path(path)
    suf = path.suffix.lower()
    if suf in (".xlsx", ".xls"):
        raw = pd.read_excel(path)
    elif suf == ".parquet":
        raw = pd.read_parquet(path)
    else:
        raw = pd.read_csv(path)
    raw.columns = [str(c).strip() for c in raw.columns]
    low = {c.lower(): c for c in raw.columns}
    if "year" in low and "value" in low:                       # long format
        df = raw.rename(columns={v: k for k, v in low.items()})
    else:                                                      # IAMC wide format
        years = [c for c in raw.columns if re.fullmatch(r"\d{4}", c)]
        ids = [c for c in raw.columns
               if c.lower() in ("model", "scenario", "region", "variable", "unit")]
        df = raw.melt(id_vars=ids, value_vars=years, var_name="year", value_name="value")
        df.columns = [c.lower() for c in df.columns]
    df["year"] = df["year"].astype(int)
    df["value"] = pd.to_numeric(df["value"], errors="coerce")
    return df.dropna(subset=["value"])[IAMC_COLS]


# =============================================================================
# UNITS, REGIONS, PANEL CONSTRUCTION
# =============================================================================
_warned: set = set()


def _warn_once(msg, *args):
    key = msg % args
    if key not in _warned:
        _warned.add(key)
        log.warning(msg, *args)


def unit_factor(var: str, unit: str) -> float:
    u = str(unit or "").replace(" ", "").lower()
    if var == CONFIG["price_var"]:
        m = re.search(r"\$(\d{4})", u)
        yr = int(m.group(1)) if m else 2010
        if not m:
            _warn_once("Price unit '%s' has no currency year; assuming 2010", unit)
        if yr not in CONFIG["deflators"]:
            _warn_once("No deflator for currency year %s; using 1.0", yr)
        return CONFIG["deflators"].get(yr, 1.0)
    table = None
    if var.startswith("Secondary Energy"):
        table, default = {"ej/yr": 277.778, "ej": 277.778, "pj/yr": 0.277778,
                          "twh/yr": 1.0, "twh": 1.0}, 277.778
    elif var.startswith("Capacity Additions"):
        table, default = {"gw/yr": 1.0, "gw": 1.0, "mw/yr": 1e-3}, 1.0
    elif var.startswith("Capacity"):
        table, default = {"gw": 1.0, "mw": 1e-3}, 1.0
    elif var.startswith("Emissions"):
        table, default = {"mtco2/yr": 1.0, "mtco2": 1.0, "ktco2/yr": 1e-3,
                          "gtco2/yr": 1e3}, 1.0
    if table is None:
        return 1.0
    if u not in table:
        _warn_once("Unrecognised unit '%s' for %s; assuming factor %s", unit, var, default)
    return table.get(u, default)


def _seg(region: str) -> str:
    return str(region).split("|")[-1].strip()


def resolve_regions(dm: pd.DataFrame, model: str, key: str) -> list[str]:
    present = set(dm["region"].unique())
    for mpat, regs in CONFIG["region_overrides"].get(key, {}).items():
        if re.search(mpat, model):
            return [r for r in regs if r in present]
    spec = CONFIG["regions"][key]
    have = dm.groupby("region")["variable"].agg(set)
    ok = []
    for r, vars_ in have.items():
        s = _seg(r)
        if not any(re.search(p, s) for p in spec["include"]):
            continue
        if CONFIG["region_exclude"] and re.search(CONFIG["region_exclude"], s):
            continue
        if CONFIG["price_var"] in vars_ and any(v.startswith(SE + "|") for v in vars_):
            ok.append(r)
    if len(ok) > 1 and not spec.get("aggregate"):
        counts = dm[dm["region"].isin(ok)].groupby("region").size()
        pick = counts.idxmax()
        _warn_once("%s / %s: several regions match %s; using '%s' "
                   "(set region_overrides to choose)", model, key, ok, pick)
        ok = [pick]
    return ok


def _wide(sub: pd.DataFrame) -> pd.DataFrame:
    sub = sub.copy()
    pairs = sub[["variable", "unit"]].drop_duplicates()
    fac = {(v, u): unit_factor(v, u) for v, u in pairs.itertuples(index=False)}
    sub["value"] = sub["value"].to_numpy() * np.array(
        [fac[(v, u)] for v, u in zip(sub["variable"], sub["unit"])])
    return sub.pivot_table(index=["scenario", "region", "year"], columns="variable",
                           values="value", aggfunc="first")


def _aggregate(w: pd.DataFrame) -> pd.DataFrame:
    if w.index.get_level_values("region").nunique() == 1:
        return w.droplevel("region")
    price = CONFIG["price_var"]
    lv = ["scenario", "year"]
    tot = (w[CONFIG["elec_total_var"]] if CONFIG["elec_total_var"] in w
           else pd.Series(1.0, index=w.index))
    ext = w.drop(columns=[price], errors="ignore").groupby(level=lv).sum(min_count=1)
    if price in w:
        num = (w[price] * tot).groupby(level=lv).sum(min_count=1)
        den = tot.where(w[price].notna()).groupby(level=lv).sum(min_count=1)
        ext[price] = num / den
    return ext


def tech_series(w: pd.DataFrame, base: str, suffix: str):
    col = f"{base}|{suffix}"
    if col in w and w[col].notna().any():
        return w[col], "reported"
    if suffix.endswith(WO):
        root = f"{base}|{suffix[: -len(WO)]}"
        ccs = f"{root}|w/ CCS"
        if root in w and w[root].notna().any():
            s = w[root] - (w[ccs].fillna(0.0) if ccs in w else 0.0)
            return s.clip(lower=0.0), "derived"
    return None, "missing"


def _points_for(w: pd.DataFrame, model: str, key: str, regs: list[str]):
    price_var = CONFIG["price_var"]
    if price_var not in w:
        return pd.DataFrame(), None
    col = 2 if CONFIG["ef_basis"] == "lifecycle" else 1
    G, C, A, ef, efd, status = {}, {}, {}, {}, {}, {}
    for tk, spec in CONFIG["techs"].items():
        g, st = tech_series(w, SE, spec[0])
        status[tk] = st
        G[tk] = g if g is not None else pd.Series(0.0, index=w.index)
        C[tk], _ = tech_series(w, CAP, spec[0])
        A[tk], _ = tech_series(w, ADD, spec[0])
        ef[tk], efd[tk] = spec[col], spec[1]
    G = pd.DataFrame(G).fillna(0.0)
    efv, efdv = pd.Series(ef), pd.Series(efd)

    key_techs = [t for t in ("coal", "gas", "solar", "wind") if t in CONFIG["techs"]]
    cap_ok = all(C[t] is not None and A[t] is not None for t in key_techs)
    method = CONFIG["bm_method"]
    if method == "auto":
        method = "capacity" if cap_ok else "increment"
    if method == "capacity" and not cap_ok:
        _warn_once("%s / %s: capacity data incomplete, using increments", model, key)
        method = "increment"
    if method == "capacity":
        Cd = pd.DataFrame({t: (C[t] if C[t] is not None else 0.0) for t in G}, index=w.index)
        Ad = pd.DataFrame({t: (A[t] if A[t] is not None else 0.0) for t in G}, index=w.index)

    emis = w[CONFIG["elec_emis_var"]] if CONFIG["elec_emis_var"] in w else None
    tot_rep = w[CONFIG["elec_total_var"]] if CONFIG["elec_total_var"] in w else None
    y0, y1 = CONFIG["years"]
    rows = []
    for scen, g in G.groupby(level="scenario"):
        g = g.droplevel("scenario").sort_index()
        pr = w[price_var].xs(scen, level="scenario")
        em = emis.xs(scen, level="scenario") if emis is not None else None
        tr = tot_rep.xs(scen, level="scenario") if tot_rep is not None else None
        yrs = g.index.to_numpy()
        for a, b in zip(yrs[:-1], yrs[1:]):
            if b < y0 or b > y1:
                continue
            step = b - a
            gen_b = float(g.loc[b].sum())
            if gen_b <= 0:
                continue
            avg = float((g.loc[b] * efv).sum() / gen_b)
            if method == "average":
                new, bm, tot_new = None, avg, gen_b
            else:
                if method == "increment":
                    new = (g.loc[b] - g.loc[a]).clip(lower=0.0)
                else:
                    capb = Cd.loc[(scen, b)]
                    cf = (g.loc[b] / (capb * 8.76)).replace([np.inf, -np.inf], np.nan)
                    new = (Ad.loc[(scen, b)].fillna(0.0) * step
                           * cf.clip(0, 1).fillna(0.0) * 8.76).clip(lower=0.0)
                tot_new = float(new.sum())
                bm = float((new * efv).sum() / tot_new) if tot_new > 0 else np.nan
            valid = method == "average" or tot_new >= CONFIG["min_new_frac"] * gen_b
            pa, pb = pr.get(a, np.nan), pr.get(b, np.nan)
            if CONFIG["price_timing"] == "mid":
                vals = [p for p in (pa, pb) if pd.notna(p)]
                P = float(np.mean(vals)) if vals else np.nan
            else:
                P = pb
            recon = float((g.loc[b] * efdv).sum())
            rows.append(dict(
                model=model, scenario=scen, region_key=key, regions=";".join(regs),
                year=int(b), step=int(step), price=P, mef_bm=bm, mef_avg=avg,
                new_twh=tot_new, gen_twh=gen_b, method=method, valid=bool(valid),
                emis_ratio=(recon / em.get(b) if em is not None and pd.notna(em.get(b))
                            and em.get(b) > 0 else np.nan),
                coverage=(gen_b / tr.get(b) if tr is not None and pd.notna(tr.get(b))
                          and tr.get(b) > 0 else np.nan)))
    pts = pd.DataFrame(rows)
    diag = dict(model=model, region_key=key, regions=";".join(regs), method=method,
                n_points=len(pts),
                median_coverage=float(pts["coverage"].median()) if len(pts) else np.nan,
                median_emis_ratio=float(pts["emis_ratio"].median()) if len(pts) else np.nan,
                tech_status=json.dumps(status))
    return pts, diag


def build_points(df: pd.DataFrame):
    pts, diags = [], []
    for model, dm in df.groupby("model"):
        if CONFIG["model_exclude"] and re.search(CONFIG["model_exclude"], model):
            continue
        if CONFIG["scenario_exclude"]:
            dm = dm[~dm["scenario"].str.contains(CONFIG["scenario_exclude"], regex=True)]
        for key in CONFIG["regions"]:
            regs = resolve_regions(dm, model, key)
            if not regs:
                log.info("%s: no usable region for %s", model, key)
                continue
            p, d = _points_for(_aggregate(_wide(dm[dm["region"].isin(regs)])), model, key, regs)
            if d is not None and len(p):
                pts.append(p)
                diags.append(d)
    if not pts:
        sys.exit("No usable points. Run --list-regions and check variables/regions.")
    return pd.concat(pts, ignore_index=True), pd.DataFrame(diags)


# =============================================================================
# NORMALISATION
# =============================================================================
def mef_floor() -> float:
    return CONFIG["mef_min"][CONFIG["ef_basis"]]


def weighted_median(x, w) -> float:
    x, w = np.asarray(x, float), np.asarray(w, float)
    m = np.isfinite(x) & np.isfinite(w) & (w > 0)
    if not m.any():
        return np.nan
    o = np.argsort(x[m])
    xs, c = x[m][o], np.cumsum(w[m][o])
    return float(xs[np.searchsorted(c, 0.5 * c[-1])])


def normalise(pts: pd.DataFrame) -> pd.DataFrame:
    floor = mef_floor()
    pts = pts[pts["valid"] & pts["price"].notna() & pts["mef_bm"].notna()].copy()
    pts["w_raw"] = np.power(pts["new_twh"].clip(lower=1e-6), CONFIG["weight_power"])
    out = []
    for (m, r), g in pts.groupby(["model", "region_key"]):
        ref = g[(g["price"] <= CONFIG["ref_price_max"]) & (g["year"] <= CONFIG["ref_year_max"])]
        if len(ref) < 2:
            ref = g[g["price"] <= g["price"].quantile(0.2)]
            _warn_once("%s / %s: few low-price points; reference = lowest price quintile", m, r)
        mef_ref = weighted_median(ref["mef_bm"], ref["w_raw"])
        if not np.isfinite(mef_ref) or mef_ref - floor < 0.05:
            _warn_once("%s / %s: reference build margin %.3f too close to floor; skipped",
                       m, r, mef_ref)
            continue
        g = g.copy()
        g["mef_ref"] = mef_ref
        g["p_ref"] = weighted_median(ref["price"], ref["w_raw"])
        g["t_ref"] = weighted_median(ref["year"], ref["w_raw"])
        g["y"] = ((g["mef_bm"] - floor) / (mef_ref - floor)).clip(-0.2, 2.0)
        out.append(g)
    if not out:
        sys.exit("Normalisation left no data.")
    return pd.concat(out, ignore_index=True)


# =============================================================================
# CURVE FUNCTIONS AND FITTING
# =============================================================================
NPAR = {"one": 2, "one_drift": 3, "two": 5}
BOUNDS = {
    "one":       ([np.log(2), np.log(0.5)], [np.log(3000), np.log(8)]),
    "one_drift": ([np.log(2), np.log(0.5), -0.1], [np.log(3000), np.log(8), 0.1]),
    "two":       ([np.log(2), np.log(0.5), -3.0, np.log(0.5), -6.0],
                  [np.log(1500), np.log(10), 5.0, np.log(8), 6.0]),
}


def hill(P, P50, k):
    P = np.clip(np.asarray(P, float), 0.0, None)
    return 1.0 / (1.0 + np.power(P / P50, k))


def unpack(spec: str, x) -> dict:
    if spec == "one":
        return {"P50": float(np.exp(x[0])), "k": float(np.exp(x[1])), "lam": 0.0}
    if spec == "one_drift":
        return {"P50": float(np.exp(x[0])), "k": float(np.exp(x[1])), "lam": float(x[2])}
    p1 = float(np.exp(x[0]))
    return {"P50_1": p1, "k1": float(np.exp(x[1])), "P50_2": p1 * (1 + float(np.exp(x[2]))),
            "k2": float(np.exp(x[3])), "s": float(1 / (1 + np.exp(-x[4])))}


def H(P, t, th: dict):
    """Fraction of the (MEF_max - MEF_min) gap remaining at price P, year t."""
    if "s" in th:
        return (th["s"] * hill(P, th["P50_2"], th["k2"])
                + (1 - th["s"]) * hill(P, th["P50_1"], th["k1"]))
    P50 = th["P50"] * np.exp(-th.get("lam", 0.0) * (np.asarray(t, float) - CONFIG["t_ref"]))
    return hill(P, P50, th["k"])


def make_data(p: pd.DataFrame, group_col: str = "model") -> SimpleNamespace:
    w = p["w_raw"].to_numpy(float)
    grp = p[group_col].to_numpy()
    wn = np.empty_like(w)
    for gk in np.unique(grp):          # equal total weight per model (or boot group)
        m = grp == gk
        wn[m] = w[m] / w[m].sum()
    wn /= wn.mean()
    return SimpleNamespace(P=p["price"].to_numpy(float), t=p["year"].to_numpy(float),
                           y=p["y"].to_numpy(float), w=wn,
                           p_ref=p["p_ref"].to_numpy(float), t_ref=p["t_ref"].to_numpy(float))


def _resid(x, spec, d):
    th = unpack(spec, x)
    yhat = H(d.P, d.t, th) / np.maximum(H(d.p_ref, d.t_ref, th), 1e-9)
    return np.sqrt(d.w) * (d.y - yhat)


def _starts(spec):
    if spec == "one":
        return [[np.log(p), np.log(k)] for p in (25, 60, 120, 250) for k in (1.2, 2, 3.5)]
    if spec == "one_drift":
        return [[np.log(p), np.log(k), 0.0] for p in (25, 60, 120, 250) for k in (1.2, 2, 3.5)]
    return [[np.log(p1), np.log(k1), np.log(r - 1), np.log(k2), np.log(s / (1 - s))]
            for p1 in (20, 45) for r in (2.5, 5) for k1 in (2.5, 4)
            for k2 in (1.5, 3) for s in (0.35, 0.6)]


def fit(spec: str, d: SimpleNamespace, x0s=None):
    lo, hi = map(np.asarray, BOUNDS[spec])
    best = None
    for x0 in (x0s if x0s is not None else _starts(spec)):
        x0 = np.clip(np.asarray(x0, float), lo + 1e-6, hi - 1e-6)
        try:
            r = least_squares(_resid, x0, bounds=(lo, hi), args=(spec, d),
                              loss=CONFIG["loss"], f_scale=CONFIG["f_scale"], max_nfev=3000)
        except (ValueError, FloatingPointError):
            continue
        if best is None or r.cost < best.cost:
            best = r
    if best is None:
        return None
    rr = _resid(best.x, spec, d)
    sw = d.w.sum()
    mse = float((rr ** 2).sum() / sw)
    n_eff = float(sw ** 2 / (d.w ** 2).sum())
    return {"spec": spec, "x": best.x, "params": unpack(spec, best.x), "mse": mse,
            "rmse": mse ** 0.5, "n": int(len(d.y)), "n_eff": n_eff,
            "aic": n_eff * np.log(max(mse, 1e-12)) + 2 * NPAR[spec]}


def choose_spec(pooled: dict, region: str | None = None) -> str:
    forced = CONFIG.get("spec_force", {}).get(region)
    if forced and forced in pooled:
        return forced
    if CONFIG["spec_choice"] != "auto":
        return CONFIG["spec_choice"]
    if "one" not in pooled:
        return min(pooled, key=lambda s: pooled[s]["aic"])
    better = [s for s in pooled if s != "one" and pooled[s]["aic"] < pooled["one"]["aic"] - 2]
    return min(better, key=lambda s: pooled[s]["aic"]) if better else "one"


def bootstrap(pr: pd.DataFrame, spec: str, x0, rng) -> list[dict]:
    models = pr["model"].unique()
    by_m = {m: pr[pr["model"] == m] for m in models}
    scen = {m: g["scenario"].unique() for m, g in by_m.items()}
    draws = []
    for _ in range(CONFIG["bootstrap"]):
        ms = (rng.choice(models, len(models), replace=True)
              if CONFIG["bootstrap_models"] and len(models) > 1 else models)
        parts = []
        for j, m in enumerate(ms):
            g = by_m[m]
            pick = rng.choice(scen[m], len(scen[m]), replace=True)
            parts.append(pd.concat([g[g["scenario"] == s] for s in pick]).assign(_grp=j))
        f = fit(spec, make_data(pd.concat(parts, ignore_index=True), "_grp"), x0s=[x0])
        if f:
            draws.append(f["params"])
    return draws


# =============================================================================
# ANCHORED CURVES (also importable by the TEA)
# =============================================================================
def mef_curve(P, params: dict, mef_emp: float, p_now: float, mef_min: float,
              year=None, t_now: int | None = None, clamp_below_now: bool = False):
    """MEF(P) [tCO2/MWh] through the empirical anchor (p_now, mef_emp).
    Returns (mef, mef_max). `year` only matters for the time-drift spec."""
    t_now = CONFIG["t_now"] if t_now is None else t_now
    year = t_now if year is None else year
    h_now = float(H(p_now, t_now, params))
    mef_max = mef_min + (mef_emp - mef_min) / max(h_now, 1e-9)
    mef = mef_min + (mef_max - mef_min) * H(P, year, params)
    if clamp_below_now:
        mef = np.where(np.asarray(P, float) <= p_now, mef_emp, mef)
    return mef, mef_max


def mef_lookup(table: pd.DataFrame, region: str, P, year=None, stat: str = "mef_p50"):
    """Interpolate a column of mef_curves.csv for use in the TEA."""
    t = table[table["region"] == region]
    if (t["year"].astype(str) != "all").any():
        yrs = t.loc[t["year"].astype(str) != "all", "year"].astype(int)
        target = yrs.iloc[(yrs - (year or yrs.min())).abs().argmin()]
        t = t[t["year"].astype(str) == str(target)]
    return np.interp(P, t["price"].to_numpy(float), t[stat].to_numpy(float))


# =============================================================================
# DRIVER
# =============================================================================
def _param_row(region, scope, f, chosen=False):
    return {"region": region, "scope": scope, "spec": f["spec"], "chosen": chosen,
            **f["params"], "rmse": f["rmse"], "aic": f["aic"], "n": f["n"], "n_eff": f["n_eff"]}


def _plot(region, pr, fits_model, pooled, chosen, draws, curves, out):
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        return
    Pmax = float(min(max(pr["price"].quantile(0.98), 150), CONFIG["price_grid"][1]))
    P = np.linspace(0, Pmax, 300)
    t_plot = CONFIG["t_ref"]
    ncol = 2 if curves is not None else 1
    fig, ax = plt.subplots(1, ncol, figsize=(6.5 * ncol, 4.6), squeeze=False)
    a = ax[0, 0]
    colours = plt.rcParams["axes.prop_cycle"].by_key()["color"]
    for i, (m, g) in enumerate(pr.groupby("model")):
        c = colours[i % len(colours)]
        a.scatter(g["price"], g["y"], s=10 + 40 * g["w_raw"] / pr["w_raw"].max(),
                  alpha=0.5, color=c, label=m)
        f = fits_model.get((m, chosen))
        if f:
            pref = g["p_ref"].iloc[0]
            a.plot(P, H(P, t_plot, f["params"]) / H(pref, t_plot, f["params"]), color=c, lw=1)
    pref = float(pr["p_ref"].median())
    th = pooled[chosen]["params"]
    band = np.array([H(P, t_plot, d) / H(pref, t_plot, d) for d in draws]) if draws else None
    if band is not None and len(band):
        a.fill_between(P, *np.nanpercentile(band, [5, 95], axis=0), color="k", alpha=0.12,
                       label="pooled 5-95%")
    a.plot(P, H(P, t_plot, th) / H(pref, t_plot, th), "k", lw=2.5, label=f"pooled ({chosen})")
    a.set(xlabel="Carbon price (target currency/tCO2)",
          ylabel=f"{CONFIG['bm_method'].capitalize()} intensity / low-price reference",
          title=f"{region}: normalised {CONFIG['bm_method']} intensity", ylim=(-0.1, 1.4))
    a.legend(fontsize=7)
    if curves is not None:
        b = ax[0, 1]
        for yr, c in curves.groupby("year"):
            b.plot(c["price"], c["mef_central"], lw=2, label=f"central ({yr})")
            b.fill_between(c["price"], c["mef_p05"], c["mef_p95"], alpha=0.15)
        an = CONFIG["anchors"][region]
        b.scatter([an["p_now"]], [an["mef_emp"]], color="r", zorder=5, label="empirical anchor")
        b.set(xlabel="Carbon price (target currency/tCO2)", ylabel="MEF (tCO2/MWh)",
              title=f"{region}: anchored MEF(P)", xlim=(0, Pmax), ylim=(0, None))
        b.legend(fontsize=7)
    fig.tight_layout()
    fig.savefig(out / f"fit_{region}.png", dpi=150)
    plt.close(fig)


def run(df: pd.DataFrame, out: Path) -> dict:
    out.mkdir(parents=True, exist_ok=True)
    raw, diag = build_points(df)
    raw.to_csv(out / "points_raw.csv", index=False)
    diag.to_csv(out / "diagnostics.csv", index=False)
    pts = normalise(raw)
    pts.to_csv(out / "points_normalised.csv", index=False)

    floor = mef_floor()
    rng = np.random.default_rng(CONFIG["seed"])
    lo, hi, st = CONFIG["price_grid"]
    Pg = np.arange(lo, hi + st, st, dtype=float)
    prow, drow, srow, crow, results = [], [], [], [], {}

    for region, pr in pts.groupby("region_key"):
        log.info("Fitting %s: %d points, %d models", region, len(pr), pr["model"].nunique())
        fits_model = {}
        for m, pm in pr.groupby("model"):
            d = make_data(pm)
            for spec in CONFIG["specs"]:
                f = fit(spec, d)
                if f:
                    fits_model[(m, spec)] = f
                    prow.append(_param_row(region, m, f))
        d = make_data(pr)
        pooled = {s: f for s in CONFIG["specs"] if (f := fit(s, d))}
        chosen = choose_spec(pooled, region)
        prow += [_param_row(region, "POOLED", f, s == chosen) for s, f in pooled.items()]
        draws = bootstrap(pr, chosen, pooled[chosen]["x"], rng)
        drow += [{"region": region, "draw": i, "spec": chosen, **th} for i, th in enumerate(draws)]

        years = CONFIG["curve_years"] if chosen == "one_drift" else ("all",)
        th = pooled[chosen]["params"]
        for yr in years:
            t = CONFIG["t_ref"] if yr == "all" else yr
            hb = np.array([H(Pg, t, dd) for dd in draws])
            q = np.nanpercentile(hb, [5, 50, 95], axis=0)
            srow += [{"region": region, "year": yr, "price": p, "H_central": h,
                      "H_p05": a, "H_p50": b, "H_p95": c}
                     for p, h, a, b, c in zip(Pg, H(Pg, t, th), *q)]

        an = CONFIG["anchors"].get(region, {})
        curves = None
        if an.get("mef_emp") is not None and an.get("p_now") is not None:
            kw = dict(mef_emp=an["mef_emp"], p_now=an["p_now"], mef_min=floor,
                      clamp_below_now=CONFIG["clamp_below_now"])
            rows = []
            for yr in years:
                t = None if yr == "all" else yr
                cen, mmax = mef_curve(Pg, th, year=t, **kw)
                mb = np.array([mef_curve(Pg, dd, year=t, **kw)[0] for dd in draws])
                q = np.nanpercentile(mb, [5, 50, 95], axis=0)
                rows += [{"region": region, "year": yr, "price": p, "mef_central": c,
                          "mef_p05": a, "mef_p50": b, "mef_p95": e, "mef_max_central": mmax}
                         for p, c, a, b, e in zip(Pg, cen, *q)]
                if mmax > 1.15:
                    log.warning("%s: implied MEF_max %.2f exceeds coal intensity -- check "
                                "anchor p_now / mef_emp", region, mmax)
            curves = pd.DataFrame(rows)
            crow.append(curves)
        else:
            log.info("%s: no anchor set -> shape curve only (fill CONFIG['anchors'])", region)
        _plot(region, pr, fits_model, pooled, chosen, draws, curves, out)
        results[region] = {"chosen": chosen, "pooled": pooled[chosen]["params"],
                           "n_draws": len(draws)}

    pd.DataFrame(prow).to_csv(out / "fit_params.csv", index=False)
    pd.DataFrame(drow).to_csv(out / "bootstrap_draws.csv", index=False)
    pd.DataFrame(srow).to_csv(out / "shape_curves.csv", index=False)
    if crow:
        pd.concat(crow, ignore_index=True).to_csv(out / "mef_curves.csv", index=False)
    (out / "run_config.json").write_text(json.dumps(CONFIG, indent=2, default=str))
    for r, v in results.items():
        log.info("%s -> %s %s (%d draws)", r, v["chosen"],
                 {k: round(x, 3) for k, x in v["pooled"].items()}, v["n_draws"])
    return results


def list_regions(df: pd.DataFrame):
    for model, dm in df.groupby("model"):
        have = dm.groupby("region")["variable"].agg(set)
        usable = sorted(r for r, v in have.items()
                        if CONFIG["price_var"] in v and any(x.startswith(SE + "|") for x in v))
        flag = "  [EXCLUDED by model_exclude]" if (
            CONFIG["model_exclude"] and re.search(CONFIG["model_exclude"], model)) else ""
        print(f"\n== {model}{flag}\n   scenarios: {', '.join(sorted(dm['scenario'].unique()))}")
        for key in CONFIG["regions"]:
            print(f"   {key:6s} -> {resolve_regions(dm, model, key) or 'NO MATCH (add region_overrides)'}")
        print(f"   regions with price + generation ({len(usable)}): {', '.join(usable)}")


# =============================================================================
# SELF-TEST ON SYNTHETIC NGFS-LIKE DATA
# =============================================================================
def synthetic_ngfs(seed: int = 1):
    """Three 'models' with known Hill parameters. Model B reports prices in
    US$2010 (tests deflation); Model C reports only total coal (tests the
    derived 'w/o CCS' path). No retirements, so increments = new builds."""
    rng = np.random.default_rng(seed)
    truth = {"Model A": (70.0, 2.0), "Model B": (95.0, 2.2), "Model C": (120.0, 1.8)}
    years = list(range(2020, 2065, 5))
    paths = {
        "Current Policies":   lambda t: 2.0,
        "NDCs":               lambda t: 8.0 * np.exp(0.03 * (t - 2020)),
        "Fragmented World":   lambda t: 10.0 * np.exp(0.05 * (t - 2020)),
        "Delayed Transition": lambda t: 3.0 if t <= 2030 else 3.0 * np.exp(0.14 * (t - 2030)),
        "Below 2C":           lambda t: 25.0 * np.exp(0.045 * (t - 2020)),
        "Net Zero 2050":      lambda t: 50.0 * np.exp(0.06 * (t - 2020)),
    }
    rows = []
    for model, (P50, k) in truth.items():
        usd2010 = model == "Model B"
        for scen, path in paths.items():
            coal, wind = 3000.0, 600.0
            for i, t in enumerate(years):
                if i:
                    pm = 0.5 * (path(t) + path(years[i - 1]))
                    frac = min(0.9 * hill(pm, P50, k) * np.exp(rng.normal(0, 0.08)), 1.0)
                    new = (coal + wind) * (1.025 ** 5 - 1)
                    coal, wind = coal + new * frac, wind + new * (1 - frac)
                p = path(t) / (1.44 if usd2010 else 1.0)
                punit = "US$2010/t CO2" if usd2010 else "US$2024/t CO2"
                coal_var = f"{SE}|Coal" if model == "Model C" else f"{SE}|Coal|w/o CCS"
                for var, val, unit in [(CONFIG["price_var"], p, punit),
                                       (coal_var, coal / 277.778, "EJ/yr"),
                                       (f"{SE}|Wind", wind / 277.778, "EJ/yr"),
                                       (SE, (coal + wind) / 277.778, "EJ/yr"),
                                       (CONFIG["elec_emis_var"], coal * 0.95, "Mt CO2/yr")]:
                    rows.append((model, scen, "USA", var, unit, t, val))
    return pd.DataFrame(rows, columns=IAMC_COLS), truth


def selftest(out: Path) -> bool:
    CONFIG.update(ef_basis="direct", bootstrap=80, bm_method="increment", spec_force={},
                  anchors={"USA": {"mef_emp": 0.60, "p_now": 5.0}})
    CONFIG["regions"] = {"USA": CONFIG["regions"]["USA"]}
    df, truth = synthetic_ngfs()
    run(df, out)
    fp = pd.read_csv(out / "fit_params.csv")
    ok = True
    print("\nSELF-TEST: recovery of known single-stage parameters")
    for m, (p50, k) in truth.items():
        r = fp[(fp["scope"] == m) & (fp["spec"] == "one")].iloc[0]
        e1, e2 = r["P50"] / p50 - 1, r["k"] / k - 1
        good = abs(e1) < 0.15 and abs(e2) < 0.25
        ok &= good
        print(f"  {m}: P50 {r['P50']:6.1f} (true {p50:5.1f}, {e1:+.1%})  "
              f"k {r['k']:.2f} (true {k:.2f}, {e2:+.1%})  {'ok' if good else 'FAIL'}")
    pooled = fp[(fp["scope"] == "POOLED") & fp["chosen"]].iloc[0]
    in_range = 70 <= pooled["P50"] <= 120
    ok &= in_range and pooled["spec"] == "one"
    print(f"  pooled: spec={pooled['spec']} P50 {pooled['P50']:.1f} k {pooled['k']:.2f} "
          f"({'ok' if in_range else 'FAIL'}: expected between model values, spec 'one')")
    cur = pd.read_csv(out / "mef_curves.csv")
    at = float(cur.loc[np.isclose(cur["price"], 5.0), "mef_central"].iloc[0])
    ok &= abs(at - 0.60) < 1e-6
    print(f"  anchor: MEF(5) = {at:.4f} (expected 0.6000)")
    print("SELF-TEST PASSED" if ok else "SELF-TEST FAILED")
    return ok


def main():
    ap = argparse.ArgumentParser(description="Fit MEF(P) curves to NGFS scenarios.")
    ap.add_argument("--source", choices=["pyam", "file"])
    ap.add_argument("--db", default="ngfs_phase_5", help="IIASA database name (pyam)")
    ap.add_argument("--file", help="IAMC CSV/XLSX/parquet export (source=file)")
    ap.add_argument("--cache", help="CSV to save the raw pull to / reuse if present")
    ap.add_argument("--models", default="*")
    ap.add_argument("--scenarios", default="*")
    ap.add_argument("--out", default="mef_results")
    ap.add_argument("--list-regions", action="store_true")
    ap.add_argument("--selftest", action="store_true")
    ap.add_argument("--bm-method", choices=["auto", "capacity", "increment", "average"])
    ap.add_argument("--model-exclude", help="regex of models to exclude (replaces CONFIG default)")
    ap.add_argument("--bootstrap", type=int, help="number of bootstrap draws (replaces CONFIG default)")
    ap.add_argument("-v", "--verbose", action="store_true")
    a = ap.parse_args()
    logging.basicConfig(level=logging.DEBUG if a.verbose else logging.INFO,
                        format="%(levelname)s  %(message)s")

    if a.bm_method:
        CONFIG["bm_method"] = a.bm_method
    if a.model_exclude:
        CONFIG["model_exclude"] = a.model_exclude
    if a.bootstrap:
        CONFIG["bootstrap"] = a.bootstrap
    if a.selftest:
        sys.exit(0 if selftest(Path(a.out)) else 1)

    if a.cache and Path(a.cache).exists():
        df = load_file(a.cache)
        log.info("Loaded cache %s (%d rows)", a.cache, len(df))
    elif a.source == "pyam":
        df = load_pyam(a.db, a.models, a.scenarios)
        if a.cache:
            df.to_csv(a.cache, index=False)
    elif a.source == "file" and a.file:
        df = load_file(a.file)
    else:
        ap.error("give --source pyam, --source file --file PATH, or an existing --cache")

    # Keep only the variables the fit uses (full IAMC exports carry hundreds of others)
    df = df[df["variable"].isin(required_variables())]
    if a.list_regions:
        list_regions(df)
        return
    run(df, Path(a.out))


if __name__ == "__main__":
    main()
