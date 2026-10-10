"""Shared fitting tools: within-experiment dose-response with per-experiment amplitudes.

Model: y_ij = A_j * g(D_ij; theta) [+ c * D_ij], weighted least squares, A_j profiled out in
closed form, theta by 1-D / Nelder-Mead search. Uncertainty by bootstrap over studies.
"""
import numpy as np
import pandas as pd
from scipy.optimize import minimize_scalar, minimize

EXP_COLS = ["src", "study", "crop", "yieldtype", "pH", "treat", "Nrate", "years", "field",
            "lat", "lon", "SOC", "feed", "HTT", "bcC"]


def experiments(d, y="lnRR", cols=EXP_COLS, min_doses=2):
    d = d.copy()
    cols = [c for c in cols if c in d.columns]
    d["exp"] = d[cols].astype(object).fillna("NA").astype(str).agg("|".join, axis=1)
    agg = dict(y=(y, "mean"), v=("v", "mean"), nrow=(y, "size"))
    keep = [c for c in ["src", "study", "pH", "CEC", "SOC", "clay", "field", "years", "crop", "lat", "lon",
                        "precip", "temp", "Nrate", "feed", "ck", "treat", "flooded"] if c in d.columns]
    for c in keep:
        agg[c] = (c, "first")
    m = d.groupby(["exp", "dose"], as_index=False, dropna=False).agg(**agg)
    m["v"] = m.v / m.nrow                     # mean of replicate rows at the same dose
    nd = m.groupby("exp").dose.transform("nunique")
    return m[nd >= min_doses].reset_index(drop=True)


def weights(m, kind, tau2=None):
    if kind == "none":
        return np.ones(len(m))
    if kind == "study":                        # every study carries equal total weight
        return 1.0 / m.groupby("study").y.transform("size").values
    if kind == "ivw":
        v = m.v.copy()
        v = v.where(v > 0)
        v = v.fillna(np.nanmedian(v)) if v.notna().any() else pd.Series(0.0, index=m.index)
        return 1.0 / (v.values + tau2)
    raise ValueError(kind)


def _sse(m, g, w, c=0.0):
    codes, _ = pd.factorize(m.exp)
    r = m.y.values - c * m.dose.values
    A = np.bincount(codes, w * r * g) / np.bincount(codes, w * g * g)
    res = r - A[codes] * g
    return (w * res ** 2).sum(), A, res


def g_sat(D, Ds):
    return 1 - np.exp(-D / Ds)


def fit_sat(m, w):
    f = lambda ls: _sse(m, g_sat(m.dose.values, np.exp(ls)), w)[0]
    r = minimize_scalar(f, bounds=(np.log(0.2), np.log(2000)), method="bounded")
    return np.exp(r.x), r.fun


def fit_power(m, w):
    f = lambda p: _sse(m, m.dose.values ** p, w)[0]
    r = minimize_scalar(f, bounds=(0.0, 1.5), method="bounded")
    return r.x, r.fun


def fit_two(m, w):
    def f(par):
        return _sse(m, g_sat(m.dose.values, np.exp(par[0])), w, c=par[1])[0]
    best = min((minimize(f, [np.log(s), 0.0], method="Nelder-Mead", options=dict(xatol=1e-4, fatol=1e-8))
                for s in [1, 3, 10]), key=lambda r: r.fun)
    return np.exp(best.x[0]), best.x[1], best.fun


def tau2_estimate(m):
    """Between-observation (within-experiment) heterogeneity beyond sampling error."""
    Ds, _ = fit_sat(m, np.ones(len(m)))
    _, A, res = _sse(m, g_sat(m.dose.values, Ds), np.ones(len(m)))
    dof = len(m) - m.exp.nunique() - 1
    return max(0.0, (res ** 2).sum() / max(dof, 1) - np.nanmedian(m.v))


def boot_studies(m, fn, n=300, seed=1):
    r = np.random.default_rng(seed)
    st = m.study.unique()
    by = {s: g for s, g in m.groupby("study")}
    out = []
    for _ in range(n):
        pick = r.choice(st, len(st))
        b = pd.concat([by[s].assign(exp=by[s].exp + f"#{i}", study=by[s].study + f"#{i}") for i, s in enumerate(pick)],
                      ignore_index=True)
        out.append(fn(b))
    return np.array(out)


def amplitudes(m, Ds, w=None, c=0.0):
    w = np.ones(len(m)) if w is None else w
    g = g_sat(m.dose.values, Ds)
    _, A, _ = _sse(m, g, w, c)
    codes, uniq = pd.factorize(m.exp)
    # precision of A_j: sum w g^2 (relative); also count of doses
    info = m.groupby("exp", sort=False).first()
    info = info.loc[uniq]
    info["A"] = A
    info["A_wt"] = np.bincount(codes, w * g * g)
    info["ndose"] = np.bincount(codes)
    info["maxdose"] = m.groupby("exp", sort=False).dose.max().loc[uniq].values
    return info
