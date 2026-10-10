"""Multilevel random-effects meta-regression (equivalent to metafor::rma.mv(yi, vi, mods, random=~1|study/obs), ML).

y_i = x_i' beta + u_study + e_i,  u ~ N(0, s2_study),  e_i ~ N(0, v_i + s2_obs).
beta by GLS given variance components; variance components by ML (profile likelihood).
"""
import numpy as np
import pandas as pd
from scipy.optimize import minimize


def _blocks(study):
    codes, _ = pd.factorize(study)
    return [np.where(codes == k)[0] for k in range(codes.max() + 1)]


def _gls(y, X, v, blocks, s2s, s2o):
    XtWX = np.zeros((X.shape[1], X.shape[1])); XtWy = np.zeros(X.shape[1]); logdet = 0.0
    Winv_list = []
    for idx in blocks:
        V = np.diag(v[idx] + s2o) + s2s
        Vi = np.linalg.inv(V)
        _, ld = np.linalg.slogdet(V)
        logdet += ld
        Xi = X[idx]
        XtWX += Xi.T @ Vi @ Xi; XtWy += Xi.T @ Vi @ y[idx]
        Winv_list.append(Vi)
    beta = np.linalg.solve(XtWX, XtWy)
    q = 0.0
    for idx, Vi in zip(blocks, Winv_list):
        r = y[idx] - X[idx] @ beta
        q += r @ Vi @ r
    return beta, np.linalg.inv(XtWX), logdet, q


def fit(df, y, v, X_cols, study="study", intercept=True):
    d = df.dropna(subset=[y, v] + X_cols).reset_index(drop=True)
    yv = d[y].values.astype(float); vv = d[v].values.astype(float)
    X = d[X_cols].values.astype(float)
    if intercept:
        X = np.column_stack([np.ones(len(d)), X]); names = ["(intercept)"] + X_cols
    else:
        names = list(X_cols)
    blocks = _blocks(d[study])

    def nll(p):
        s2s, s2o = np.exp(p)
        _, _, ld, q = _gls(yv, X, vv, blocks, s2s, s2o)
        return 0.5 * (ld + q)
    best = min((minimize(nll, np.log([a, b]), method="Nelder-Mead") for a in [1e-3, 1e-2] for b in [1e-3, 1e-2]),
               key=lambda r: r.fun)
    s2s, s2o = np.exp(best.x)
    beta, cov, _, _ = _gls(yv, X, vv, blocks, s2s, s2o)
    se = np.sqrt(np.diag(cov))
    tab = pd.DataFrame({"est": beta, "se": se, "z": beta / se}, index=names)
    k = X.shape[1] + 2
    return dict(table=tab, s2_study=s2s, s2_obs=s2o, loglik=-best.fun - 0.5 * len(d) * np.log(2 * np.pi),
                aic=2 * best.fun + len(d) * np.log(2 * np.pi) + 2 * k, n=len(d), studies=len(blocks), cov=cov,
                names=names)


def show(res, label=""):
    print(f"\n{label}  n={res['n']} studies={res['studies']}  s2_study={res['s2_study']:.4f} s2_obs={res['s2_obs']:.4f}  AIC={res['aic']:.1f}")
    print(res["table"].round(4).to_string())
