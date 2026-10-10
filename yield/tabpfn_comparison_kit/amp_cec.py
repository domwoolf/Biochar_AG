"""Fair comparison on the subset with both pH and CEC (what the global maps will supply)."""
import numpy as np, pandas as pd
from amplitude import prep, grouped_cv, m_null, design, TAU2
from sklearn.ensemble import HistGradientBoostingRegressor, RandomForestRegressor
import metareg

def m_meta2(Xtr, ytr, wtr, Xte, etr, ete):
    f = lambda e: pd.DataFrame({"acid": np.clip(6.5 - e.pH, 0, None), "alk": np.clip(e.pH - 7.5, 0, None), "cec": np.log(e.CEC)})
    tr = etr.assign(a=ytr, va=np.clip(1 / wtr - TAU2, 1e-5, None), **f(etr))
    b = metareg.fit(tr, "a", "va", ["acid", "alk", "cec"])["table"].est.values
    return np.column_stack([np.ones(len(ete)), f(ete).values]) @ b

def m_gbm2(Xtr, ytr, wtr, Xte, etr, ete):
    c = ["pH", "CEC"]
    return HistGradientBoostingRegressor(max_depth=2, learning_rate=0.05, max_iter=200, min_samples_leaf=30,
            monotonic_cst=[-1, -1], random_state=0).fit(Xtr[c], ytr, sample_weight=wtr).predict(Xte[c])

def m_rf2(Xtr, ytr, wtr, Xte, etr, ete):
    c = ["pH", "CEC"]
    return RandomForestRegressor(400, min_samples_leaf=15, random_state=0, n_jobs=-1).fit(Xtr[c], ytr, sample_weight=wtr).predict(Xte[c])

def m_rfall(Xtr, ytr, wtr, Xte, etr, ete):
    med = Xtr.median()
    return RandomForestRegressor(400, min_samples_leaf=10, max_features=0.5, random_state=0, n_jobs=-1).fit(
        Xtr.fillna(med), ytr, sample_weight=wtr).predict(Xte.fillna(med))

if __name__ == "__main__":
    e = prep(0.5).dropna(subset=["pH", "CEC"]).reset_index(drop=True)
    print(f"subset with pH and CEC: {len(e)} obs, {e.study.nunique()} studies")
    for name, fn in [("null", m_null), ("meta-reg: acid + alk + log CEC", m_meta2), ("boosted, pH+CEC monotone", m_gbm2),
                     ("RF, pH+CEC", m_rf2), ("RF, all covariates", m_rfall)]:
        r2w, r2s, _ = grouped_cv(e, fn, nrep=5, k=10)
        print(f"  {name:32s} obs R2={r2w:6.3f}  study-level R2={r2s:6.3f}")
    # relative contribution of pH vs CEC terms across the observed interquartile range
    r = metareg.fit(e.assign(acid=np.clip(6.5 - e.pH, 0, None), alk=np.clip(e.pH - 7.5, 0, None), cec=np.log(e.CEC)), "a", "va", ["acid", "alk", "cec"])
    b = r["table"].est
    q = e[["pH", "CEC"]].quantile([0.1, 0.9])
    print(f"  10th-90th pct: pH {q.pH.values}, CEC {q.CEC.values}")
    print(f"  change in response (lnRR at 10 Mg/ha) across that range: pH(acid) term {b['acid']*(6.5-max(q.pH.iloc[0],0)):.3f} "
          f"(from pH {q.pH.iloc[0]:.1f} to 6.5);  CEC term {abs(b['cec'])*np.log(q.CEC.iloc[1]/q.CEC.iloc[0]):.3f}")
