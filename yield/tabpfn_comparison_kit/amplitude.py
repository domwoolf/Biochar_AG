"""Stage 2: how does the size of the yield response vary with soil, climate and crop?

Every field observation is normalised to a reference dose with the within-experiment shape
(power law, exponent p):  a_i = lnRR_i / (D_i / 10)^p  = implied response at 10 Mg biochar/ha.
Models compared with study-grouped cross-validation (whole studies held out), precision-weighted.
"""
import numpy as np
import pandas as pd
from sklearn.model_selection import GroupKFold
from sklearn.ensemble import HistGradientBoostingRegressor, RandomForestRegressor
from sklearn.inspection import permutation_importance
import metareg
import warnings
warnings.filterwarnings("ignore")

P = 0.50          # within-experiment power exponent (all field, ivw); sensitivity below
TAU2 = 0.012
d = pd.read_csv("harmonised2.csv")
d = d[(d.field == 1)].copy()
# merge the two romanisations of the same author/study found in the N2O check
d["study"] = d.study.replace({"张 斌 2012": "Zhang Bin 2012"})

cer = "maize|corn|wheat|rice|barley|sorghum|millet|oat|rye|triticale|cereal"
leg = "soy|bean|pea|peanut|groundnut|cowpea|lentil|chick|legume|alfalfa|clover|lupin"
crop = d.crop.fillna("").astype(str).str.lower()
d["crop_grp"] = np.select([crop.str.contains("rice", na=False).to_numpy(bool),
                           crop.str.contains(cer, na=False).to_numpy(bool),
                           crop.str.contains(leg, na=False).to_numpy(bool)],
                          ["rice", "cereal", "legume"], "other")
d["abslat"] = d.lat.abs()
d["tropical"] = (d.abslat < 23.5).astype(float).where(d.lat.notna())
d["wood"] = d.feed.fillna("").astype(str).str.lower().str.contains("wood|bamboo|pine|oak|forest|timber|sawdust|willow|eucalypt", na=False).astype(float)
d["first_year"] = (d.years <= 1.01).astype(float).where(d.years.notna())
d["v"] = d.v.where(d.v > 0).fillna(d.v.median()).clip(lower=1e-4)


def prep(p):
    e = d.copy()
    e["a"] = e.lnRR / (e.dose / 10.0) ** p
    e["va"] = e.v / ((e.dose / 10.0) ** p) ** 2
    e = e[e.a.abs() < 3]                         # drop implausible normalised values (very low doses)
    return e.reset_index(drop=True)


FEATS = ["pH", "CEC", "SOC", "clay", "abslat", "tropical", "first_year", "wood", "bcC", "pH_bc",
         "crop_rice", "crop_cereal", "crop_legume"]


def design(e):
    X = e[["pH", "CEC", "SOC", "clay", "abslat", "tropical", "first_year", "wood", "bcC", "pH_bc"]].copy()
    for g in ["rice", "cereal", "legume"]:
        X[f"crop_{g}"] = (e.crop_grp == g).astype(float)
    return X


def grouped_cv(e, model_fn, nrep=5, k=10):
    X = design(e); y = e.a.values; w = 1.0 / (e.va.values + TAU2); groups = e.study.values
    preds = np.zeros((nrep, len(e)))
    for r in range(nrep):
        # shuffle study labels so GroupKFold folds differ between repeats
        rng = np.random.default_rng(r); us = np.unique(groups); perm = dict(zip(us, rng.permutation(len(us))))
        g2 = np.array([perm[g] for g in groups])
        for tr, te in GroupKFold(n_splits=k).split(X, y, g2):
            preds[r, te] = model_fn(X.iloc[tr], y[tr], w[tr], X.iloc[te], e.iloc[tr], e.iloc[te])
    pr = preds.mean(0)
    ybar = np.average(y, weights=w)
    r2w = 1 - np.sum(w * (y - pr) ** 2) / np.sum(w * (y - ybar) ** 2)
    # study-level R2: average observed vs predicted per held-out study (what the global map needs)
    st = pd.DataFrame({"s": groups, "y": y, "p": pr, "w": w}).groupby("s").apply(
        lambda q: pd.Series({"y": np.average(q.y, weights=q.w), "p": np.average(q.p, weights=q.w)}), include_groups=False)
    r2s = 1 - ((st.y - st.p) ** 2).sum() / ((st.y - st.y.mean()) ** 2).sum()
    return r2w, r2s, pr


def m_null(Xtr, ytr, wtr, Xte, etr, ete):
    return np.full(len(Xte), np.average(ytr, weights=wtr))


def m_strat(Xtr, ytr, wtr, Xte, etr, ete):
    """pH class means (the simplest defensible alternative)."""
    cls = lambda ph: pd.cut(ph, [0, 5.5, 6.5, 7.5, 14], labels=False)
    ctr, cte = cls(etr.pH).values, cls(ete.pH).values
    out = np.full(len(Xte), np.average(ytr, weights=wtr))
    for c in range(4):
        sel = ctr == c
        if sel.sum() > 5:
            out[cte == c] = np.average(ytr[sel], weights=wtr[sel])
    return out


def m_metareg(Xtr, ytr, wtr, Xte, etr, ete):
    """multilevel meta-regression: pH (piecewise), low-CEC indicator, tropical, crop group; missing -> indicator."""
    def feats(e):
        f = pd.DataFrame(index=e.index)
        ph = e.pH.fillna(6.5)
        f["acid"] = np.clip(6.5 - ph, 0, None); f["alk"] = np.clip(ph - 7.5, 0, None)
        f["lowcec"] = (e.CEC < 10).astype(float).where(e.CEC.notna(), 0.0); f["cec_na"] = e.CEC.isna().astype(float)
        f["trop"] = e.tropical.fillna(0.0)
        f["legume"] = (e.crop_grp == "legume").astype(float); f["rice"] = (e.crop_grp == "rice").astype(float)
        return f
    ftr, fte = feats(etr), feats(ete)
    tr = etr.assign(a=ytr, va=1 / wtr - TAU2, **ftr)
    tr["va"] = tr.va.clip(lower=1e-5)
    res = metareg.fit(tr, "a", "va", list(ftr.columns))
    beta = res["table"].est.values
    return np.column_stack([np.ones(len(fte)), fte.values]) @ beta


def m_gbm(Xtr, ytr, wtr, Xte, etr, ete):
    cst = [-1 if c == "pH" else 0 for c in Xtr.columns]   # monotone: response falls as pH rises
    cst = [(-1 if c in ("pH", "CEC", "SOC") else 0) for c in Xtr.columns]
    mdl = HistGradientBoostingRegressor(max_depth=3, learning_rate=0.05, max_iter=300, min_samples_leaf=30,
                                        l2_regularization=1.0, monotonic_cst=cst, random_state=0)
    mdl.fit(Xtr, ytr, sample_weight=wtr)
    return mdl.predict(Xte)


def m_rf(Xtr, ytr, wtr, Xte, etr, ete):
    med = Xtr.median()
    Xa = Xtr.fillna(med).assign(**{f"{c}_na": Xtr[c].isna().astype(float) for c in ["pH", "CEC", "SOC", "clay"]})
    Xb = Xte.fillna(med).assign(**{f"{c}_na": Xte[c].isna().astype(float) for c in ["pH", "CEC", "SOC", "clay"]})
    mdl = RandomForestRegressor(n_estimators=400, min_samples_leaf=10, max_features=0.5, random_state=0, n_jobs=-1)
    mdl.fit(Xa, ytr, sample_weight=wtr)
    return mdl.predict(Xb)


if __name__ == "__main__":
    for p in [0.50, 0.35]:
        e = prep(p)
        print(f"\n=== normalising exponent p={p}: {len(e)} field obs, {e.study.nunique()} studies; "
              f"CEC present {e.CEC.notna().mean():.0%}, pH present {e.pH.notna().mean():.0%}")
        out = {}
        for name, fn in [("null (grand mean)", m_null), ("pH-class means", m_strat), ("meta-regression", m_metareg),
                         ("boosted trees (monotone)", m_gbm), ("random forest", m_rf)]:
            r2w, r2s, pr = grouped_cv(e, fn, nrep=3 if name in ("meta-regression", "random forest") else 5)
            out[name] = pr
            print(f"  {name:26s} grouped-CV R2 (obs, weighted) = {r2w:6.3f}   study-level R2 = {r2s:6.3f}")
        if p == 0.50:
            # in-sample random-CV for contrast (what leaky CV would report)
            from sklearn.model_selection import KFold
            X = design(e); y = e.a.values; w = 1 / (e.va.values + TAU2); pr = np.zeros(len(e))
            for tr, te in KFold(10, shuffle=True, random_state=0).split(X):
                pr[te] = m_gbm(X.iloc[tr], y[tr], w[tr], X.iloc[te], e.iloc[tr], e.iloc[te])
            r2 = 1 - np.sum(w * (y - pr) ** 2) / np.sum(w * (y - np.average(y, weights=w)) ** 2)
            print(f"  (boosted trees with ordinary random 10-fold CV, for contrast: R2 = {r2:.3f})")
            # permutation importance from a GBM fitted on all data, evaluated with grouped CV folds
            X = design(e); y = e.a.values; w = 1 / (e.va.values + TAU2)
            imps = []
            for tr, te in GroupKFold(5).split(X, y, e.study.values):
                mdl = HistGradientBoostingRegressor(max_depth=3, learning_rate=0.05, max_iter=300, min_samples_leaf=30,
                                                    l2_regularization=1.0, random_state=0).fit(X.iloc[tr], y[tr], sample_weight=w[tr])
                pi = permutation_importance(mdl, X.iloc[te], y[te], sample_weight=w[te], n_repeats=10, random_state=0)
                imps.append(pi.importances_mean)
            imp = pd.Series(np.mean(imps, 0), index=X.columns).sort_values(ascending=False)
            print("  held-out permutation importance (drop in weighted R2):"); print((imp).round(4).to_string())
            # meta-regression on all data, for the coefficient table
            r = metareg.fit(e.assign(**{"acid": np.clip(6.5 - e.pH, 0, None), "alk": np.clip(e.pH - 7.5, 0, None),
                                        "lowcec": (e.CEC < 10).astype(float)}).dropna(subset=["pH"]),
                            "a", "va", ["acid", "alk"])
            metareg.show(r, "meta-regression, pH terms only (response at 10 Mg/ha, lnRR)")
            ec = e.dropna(subset=["pH", "CEC"])
            r = metareg.fit(ec.assign(acid=np.clip(6.5 - ec.pH, 0, None), alk=np.clip(ec.pH - 7.5, 0, None),
                                      cec=np.log(ec.CEC)), "a", "va", ["acid", "alk", "cec"])
            metareg.show(r, "meta-regression, pH + log(CEC), obs with CEC")
            e.assign(pred_gbm=out["boosted trees (monotone)"], pred_meta=out["meta-regression"]).to_csv(
                "/home/claude/bc/amplitude_obs.csv", index=False)
