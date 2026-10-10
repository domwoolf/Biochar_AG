"""TabPFN as an additional model for the magnitude of the yield response, evaluated exactly like the
other candidates (study-grouped 10-fold CV, repeated; same normalisation to 10 Mg biochar/ha).

Run locally (needs PyPI and Hugging Face access; a GPU helps but CPU works for ~1,300 rows):
    pip install tabpfn            # first use downloads the weights; recent versions may require
                                  # accepting the licence on Hugging Face and `huggingface-cli login`
    python3 tabpfn_amp.py               # full comparison
    python3 tabpfn_amp.py --reps 2      # quicker
    python3 tabpfn_amp.py --dry-run     # checks the harness with a stand-in model (no TabPFN needed)

Needs, in the same folder: amplitude.py, amp_cec.py, metareg.py, fitlib.py, harmonised2.csv.
Notes:
  * TabPFN takes no sample weights; it is fitted unweighted. Scores use the same precision-weighted
    metrics as the other models, so comparisons are like for like on the evaluation side.
  * TabPFN handles missing values natively, so CEC, SOC, clay etc. are passed with NaN.
  * Study-level R2 (variance in held-out study means explained) is the number that matters for maps.
"""
import argparse
import time
import numpy as np
import pandas as pd
from amplitude import prep, grouped_cv, m_null, m_rf, m_gbm, m_metareg
from amp_cec import m_meta2, m_gbm2, m_rfall

ap = argparse.ArgumentParser()
ap.add_argument("--reps", type=int, default=5)
ap.add_argument("--n-estimators", type=int, default=8)
ap.add_argument("--dry-run", action="store_true")
args = ap.parse_args()

if args.dry_run:
    from sklearn.ensemble import ExtraTreesRegressor

    def make_model():
        class Stub:
            def fit(self, X, y):
                self.m = ExtraTreesRegressor(200, min_samples_leaf=10, random_state=0).fit(np.nan_to_num(X, nan=-1), y); return self
            def predict(self, X):
                return self.m.predict(np.nan_to_num(X, nan=-1))
        return Stub()
    LABEL = "stand-in (dry run)"
else:
    import torch
    from tabpfn import TabPFNRegressor
    DEVICE = "cuda" if torch.cuda.is_available() else "cpu"

    def make_model():
        kw = dict(n_estimators=args.n_estimators, random_state=0, device=DEVICE)
        try:
            return TabPFNRegressor(ignore_pretraining_limits=True, **kw)
        except TypeError:                 # older versions lack this argument
            return TabPFNRegressor(**kw)
    LABEL = f"TabPFN ({DEVICE})"


def m_tabpfn_all(Xtr, ytr, wtr, Xte, etr, ete):
    return make_model().fit(Xtr.values.astype(float), ytr).predict(Xte.values.astype(float))


def m_tabpfn_phcec(Xtr, ytr, wtr, Xte, etr, ete):
    c = ["pH", "CEC"]
    return make_model().fit(Xtr[c].values.astype(float), ytr).predict(Xte[c].values.astype(float))


def run(e, models, title):
    print(f"\n=== {title}: {len(e)} obs, {e.study.nunique()} studies ===")
    rows = []
    for name, fn in models:
        t0 = time.time()
        r2w, r2s, _ = grouped_cv(e, fn, nrep=args.reps, k=10)
        rows.append(dict(set=title, model=name, obs_R2=r2w, study_R2=r2s))
        print(f"  {name:36s} obs R2={r2w:6.3f}  study-level R2={r2s:6.3f}   ({time.time()-t0:.0f} s)", flush=True)
    return rows


e_all = prep(0.5)
e_cec = e_all.dropna(subset=["pH", "CEC"]).reset_index(drop=True)
out = []
out += run(e_all, [("null", m_null), ("meta-regression (pH, CEC class, crop)", m_metareg),
                   ("boosted trees, all covariates", m_gbm), ("random forest, all covariates", m_rf),
                   (f"{LABEL}, all covariates", m_tabpfn_all)], "All field observations")
out += run(e_cec, [("null", m_null), ("meta-regression: acid + log CEC", m_meta2),
                   ("boosted trees, pH + CEC", m_gbm2), ("random forest, all covariates", m_rfall),
                   (f"{LABEL}, pH + CEC", m_tabpfn_phcec), (f"{LABEL}, all covariates", m_tabpfn_all)],
           "Observations with pH and CEC")
pd.DataFrame(out).to_csv("tabpfn_comparison.csv", index=False)
print("\nSaved tabpfn_comparison.csv")
