"""Final parameter estimates for the handover spec.

Yield amplitude: each field observation normalised to 10 Mg biochar/ha with the dose exponent of its
soil-pH class (pH>=6.5: 0.30; pH<6.0: 0.64; between: 0.50), then
    a10 = b0 + b_acid*max(0, 6.5-pH) + b_cec*ln(CEC)      (multilevel meta-regression, study random effect)
The acid term is the liming (pH) part of the yield response, credited separately as avoided lime;
the CEC yield credit uses a10_CEC = b0 + b_cec*ln(CEC), i.e. the response of a soil at pH >= 6.5.
"""
import json
import numpy as np
import pandas as pd
import metareg
from amplitude import d as fielddata, TAU2

P_CLASS = {"neutral": 0.30, "acid": 0.64, "mid": 0.50}


def p_of(ph):
    return np.where(ph >= 6.5, P_CLASS["neutral"], np.where(ph < 6.0, P_CLASS["acid"], P_CLASS["mid"]))


e = fielddata.dropna(subset=["pH", "CEC"]).copy()
p = p_of(e.pH.values)
e["a"] = e.lnRR / (e.dose / 10.0) ** p
e["va"] = e.v / ((e.dose / 10.0) ** p) ** 2
e = e[e.a.abs() < 3]
e["acid"] = np.clip(6.5 - e.pH, 0, None)
e["lncec"] = np.log(e.CEC)
res = {}
for lab, cols in [("acid+lnCEC", ["acid", "lncec"]), ("lnCEC only, pH>=6.5 obs", ["lncec"])]:
    sub = e if "acid" in cols else e[e.pH >= 6.5]
    r = metareg.fit(sub, "a", "va", cols)
    metareg.show(r, lab)
    res[lab] = r
r = res["acid+lnCEC"]
b = r["table"].est
cec0 = np.exp(-b["(intercept)"] / b["lncec"])
print(f"\nCEC at which the CEC yield response reaches zero: {cec0:.1f} cmolc/kg")
for cec in [3, 5, 10, 15, 20, 30]:
    a = b["(intercept)"] + b["lncec"] * np.log(cec)
    print(f"  CEC {cec:>2}: a10_CEC = {a:.3f}  -> yield +{100*(np.exp(max(a,0))-1):.1f}% at 10 Mg biochar/ha")

out = {
    "dose_exponent": {"all_field": {"est": 0.50, "ci90": [0.29, 0.66]},
                      "pH_ge_6.5": {"est": 0.30, "ci90": [0.16, 0.46]},
                      "pH_lt_6.0": {"est": 0.64, "ci90": [0.38, 0.77]}},
    "amplitude_model": {"names": r["names"], "coef": list(map(float, b.values)),
                        "cov": np.asarray(r["cov"]).tolist(),
                        "s2_study": r["s2_study"], "n_obs": r["n"], "n_studies": r["studies"],
                        "cec_zero_response": float(cec0)},
    "n2o": {"lnRR_mean": -0.2243, "lnRR_se": 0.0681, "R_central": float(1 - np.exp(-0.2243)),
            "R_ci90": [float(1 - np.exp(-0.2243 + 1.645 * 0.0681)), float(1 - np.exp(-0.2243 - 1.645 * 0.0681))],
            "lowest_tested_dose_Mg_ha": 2.2, "n_studies": 36},
    "persistence_k_per_yr": {"ivw_all": 0.142, "ci90": [-0.43, 0.31]},
}
json.dump(out, open("/home/claude/bc/final_params.json", "w"), indent=1)
print(json.dumps({k: out[k] for k in ["n2o"]}, indent=1))
