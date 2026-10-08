"""Reference implementation of the revised biochar field-application model (for unit tests of the R port).

Per grid cell, per dose option D. All per-hectare quantities are per PHYSICAL hectare of source cropland.

  b      = b_h * CI                biochar supplied per physical ha per year (Mg/ha/yr)
  n      = max(1, ceil(D / b))     cohorts; each cohort receives D_eff = n*b every n years
  B_j,t  = sum_i D_eff * exp(-(k_y + k_C)(t - t_i))   effective stock of cohort j (applications t_i <= t <= T)
  g(B)   = (B/10)^p for B >= B_min;  (B_min/10)^p * B/B_min below (linear through origin)
  yield  = CI * V_crop * mean_j[ exp(a10_CEC * g(B_j,t)) - 1 ]                   US$/ha/yr
  N2O    = CI * N_app * EF1 * 44/28 * 1e-3 * R * min(1, D_eff/D_minN2O)
           * mean_j[ exp(-lambda (t - t_last_j)) ]                               Mg N2O/ha/yr (x GWP -> CO2e)
  passes = 1/n per ha per year for t <= T
Levelised economic value per Mg feedstock:  V = CRF(r,T) * PV_H(yield - c_pass*passes) / m,  m = b / Y_bc
Physical abatement per Mg feedstock (undiscounted, as for biochar C): sum_t N2O_t * GWP / (m T)
"""
import numpy as np


def crf(r, T):
    return r / (1 - (1 + r) ** -T)


def evaluate(b_h, CI, Y_bc, CEC, V_crop, N_app, EF1, D, r=0.062, T=20, H=100,
             b0=0.3607, b_cec=-0.1008, p=0.30, k_y=0.0, k_C=0.002, R=0.20, D_min_n2o=2.2,
             lam=np.log(2) / 3, B_min=2.5, c_pass=25.0, diesel_L_pass=17.5, ef_diesel=2.68e-3, gwp_n2o=273):
    b = b_h * CI
    m = b / Y_bc                                      # Mg feedstock per physical ha per yr
    n = max(1, int(np.ceil(D / b - 1e-9)))
    D_eff = n * b
    a10 = max(0.0, b0 + b_cec * np.log(CEC))
    t = np.arange(1, H + 1)
    B = np.zeros((n, H)); act = np.zeros((n, H))
    for ti in range(1, T + 1):                        # application years
        j = (ti - 1) % n
        B[j] += np.where(t >= ti, D_eff * np.exp(-(k_y + k_C) * (t - ti)), 0.0)
        act[j] = np.where(t >= ti, np.exp(-lam * (t - ti)), act[j])   # clock restarts at each application
    # below the lowest well-tested dose the response is linear through the origin (no extrapolation of the power law)
    shape = np.where(B >= B_min, (B / 10.0) ** p, (B_min / 10.0) ** p * B / B_min)
    yld = CI * V_crop * (np.exp(a10 * shape) - 1).mean(0)
    n2o = CI * N_app * EF1 * 44 / 28 * 1e-3 * R * min(1.0, D_eff / D_min_n2o) * act.mean(0)
    passes = np.where(t <= T, 1.0 / n, 0.0)
    disc = (1 + r) ** -t.astype(float)
    V_yield = crf(r, T) * (yld * disc).sum() / m
    V_spread = crf(r, T) * (c_pass * passes * disc).sum() / m
    A_n2o = (n2o * gwp_n2o).sum() / (m * T)
    E_diesel = (passes * diesel_L_pass * ef_diesel).sum() / (m * T)
    return dict(D_eff=D_eff, cohorts=n, V_yield=V_yield, V_spread=V_spread, A_n2o=A_n2o, E_diesel=E_diesel)


CASES = {
    # illustrative test cells (not model outputs): b_h at regional biochar-weighted medians
    "A: acid, low CEC, double-cropped (S China-like)": dict(b_h=1.62, CI=1.5, Y_bc=0.30, CEC=8, V_crop=1500, N_app=200, EF1=0.016),
    "B: neutral, mid CEC (US Corn Belt-like)": dict(b_h=1.18, CI=1.0, Y_bc=0.30, CEC=20, V_crop=1300, N_app=150, EF1=0.010),
    "C: low supply, low CEC (India-like)": dict(b_h=0.30, CI=1.4, Y_bc=0.28, CEC=10, V_crop=1600, N_app=120, EF1=0.004),
}

if __name__ == "__main__":
    print("Test values (US$ or Mg CO2e per Mg dry ash-free feedstock); r=0.062, T=20, H=100, p=0.30, k_y=0,")
    print("R=0.20, N2O half-life 3 yr, B_min=2.5, c_pass=25 US$/ha (placeholder), b0=0.3607, b_cec=-0.1008\n")
    for name, c in CASES.items():
        print(name)
        for D in [0.0, 2.5, 5, 10, 20]:
            o = evaluate(D=D, **c)
            print(f"  D={'annual' if D == 0 else D:>6}  D_eff={o['D_eff']:5.2f} cohorts={o['cohorts']:2d}  "
                  f"V_yield={o['V_yield']:6.2f}  V_spread={o['V_spread']:5.2f}  A_N2O={o['A_n2o']:.4f}  E_diesel={o['E_diesel']:.4f}")
        # sensitivity: persistence and dose exponent
        o0 = evaluate(D=5, **c)
        o1 = evaluate(D=5, k_y=np.log(2) / 3, **c)
        o2 = evaluate(D=5, p=0.50, **c)
        print(f"  D=5: V_yield k_y=0 {o0['V_yield']:.2f} | half-life 3 yr {o1['V_yield']:.2f} | p=0.50 {o2['V_yield']:.2f}\n")
