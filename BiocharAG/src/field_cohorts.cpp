// Cohort sums of the biochar field-application model (biochar_field_effects), one dose option.
// For each cell, two application histories (cohorts with q + 1 and q applications every n years,
// starting in relative year 1) are stepped forward year by year over the horizon H. The effective stock
// B decays at k, its dose response g(B) follows the power law (B / B_r)^p above B_min and is linear
// below; the N2O reduction factor decays at lam after each application. Cohort sums are accumulated
// exactly as in the vectorized R implementation (same operations, in the same order).
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

//' Cohort Sums of the Biochar Field Model (Compiled)
//'
//' One dose option of [biochar_field_effects()]: discounted yield-response sums `S_y` and N2O-reduction
//' sums `S_n` per cell, from the two cohort application histories.
//'
//' @param n Cohorts per cell; @param d_eff Effective dose (Mg ha-1); @param k Stock decay rate (yr-1);
//' @param a10 Yield log response ratio at `B_r`.
//' @param B_r,B_min,p Dose-response parameters; @param lam N2O-reduction decay rate (yr-1);
//' @param T Plant life (yr); @param H Horizon (yr); @param r Discount rate.
//' @return List with numeric vectors `S_y` and `S_n`.
//' @keywords internal
// [[Rcpp::export]]
List field_cohort_sums_cpp(NumericVector n, NumericVector d_eff, NumericVector k, NumericVector a10,
                           double B_r, double B_min, double p, double lam, int T, int H, double r) {
  const int nc = n.size();
  NumericVector S_y(nc), S_n(nc);
  const double s_lin = std::pow(B_min / B_r, p) / B_min;
  const double elam = std::exp(-lam);
  std::vector<double> disc(H + 1), pw(T + 1);
  for (int u = 1; u <= H; ++u) disc[u] = std::pow(1.0 + r, -u);
  for (int j = 0; j <= T; ++j) pw[j] = std::pow(1.0 + r, -j);
  auto g_full = [&](double B) { return B < B_min ? s_lin * B : std::pow(B / B_r, p); };

  for (int i = 0; i < nc; ++i) {
    // For n > T + 1 the results do not depend on n (no cohort is treated twice); capping avoids overflow
    const int ni = n[i] > T + 1 ? T + 1 : (int) n[i];
    const double de = d_eff[i];
    const int q = T / ni;              // applications of the "lo" cohorts
    const int rem = T - q * ni;        // cohorts 0 .. rem-1 get q + 1 applications
    const int nt = std::min(ni, T);    // cohorts treated at least once
    const int c_hi = q + 1;
    const double w_lo = q > 0 ? 1.0 : 0.0;
    const bool do_lo = rem < nt;
    const double ek = std::exp(-k[i]), ekp = std::exp(-k[i] * p), a = a10[i];
    double B1 = 0, g1 = 0, a1 = 0, B0 = 0, g0 = 0, a0 = 0;
    int nx1 = 1, nx0 = 1, d1 = 0, d0 = 0;
    double P_hi = 0, P_lo = 0, N_hi = 0, N_lo = 0, sy = 0, sn = 0;
    for (int u = 1; u <= H; ++u) {
      B1 *= ek; g1 *= ekp; a1 *= elam;
      if (B1 < B_min) g1 = s_lin * B1;
      if (u <= T && nx1 == u && d1 < c_hi) {
        nx1 += ni; d1 += 1; B1 += de; g1 = g_full(B1); a1 = 1;
      }
      if (a != 0 && g1 != 0) P_hi += disc[u] * std::expm1(a * g1); // the term is exactly 0 otherwise
      N_hi += a1;
      if (do_lo) {
        B0 *= ek; g0 *= ekp; a0 *= elam;
        if (B0 < B_min) g0 = s_lin * B0;
        if (u <= T && nx0 == u && d0 < q) {
          nx0 += ni; d0 += 1; B0 += de; g0 = g_full(B0); a0 = 1;
        }
        if (a != 0 && g0 != 0) P_lo += disc[u] * w_lo * std::expm1(a * g0);
        N_lo += w_lo * a0;
      }
      const int j = H - u;             // cohort whose horizon ends at relative year u
      if (j <= T - 1 && j < nt) {
        const bool is_hi = j < rem;
        sy += pw[j] * (is_hi ? P_hi : P_lo);
        sn += is_hi ? N_hi : N_lo;
      }
    }
    S_y[i] = sy;
    S_n[i] = sn;
  }
  return List::create(_["S_y"] = S_y, _["S_n"] = S_n);
}
