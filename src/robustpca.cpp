#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List robustPCAadmm_c(arma::mat& M, double gamma = 0.1, double tau = 1, double eps = 0.001){
  int n = M.n_rows, p = M.n_cols;
  arma::mat S(n, p, arma::fill::zeros), eta(n, p, arma::fill::zeros);
  double gammatau = gamma * tau;
  arma::mat L = M - S;
  double obj_new = (unsigned)!((int)0), obj;
  arma::mat Q, R, R_t, soft_S;
  arma::colvec d, soft_d;
  do {
    obj = obj_new;
    arma::svd_econ(Q, d, R, M - S - eta);
    soft_d = arma::abs(d) - tau;
    soft_d(arma::find(soft_d < 0)).zeros();
    d = arma::sign(d) % soft_d;
    R_t = R.t();
    L = Q * (R_t.each_col() % d);
    soft_S = arma::abs(M - L - eta) - gammatau;
    soft_S(arma::find(soft_S < 0)).zeros();
    S = arma::sign(M - L - eta) % soft_S;
    eta = eta + S + L - M;
    obj_new = sum(d) + gamma * arma::accu(arma::abs(S));
  } while(std::fabs(obj - obj_new) >= eps);
  return Rcpp::List::create(Rcpp::Named("L") = L, Rcpp::Named("S") = S, Rcpp::Named("eta") = eta);
}
