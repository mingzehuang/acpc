#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
arma::colvec soft_I(arma::vec& A, double lambda){
  arma::vec soft_A = arma::abs(A) - lambda;
  soft_A(arma::find(soft_A < 0)).zeros();
  return arma::sign(A) % soft_A;
}

// [[Rcpp::export]]
arma::mat soft_M(arma::mat& A, double lambda){
  arma::mat soft_A = arma::abs(A) - lambda;
  soft_A(arma::find(soft_A < 0)).zeros();
  return arma::sign(A) % soft_A;
}

// [[Rcpp::export]]
Rcpp::List robustPCAadmm(arma::mat& M, double gamma = 0.1, double tau = 1, double eps = 0.001){
  int n = M.n_rows;
  int p = M.n_cols;
  arma::mat S(n, p, arma::fill::zeros);
  arma::mat eta(n, p, arma::fill::zeros);
  double gammatau = gamma * tau;
  arma::mat L = M - S;
  double obj_new = (unsigned)!((int)0);
  double obj;
  arma::mat L_sn;
  arma::mat S_s;
  Rcpp::List sn;
  arma::vec s;
  arma::mat Q, R;
  arma::mat R_t;
  do {
    obj = obj_new;
    arma::svd_econ(Q, s, R, M - S - eta);
    arma::colvec newd = soft_I(s, tau);
    R_t = R.t();
    arma::mat newA = Q * (R_t.each_col() % newd);
    L = newA;
    S_s = M - L - eta;
    S = soft_M(S_s, gammatau);
    eta = eta + S + L - M;
    obj_new = arma::sum(newd) + gamma * arma::accu(arma::abs(S));
  } while(std::fabs(obj - obj_new) >= eps);
  return Rcpp::List::create(Rcpp::Named("L") = L, Rcpp::Named("S") = S, Rcpp::Named("eta") = eta);
}
