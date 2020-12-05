#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// [[Rcpp::export]]
arma::mat robustPCA(arma::mat& M, double eps, int MaxIter, double gamma = 0.1, double tau = 1){
  int n = M.n_rows, p = M.n_cols;
  arma::mat S(n, p, arma::fill::zeros), eta(n, p, arma::fill::zeros);
  double gammatau = gamma * tau;
  arma::mat L = M - S;
  double obj_new = (unsigned)!((int)0), obj;
  arma::mat Q, R, R_t, soft_S;
  arma::colvec d, soft_d;
  int i = 0;
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
    i++;
  } while((i < MaxIter) & (std::abs(obj - obj_new) >= eps));
  return L;
}

// [[Rcpp::export]]
Rcpp::List sparsePCA(arma::mat& M, int r, double eps, int MaxIter, double lambda = 1, double gamma = 0.1, double tau = 1){
  arma::mat X = robustPCA(M, eps, MaxIter, gamma, tau);
  int p = X.n_cols;
  double obj_new = (unsigned)!((int)0),obj;
  arma::colvec s;
  arma::mat Q, R;
  arma::mat U;
  arma::mat X_t = X.t();
  arma::mat tXU, soft_tXU;
  arma::mat V(p, r, arma::fill::zeros);
  int i = 0;
  do {
    obj = obj_new;
    arma::svd_econ(Q, s, R, X * V);
    U = Q * R.t();
    tXU = X_t * U;
    soft_tXU = arma::abs(tXU) - lambda;
    soft_tXU(arma::find(soft_tXU < 0)).zeros();
    V = arma::sign(tXU) % soft_tXU;
    obj_new = arma::accu(square(X - U * V.t()))/2 + lambda * arma::accu(abs(V));
    i++;
  } while ((i < MaxIter) & (std::abs(obj - obj_new) >= eps));
  return Rcpp::List::create(Rcpp::Named("U") = U, Rcpp::Named("V") = V);
}
