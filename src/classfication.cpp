#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List MyKmeans_c(const arma::mat& X, int K, const arma::mat& M, const arma::colvec& W, int numIter) {
  int n = X.n_rows;
  int p = X.n_cols;
  arma::mat center_t(p, K);
  arma::mat sqcenter_t(p, K);
  arma::mat center_new_t = M.t();
  arma::mat distance(n, K);
  arma::ucolvec Y(n);
  arma::ucolvec Y_r = arma::regspace< arma::ucolvec >(0, n - 1);
  arma::mat Y_index(n, K);
  arma::mat X_t = X.t();
  int i = 0;
  do {
    center_t = center_new_t;
    Y_index.zeros();
    distance = 2 * X * (center_t.each_col() % W);
    sqcenter_t = arma::square(center_t);
    distance.each_row() -= arma::sum((sqcenter_t.each_col() % W), 0);
    Y = arma::index_max(distance, 1);
    Y_index(arma::sub2ind(arma::size(Y_index), (arma::join_rows(Y_r, Y)).t())).ones();
    center_new_t = X_t * Y_index;
    center_new_t.each_row() /= arma::sum(Y_index, 0);
    i++;
  } while((i < numIter) & (!(arma::approx_equal(center_new_t, center_t, "absdiff", 0))));
  return Rcpp::List::create(Rcpp::Named("Y") = Y, Rcpp::Named("center") = center_new_t.t());
}
