#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List Kmeans(const arma::mat& X, const arma::mat& M, int MaxIter) {
  int n = X.n_rows;
  int p = X.n_cols;
  int K = M.n_rows;
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
    distance = 2 * X * center_t;
    distance.each_row() -= arma::sum(arma::square(center_t), 0);
    Y = arma::index_max(distance, 1);
    Y_index(arma::sub2ind(arma::size(Y_index), (arma::join_rows(Y_r, Y)).t())).ones();
    center_new_t = X_t * Y_index;
    center_new_t.each_row() /= arma::sum(Y_index, 0);
    i++;
  } while((i < MaxIter) & (!(arma::approx_equal(center_new_t, center_t, "absdiff", 0))));
  return Rcpp::List::create(Rcpp::Named("Y") = Y, Rcpp::Named("center") = center_new_t.t());
}
