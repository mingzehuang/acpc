#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::ucolvec MyKmeans_c(const arma::mat& X, int K, const arma::mat& M, int numIter) {
  arma:mat Xv = sparsePCA(X, Vstart, lambda, eps)
  int n = X.n_rows;
  int p = X.n_cols;
  int KM = M.n_rows;
  int pM = M.n_cols;
  if (numIter < 1) {
    throw std::runtime_error("Maximal number of iterations should be at least 1, whereas " + std::to_string(numIter) + "smaller than 1.");
  } else if (K > n) {
    throw std::runtime_error("Number of clusters K should not exceed number of data points n, whereas K = " + std::to_string(K) + "greater than rows of X =" + std::to_string(n));
  } else if (K < 1) {
    throw std::runtime_error("Number of clusters K should be at least 1, whereas K = " + std::to_string(K) + "smaller than 1.");
  } else if (KM != K) {
    throw std::runtime_error("Number of Cluster centers should be equal to number of clusters K, whereas M contains" + std::to_string(KM) + "rows, not equal to K =" + std::to_string(K));
  } else if (pM != p) {
    throw std::runtime_error("Dimensions of centers M should be equal to dimensions of data points X, whereas M contains" + std::to_string(pM) + "columns and X contains" + std::to_string(p) + "columns.");
  };
  arma::mat center_t(p, K);
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
  } while((i < numIter) & (!(arma::approx_equal(center_new_t, center_t, "absdiff", 0))));;
  return Y;
}