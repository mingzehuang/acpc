// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// A - n x p matrix
// B - m x p matrix
arma::mat euclidDist(const arma::mat& A, const arma::mat& B){
  arma::mat AB = - 2 * (A * arma::trans(B));
  arma::mat dist = AB.each_row() + sum(arma::square(B), 1).t();
  //returns the distances between each pair of rows of A and B
  return(dist);
}

// X - n x p matrix of points
// Y - n vector of labels for points in X
// K - number of distinct labels in Y
arma::mat newClusterCenters(const arma::mat& X, const arma::uvec& Y, int K) {
  // M stores cluster centers
  arma::mat M(K, X.n_cols, arma::fill::zeros);
  // count stores the number of points in each cluster
  arma::colvec count(K, arma::fill::zeros);
  int n = X.n_rows;
  for (arma::uword i = 0; i < n; ++i) {  // I think if you define i and calculate X.n_rows before the loop, it would be faster.
    M.row(Y.at(i)) += X.row(i);
    ++count(Y.at(i));
  }
  M.each_col() /= count;
  // return M - K x p matrix of new cluster centers
  return(M);
}

// X - n x p matrix
// K - number of clusters
// M - K x p cluster centers (always given)
// numIter - maximal number of iterations
// [[Rcpp::export]]
arma::uvec MyKmeans_submission(const arma::mat& X, int K,
                               const arma::mat& M, int numIter){
  // All input is assumed to be correct
  
  // Initialize some parameters
  int n = X.n_rows;
  int p = X.n_cols;
  arma::uvec Y(n); // to store cluster assignments
  
  // Initialize any additional parameters if needed
  int counter = 0;
  arma::mat Mprev = M + 1;
  arma::mat Mnext = M;
  
  // For loop with kmeans algorithm
  while (counter < numIter && any(vectorise(Mprev != Mnext))) {
    Y = index_min(euclidDist(X, Mnext), 1);
    ++counter;
    Mprev = Mnext;
    // compute new cluster centers based on new cluster assignments
    Mnext = newClusterCenters(X, Y, K);
    // compute new cluster assignments
  }
  
  // Returns the vector of cluster assignments
  return(Y + 1);
}

