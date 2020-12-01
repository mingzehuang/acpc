// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// y - input vector of classes
// n - length of y
// K - number of classes in y
arma::mat encode(const arma::uvec& y, const int& n, const int& K) {
  
  arma::mat indmat(n, K, arma::fill::zeros); //set matrix for one-hot encoding
  arma::ucolvec Y_r = arma::regspace< arma::ucolvec >(0, n - 1); // Set linear numbers to convert label into subscription.
  indmat(arma::sub2ind(arma::size(indmat), (arma::join_rows(Y_r, y)).t())).ones(); //Combine linear numbers with labels, set the selected element to 1.
  
  // output one-hot-encoded y in the form of matrix
  return indmat;
}

// For simplicity, no test data, only training data, and no error calculation.
// X - n x p data matrix
// y - n length vector of classes, from 0 to K-1
// numIter - number of iterations, default 50
// eta - damping parameter, default 0.1
// lambda - ridge parameter, default 1
// beta_init - p x K matrix of starting beta values (always supplied in right format)
// [[Rcpp::export]]
Rcpp::List LRMultiClass_c(const arma::mat& X_ext, const arma::mat& X, const arma::uvec& y, int numIter, double eta = 0.1, double lambda = 1){
  // All input is assumed to be correct
  
  // Initialize some parameters
  int K = max(y) + 1; // number of classes
  int p = X.n_cols;
  int n = X.n_rows;
  // Initialize anything else that you may need
  arma::mat beta = arma::mat(0, p, K); // to store betas and be able to change them if needed
  arma::vec objective(numIter + 1); // to store objective values
  arma::mat indmat = encode(y, n, K); // encode y using one-hot encoding method
  arma::uvec ind_c = arma::find(indmat == 0);  // get index for other elements
  arma::mat lambda_diag = arma::diagmat((lambda * arma::ones(p))); // diagonal lambda matrix
  arma::mat eXb = arma::exp(X * beta); // Set matrix for exponential of Xb.
  arma::ucolvec y_pred = arma::index_max(eXb, 1); // Index for predicted y group.
  arma::colvec denominator_prob_beta = sum(eXb, 1); // Denominator for the probability given beta.
  arma::mat prob_beta = eXb.each_col() / denominator_prob_beta; // Probability given beta.
  arma::mat pick_prob_beta  = prob_beta; // Selected probability given beta.
  arma::mat w; // w matrix for Hessian calculation.
  arma::mat gradient; // Gradient matrix.
  arma::mat X_t = X.t(); // Transposed X.
  arma::mat H; // Hessian matrix.
  // Compute first objective with un-updated beta
  pick_prob_beta(ind_c).zeros();
  objective(0) = - sum(log(sum(pick_prob_beta, 1))) + (lambda / 2) * arma::accu(arma::square(beta));
  
  // Newton's method cycle - implement the update EXACTLY numIter iterations
  for (int t = 0; t < numIter; ++t) {
    // update beta
    w = prob_beta % (1 - prob_beta);
    gradient = X_t * (prob_beta - indmat) + lambda * beta;
    for (int j = 0; j < K; j++) {
      H = X_t * (X.each_col() % w.col(j)) + lambda_diag;
      beta.col(j) = beta.col(j) - eta * arma::solve(H, gradient.col(j));
    };
    // compute new objective
    eXb = arma::exp(X * beta);
    y_pred = arma::index_max(eXb, 1);
    denominator_prob_beta = sum(eXb, 1);
    prob_beta = eXb.each_col() / denominator_prob_beta;
    pick_prob_beta = prob_beta;
    pick_prob_beta(ind_c).zeros();
    objective(t + 1) = - sum(log(sum(pick_prob_beta, 1))) + (lambda / 2) * arma::accu(arma::square(beta));
  }
    arma::uvec y_ext = arma::exp(X_ext * beta)
  // Create named list with betas and objective values
  return y_ext;
}
