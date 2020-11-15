// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// simple example of creating two matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export]]
double normArmaM(const arma::mat& X, int p = 2){
    return(arma::norm(X, p));
}

// [[Rcpp::export]]
double normArmaV(const arma::colvec& Y, int p = 2){
    return(arma::norm(Y, p));
}

// [[Rcpp::export]]
arma::mat procrustes(arma::mat& X, arma::mat &V){
    arma::mat U;
    
    arma::vec s;
    arma::mat Q, R;
    
    arma::svd_econ(Q, s, R, X * V);
    U = Q * R.t();
    return(U);
}


// [[Rcpp::export]]
double soft_I(double a, double lambda){
    if (a > lambda){
        return(a-lambda);
    }else if (a < -lambda){
        return(a + lambda);
    }else{
        return(0);
    }
}

//Sparse PCA problem algorithm
// [[Rcpp::export]]
Rcpp::List sparsePCA(arma::mat& X, arma::mat& Vstart, double lambda, double eps = 0.0001){
    
    // initialize U
    int r = Vstart.n_cols;
    int p = Vstart.n_rows;
    arma::vec s;
    arma::mat Q, R;
    arma::mat U;
    arma::svd_econ(Q, s, R, X * Vstart);
    U = Q * R.t();
    arma::mat V(p, r);
    
    // Calculate current objective
    double fold, fnew;
    fold = accu(square(X - U * Vstart.t()))/2 + lambda * accu(abs(Vstart));
    
    // Additional intermediate
    arma::mat XU;
    
    // To store error and objective function difference
    double error = 1000;
    
    // Alternate updates of U with updates of V
    while (error > eps){
        XU = X.t() * U;
        // Update V
        for (int j = 0; j < p; j++){
            for (int k = 0; k < r; k++){
                V(j, k) = soft_I(XU(j, k), lambda);
            }
        }
        
        // Update U
        arma::svd_econ(Q, s, R, X * V);
        U = Q * R.t();
        
        // Calculate new objective
        fnew = accu(square(X - U * V.t()))/2 + lambda * accu(abs(V));
        
        // Calculate error
        error = std::abs(fold - fnew);
        
        fold = fnew;
    }
    return Rcpp::List::create(Rcpp::Named("U") = U, Rcpp::Named("V") = V, Rcpp::Named("error") = error);
}