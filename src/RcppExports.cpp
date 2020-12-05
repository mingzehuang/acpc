// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// Kmeans
Rcpp::List Kmeans(const arma::mat& X, const arma::mat& M, const arma::colvec& W, int MaxIter);
RcppExport SEXP _acpc_Kmeans(SEXP XSEXP, SEXP MSEXP, SEXP WSEXP, SEXP MaxIterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type M(MSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type W(WSEXP);
    Rcpp::traits::input_parameter< int >::type MaxIter(MaxIterSEXP);
    rcpp_result_gen = Rcpp::wrap(Kmeans(X, M, W, MaxIter));
    return rcpp_result_gen;
END_RCPP
}
// robustPCA
arma::mat robustPCA(arma::mat& M, double eps, int MaxIter, double gamma, double tau);
RcppExport SEXP _acpc_robustPCA(SEXP MSEXP, SEXP epsSEXP, SEXP MaxIterSEXP, SEXP gammaSEXP, SEXP tauSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type M(MSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< int >::type MaxIter(MaxIterSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    rcpp_result_gen = Rcpp::wrap(robustPCA(M, eps, MaxIter, gamma, tau));
    return rcpp_result_gen;
END_RCPP
}
// sparsePCA
Rcpp::List sparsePCA(arma::mat& X, double eps, int MaxIter, int r, double lambda);
RcppExport SEXP _acpc_sparsePCA(SEXP XSEXP, SEXP epsSEXP, SEXP MaxIterSEXP, SEXP rSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< int >::type MaxIter(MaxIterSEXP);
    Rcpp::traits::input_parameter< int >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(sparsePCA(X, eps, MaxIter, r, lambda));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_acpc_Kmeans", (DL_FUNC) &_acpc_Kmeans, 4},
    {"_acpc_robustPCA", (DL_FUNC) &_acpc_robustPCA, 5},
    {"_acpc_sparsePCA", (DL_FUNC) &_acpc_sparsePCA, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_acpc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
