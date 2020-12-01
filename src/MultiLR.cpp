#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List LRMultiClass_c(const arma::mat& X, const arma::ucolvec& y, const arma::mat& Xt, const arma::ucolvec& yt, const int& numIter, const double& eta, const double& lambda, const arma::mat& beta_init) {
  int n = X.n_rows;
  int p = X.n_cols;
  int ny = y.size();
  arma::ucolvec class_y = sort(unique(y));
  int K = class_y.size();
  int ntest = Xt.n_rows;
  int ptest = Xt.n_cols;
  int nytest = yt.size();
  arma::ucolvec class_ytest = sort(unique(yt));
  arma::colvec X_c1 = X.col(0) - arma::ones< arma::colvec >(n);
  int pbeta_init = beta_init.n_rows;
  int Kbeta_init = beta_init.n_cols;
  arma::colvec Xt_c1 = Xt.col(0) - arma::ones< arma::colvec >(ntest);
  if (any(X_c1) | any(Xt_c1)) {
    throw std::runtime_error("The first column of X and Xt should be 1s to include intercepts.");
  } else if (n != ny) {
    throw std::runtime_error("The number of training data points should be equal to class labels of training data points, whereas X has" + std::to_string(n) + "rows but y has" + std::to_string(ny) + "elements.");
  } else if (ntest != nytest) {
    throw std::runtime_error("The number of test data points should be equal to class labels of test data points, whereas Xt has" + std::to_string(ntest) + "rows but yt has" + std::to_string(nytest) + "elements.");
  } else if (p != ptest) {
    throw std::runtime_error("The number of features for test data points should be equal to the number for training data points, whereas test data has" + std::to_string(ptest) + "features but training data has" + std::to_string(p) + "features (include intercepts).");
  } else if (eta <= 0) {
    throw std::runtime_error("eta should be positive, whereas eta =" + std::to_string(eta) + "is not greater than 0.");
  } else if (lambda < 0) {
    throw std::runtime_error("lambda should be non-negative, whereas lambda =" + std::to_string(lambda) + "is less than 0.");
  } else if (pbeta_init != p) {
    throw std::runtime_error("Initial value for parameters should be corresponding to the number of features, whereas beta_init has" + std::to_string(pbeta_init) + "rows but training data has" + std::to_string(p) + "columns (include intercepts).");
  } else if (Kbeta_init != K) {
    throw std::runtime_error("Initial value for parameters should be corresponding to the number of classes, whereas beta_init has" + std::to_string(Kbeta_init) + "columns but training data has" + std::to_string(K) + "classes.");
  } else if (!(arma::approx_equal(class_y, class_ytest, "absdiff", 0))) {
    throw std::runtime_error("The class labels of test data do not match the labels of training data.");
  };
  arma::colvec objective(numIter + 1);
  arma::colvec error_train(numIter + 1);
  arma::colvec error_test(numIter + 1);
  arma::mat indmat(n, K, arma::fill::zeros);
  arma::ucolvec Y_r = arma::regspace< arma::ucolvec >(0, n - 1);
  indmat(arma::sub2ind(arma::size(indmat), (arma::join_rows(Y_r, y)).t())).ones();
  arma::uvec ind_c = arma::find(indmat == 0);
  arma::mat lambda_diag = arma::diagmat((lambda * arma::ones(p)));
  arma::mat beta = beta_init;
  arma::mat eXb;
  arma::mat eXb_test;
  arma::ucolvec y_pred;
  arma::ucolvec ytest_pred;
  arma::colvec denominator_prob_beta;
  arma::mat prob_beta;
  arma::mat pick_prob_beta;
  arma::mat w;
  arma::mat gradient;
  arma::mat X_t = X.t();
  arma::mat H;
  for (int t = 0; ; t++) {
    eXb = arma::exp(X * beta);
    y_pred = arma::index_max(eXb, 1);
    denominator_prob_beta = sum(eXb, 1);
    prob_beta = eXb.each_col() / denominator_prob_beta;
    error_train(t) = 100 * sum(y_pred != y) / n;
    eXb_test = exp(Xt * beta);
    ytest_pred = arma::index_max(eXb_test, 1);
    error_test(t) = 100 * sum(ytest_pred != yt) / ntest;
    pick_prob_beta = prob_beta;
    pick_prob_beta(ind_c).zeros();
    objective(t) = - sum(log(sum(pick_prob_beta, 1))) + (lambda / 2) * arma::accu(arma::square(beta));
    if (t == numIter) {
      std::cout << "Maximal number of Iterations =" +std::to_string(numIter) + "is reached." << std::endl;
      break;
    } else {
      w = prob_beta % (1 - prob_beta);
      gradient = X_t * (prob_beta - indmat) + lambda * beta;
      for (int j = 0; j < K; j++) {
        H = X_t * (X.each_col() % w.col(j)) + lambda_diag;
        beta.col(j) = beta.col(j) - eta * arma::solve(H, gradient.col(j));
      };
    };
  };
  return Rcpp::List::create(Rcpp::Named("beta") = beta, Rcpp::Named("error_train") = error_train, Rcpp::Named("error_test") = error_test, Rcpp::Named("objective") = objective);
}