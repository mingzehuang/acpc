
#' Area classification based on survey data.
#'
#' @param X n * (p + 1) data frame contains first column for observation name ID and the rest p numeric features for n observations, column names are feature IDs.
#' @param K Scalar, Number of clusters.
#' @param r Scalar, number of principle components you want to generate, r<< p.
#' @param eps_r Scalar, tolerance for robust PCA.
#' @param MaxIter_r Scalar, maximum iteration for robust PCA.
#' @param gamma Scalar, relative weights between nuclear norm and sparsity penalties for robust PCA.
#' @param tau Scalar, scale parameter for ADMM updates in robust PCA.
#' @param eps_s Scalar, tolerance for sparSe PCA.
#' @param MaxIter_s Scalar, maximum iteration for sparse PCA.
#' @param lambda Scalar, sparsity penalty parameter for sparse PCA.
#' @param MaxIter_k Scalar, maximum iteration for Kmeans.
#'
#' @return Label for clustering, Y; scores for observations, U; loadings for principle components, V.
#' @export
#'
#' @examples 
#' # Create matrix M according to M = low rank + few large elements
#' n = 100
#' p = 40
#' # Create rank 3 component
#' set.seed(1234)
#' out <- svd(matrix(rnorm(n*p), n, p), nu = 3, nv = 3)
#' trueL <- out$u %*% diag(out$d[1:3]) %*% t(out$v)
#' # Create sparse component
#' trueS <- matrix(rt(n*p, df = 1), n, p)
#' trueS[abs(trueS) < 2.5] <- 0
#' sum(trueS !=0)
#' # Create M by combining the above
#' M <- trueL + trueS
#' # Verify that M itself is not small rank and not even close to being rank 3
#' svd(M)$d
#' K = 5
#' M <- cbind(data.frame(as.character(1:n), M))
#' acpc(M, K)

acpc <- function(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3){
  X = na.omit(X) # Delete observations with missing value.
  ID = X[ , 1] # Extract name ID for observations.
  X = X[ , -1] # Extract features for observations.
  featureID = colnames(X) # Etract ID for features.
  X = as.matrix(X) # Convert features into matrix.
  n = nrow(X) # Compute rows of X.
  p = ncol(X) # Compute columns of X.
  if (eps_r <= 0 | eps_s <= 0) {
    stop(paste("Tolerance should be positive, whereas eps_r = ", eps_r, " or eps_s = ", eps_s, "not greater than 0."))
  } else if (MaxIter_r < 10 | MaxIter_s < 10 | MaxIter_k < 10) {
    stop(paste("To guarantee the possibility of convergence, please don't put maximum iteration number smaller than 10. ", MaxIter_r, " or ", MaxIter_s, " or ", MaxIter_k, "smaller than 10."))
  } else if (gamma <= 0 | tau <= 0) {
    stop(paste("Parameters gamma and tau should be positive for robust PCA, whereas gamma = ", gamma, " or tau = ", tau, "smaller or equal to 0."))
  } else if (lambda < 0) {
    stop(paste("Parameter lambda should be non-negative for sparse PCA, whereas lambda = ", lambda, "small than 0."))
  } else if (length(featureID) < p) {
    stop("Features should have column name as feature ID, otherwise you cannot interpret loadings.")
  } else if (!(is.numeric(X))) {
    stop("Features should be numeric to process robust sparse PCA and K-means.")
  } else if (K <= 1 | K > n) {
    stop(paste("Number of clusters should be integer at least 2, but not exceed number of observations, whereas clusters K = ", K, ", observations n = ", n, " after throwing missing value away."))
  } else if (r < 1 | r > p) {
    stop(paste("Number of principle conponents should integer be at least 1, but not exceed number of features, whereas principle components r = ", r, ", features p = ", p))
  }
  X = scale(X)* sqrt(n/(n-1))   # Center and scale X
  L = robustPCA(X, eps_r, MaxIter_r, gamma, tau) # Robust PCA
  spca = sparsePCA(L, eps_s, MaxIter_s, r, lambda = 0.1) # Sparse PCA
  U = spca$U
  V = spca$V
  rownames(V) = featureID # Add feature ID onto loading matrix.
  classification = Kmeans(U, U[sample(n, K), , drop = F], MaxIter_k) # K-means classification.
  Y = classification$Y + 1 # Adjust cluster labels since C++ index from 0 instead of 1.
  names(Y) = ID # Add observation ID onto score matrix.
  center = classification$center # Extract scores of cluster centers.
  # Return the class assignments
  return(list(Y = Y, U = U, center = center, V = V))
}