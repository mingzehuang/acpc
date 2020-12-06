
#' Area classification based on survey data.
#'
#' @param X n * (p + 1) matrix contains first column for name and the rest p numeric features for n observations.
#' @param cp K * 1 vector contains initial guess of cluster center points.
#' @param r Scalar, number of principle components you want to generate, r<< p.
#' @param W r * 1 vector, Weights on principle components.
#' @param eps_r Scalar, tolerance for robust PCA.
#' @param MaxIter_r Scalar, maximum iteration for robust PCA.
#' @param gamma Scalar, relative weights between nuclear norm and sparsity penalties for robust PCA.
#' @param tau Scalar, scale parameter for ADMM updates in robust PCA.
#' @param eps_s Scalar, tolerance for sparSe PCA.
#' @param MaxIter_s Scalar, maximum iteration for sparse PCA.
#' @param lambda Scalar, sparsity penalty parameter for sparse PCA.
#' @param MaxIter_k Scalar, maximum iteration for Kmeans.
#'
#' @return Label for clustering, Y; scores for observations, U; loadings for principle components, V; Observation name and corresponding cluster label, clusterlabel.
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

acpc <- function(X, K, r = 2, cp = NULL, W = NULL , eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 1, MaxIter_k = 1e+3){
  X = na.omit(X) # Delete observations with missing value.
  name = X[ ,1] # Extract name ID for observations.
  X = as.matrix(X[ ,-1]) # Extract features for observations.
  n = nrow(X) # Compute rows of X.
  p = ncol(X) # Compute columns of X.
  X = scale(X)* sqrt(n/(n-1))   # Center and scale X
  L = robustPCA(X, eps_r, MaxIter_r, gamma, tau) # Robust PCA
  spca = sparsePCA(L, eps_s, MaxIter_s, r, lambda) # Sparse PCA
  U = spca$U
  V = spca$V
  if (missing(cp) | is.null(cp)) {
    cp = sample(n, K)
  }
  M = U[cp, , drop = F]
  if (missing(W) | is.null(W)) {
    W = rep(1/r, r)
  }
  Y = Kmeans(U, M, W, MaxIter_k)
  # Return the class assignments
  return(list(Y = Y, U = U, V = V, clusterlabel = cbind(name, Y)))
}