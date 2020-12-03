normalize <- function(X) {

  return (list(Xtilde = Xtilde, n = n, p = p))
}

#' Title
#'
#' @param X 
#' @param Y 
#' @param K 
#' @param r 
#' @param lambda 
#' @param eps 
#' @param numIter 
#'
#' @return
#' @export
#'
#' @examples
acpc <- function(X, Y = NULL, K = NULL, r, lambda = 0, W = NULL, eps = 0.0001, numIter = 100){
  X = as.matrix(X) # Convert X into matrix.
  n = nrow(X) # Compute rows of X.
  p = ncol(X) # Compute columns of X.
  if (missing(W) | is.null(W)) {
    W = rep(1/p, p)
  }
  # Center and scale X
  Xmeans = colMeans(X)  # X^bar
  X_centered = X - matrix(Xmeans, n, p, byrow = T)  # X centered.
  weights = sqrt(colSums(X_centered^2) / n)  # Weights of X.
  Xtilde = X_centered / matrix(weights, n, p, byrow = T)  # X^tilde
  spca = sparsePCA(Xtilde, r, lambda, eps) # Sparse PCA for X_ext.
  U = spca$U
  V = spca$V
  M = U[sample(nrow(U), K), , drop = F] # Sample initial centers.
  for (j in 1:K) {
    M[j, ] = colMeans(X[Y == j, , drop = F])  
  }
  out = MyKmeans_c(U, K, M, W, numIter)
  Y = out$Y
  center = out$center
  # Return the class assignments
  return(list(Y = Y, center= center, U = U, V = V, W = W))
}