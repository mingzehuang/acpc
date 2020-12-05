
acpc <- function(X, Y = NULL, K = NULL, r, lambda = 1, gamma = 0.1, tau = 1, W = NULL, eps = 0.0001, MaxIter = 100){
  X = as.matrix(X) # Convert X into matrix.
  n = nrow(X) # Compute rows of X.
  p = ncol(X) # Compute columns of X.
  if (missing(W) | is.null(W)) {
    W = rep(1/p, p)
  }
  X = scale(X)* sqrt(n/(n-1))   # Center and scale X
  rspca = robustsparsePCA(X, r, eps, MaxIter, lambda, gamma, tau) # Sparse PCA for X_ext.
  U = rspca$U
  V = rspca$V
  M = U[sample(nrow(U), K), , drop = F] # Sample initial centers.
  for (j in 1:K) {
    M[j, ] = colMeans(X[Y == j, , drop = F])  
  }
  out = Kmeans(U, K, M, W, MaxIter)
  Y = out$Y
  center = out$center
  # Return the class assignments
  return(list(Y = Y, center= center, U = U, V = V, W = W))
}