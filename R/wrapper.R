
acpc <- function(X, center, r = 2, W = NULL , eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 1, MaxIter_k = 1e+3){
  X = as.matrix(X) # Convert X into matrix.
  n = nrow(X) # Compute rows of X.
  p = ncol(X) # Compute columns of X.
  X = scale(X)* sqrt(n/(n-1))   # Center and scale X
  L = robustPCA(X, eps_r, MaxIter_r, gamma, tau) # Robust PCA
  spca = sparsePCA(L, eps_s, MaxIter_s, r, lambda) # Sparse PCA
  U = spca$U
  V = spca$V
  M = U[center, , drop = F]
  if (missing(W) | is.null(W)) {
    W = rep(1/r, r)
  }
  out = Kmeans(U, M, W, MaxIter_k)
  Y = out$Y
  center = out$center
  # Return the class assignments
  return(list(Y = Y, center= center, U = U, V = V, W = W))
}