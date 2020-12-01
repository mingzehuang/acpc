normalize <- function(X) {
  X = as.matrix(X) # Convert X into matrix.
  n = nrow(X) # Compute rows of X.
  p = ncol(X) # Compute columns of X.
  # Center and scale X
  Xmeans = colMeans(X)  # X^bar
  X_centered = X - matrix(Xmeans, n, p, byrow = T)  # X centered.
  weights = sqrt(colSums(X_centered^2) / n)  # Weights of X.
  Xtilde = X_centered / matrix(weights, n, p, byrow = T)  # X^tilde
  return (list(Xtilde = Xtilde, n = n, p = p))
}

acpc <- function(X_ext, X = NULL, Y = NULL, K, r_spca, lambda_spca, eta = 0.1, lambda_mlr = 1, eps = 0.0001, numIter = 100){
  if (missing(X) | is.null(X) | missing(Y) | is.null) {
    normalX_ext = normalize(X_ext) # Center and scale X_ext.  
    X_ext = normalX_ext$Xtilde # Center and scale X_ext.
    p_ext = normalX_ext$p # Compute column of X_ext.
    U_ext = sparsePCA(X_ext, r_spca, lambda_spca, eps)$U # Sparse PCA for X_ext.
    M = U_ext[sample(nrow(U), K), , drop = F] # Sample initial centers.
    Y_ext = MyKmeans_c(U_ext, K, M, numIter) 
  } else {
    n = nrow(X) # Compute rows of X.
    p = ncol(X) # Compute columns of X.
    X_ext = rbind(X_ext, X) # Stack X and X_ext together to extract common principle components.
    normalX_ext = normalize(X_ext) # Center and scale X_ext.  
    X_ext = normalX_ext$Xtilde # Center and scale X_ext.
    n_ext = normalX_ext$n # Compute rows of X_ext.
    p_ext = normalX_ext$p # Compute column of X_ext.
    U_ext = sparsePCA(X_ext, r_spca, lambda_spca, eps)$U # Sparse PCA for X_ext.
    Y = as.vector(Y)  # Convert Y into vector.
    n_Y = length(Y) # Compute numbers of elements of Y.
    Y_ext = LRMultiClass_c(U_ext[1:(n_ext - n), , drop = F], U_ext[(n_ext - n + 1):n_ext, , drop = F], Y, numIter, eta, lambda)
  }
  # Return the class assignments
  return(Y)
}