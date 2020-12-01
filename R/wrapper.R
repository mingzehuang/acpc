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

#' Title
#'
#' @param X_ext 
#' @param X 
#' @param Y 
#' @param K 
#' @param r_spca 
#' @param lambda_spca 
#' @param eta 
#' @param lambda_mlr 
#' @param eps 
#' @param numIter 
#'
#' @return
#' @export
#'
#' @examples
acpc <- function(X_ext, X = NULL, Y = NULL, K = NULL, r_spca, lambda_spca = 0, eta = 0.1, lambda_mlr = 1, eps = 0.0001, numIter = 100){
  if (missing(X) | is.null(X) | missing(Y) | is.null) {
    normalX_ext = normalize(X_ext) # Center and scale X_ext.  
    X_ext = normalX_ext$Xtilde # Center and scale X_ext.
    p_ext = normalX_ext$p # Compute column of X_ext.
    spca_ext = sparsePCA(X_ext, r_spca, lambda_spca, eps) # Sparse PCA for X_ext.
    U_ext = spca_ext$U
    V = spca_ext$V
    M = U_ext[sample(nrow(U), K), , drop = F] # Sample initial centers.
    Y_ext = MyKmeans_c(U_ext, K, M, numIter) 
  } else {
    n = nrow(X) # Compute rows of X.
    p = ncol(X) # Compute columns of X.
    X_all = rbind(X_ext, X) # Stack X and X_ext together to extract common principle components.
    normalX_all = normalize(X_all) # Center and scale X_ext.  
    X_all = normalX_all$Xtilde # Center and scale X_ext.
    n_all = normalX_all$n # Compute rows of X_ext.
    p_all = normalX_all$p # Compute column of X_ext.
    spca_all = sparsePCA(X_all, r_spca, lambda_spca, eps) # Sparse PCA for X_all.
    U_all = spca_all$U
    V = spca_all$V
    U_ext = U_all[1:(n_all - n), , drop = F]
    U = U_all[(n_all - n + 1):n_ext, , drop = F]
    Y = as.vector(Y)  # Convert Y into vector.
    n_Y = length(Y) # Compute numbers of elements of Y.
    Y_ext = LRMultiClass_c(U_ext, U, Y, numIter, eta, lambda)
  }
  # Return the class assignments
  return(list(Y_ext = Y_ext, U_ext = U_ext, V))
}