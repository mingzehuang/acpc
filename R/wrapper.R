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
#' @param eps 
#' @param numIter 
#'
#' @return
#' @export
#'
#' @examples
acpc <- function(X_ext, X = NULL, Y = NULL, K = NULL, r_spca, lambda_spca = 0, eps = 0.0001, numIter = 100){
    normalX_ext = normalize(X_ext) # Center and scale X_ext.  
    X_ext = normalX_ext$Xtilde # Center and scale X_ext.
    p_ext = normalX_ext$p # Compute column of X_ext.
    spca_ext = sparsePCA(X_ext, r_spca, lambda_spca, eps) # Sparse PCA for X_ext.
    U_ext = spca_ext$U
    V = spca_ext$V
    M = U_ext[sample(nrow(U), K), , drop = F] # Sample initial centers.
    Y_ext = MyKmeans_c(U_ext, K, M, numIter) 
  # Return the class assignments
  return(list(Y_ext = Y_ext, U_ext = U_ext, V))
}