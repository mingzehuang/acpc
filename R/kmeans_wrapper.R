#' Title
#'
#' @param X Data matrix with dimensions n by p
#' @param K Number of clusters  with K less than or equal to n (optional)
#' @param M Matrix of initial cluster centers with dimensions K by p (optional)
#' @param numIter Number of iterations of algorithm (default = 100)
#'
#' @return Returns vector Y of length n, which assigns each data point into a cluster
#' @export
#'
#' @examples
#' # Example on 2D-plane
#' # Generate features.
#' X = rbind(matrix(rnorm(200, sd = 1), ncol = 2), matrix(rnorm(200, mean = 6, sd = 1), ncol = 2))
#' colnames(X) = c("x", "y")
#' # Cluster observations into 2 groups.
#' clusters = MyKmeans(X, 2)
#' plot(X, col = clusters)
MyKmeans <- function(X, K, r, lambda_spca, eps = 0.0001, numIter = 100){
  U = sparsePCA(X, r_spca, lambda_spca, eps)$U
  # Warning error if maximal iteration less than 1.
  if(numIter < 1) {
    stop(paste("Maximal number of iterations: ", numIter, "< 1"))  
  } else if(K < 1) {
  # Warning if number of clusters is less than 1
    stop(paste("Number of clusters: ", numIter, "< 1"))
  } else if (K > nrow(U)) {
  # Check whether number of clusters K exceed number of data points n.
    stop(paste("Number of clusters K should not exceed number of data points n: K = ", K, "> X =", nrow(X)))
  } 
    M = U[sample(nrow(U), K), , drop = F]
  
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = as.vector(MyKmeans_c(U, K, M, numIter))
  
  # Return the class assignments
  return(Y)
}