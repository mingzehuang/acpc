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
MyKmeans <- function(X, K, M = NULL, Vstart, lambda, eps, numIter = 100){
  Xv = sparsePCA(X, Vstart, lambda, eps = 0.0001)
  # Check whether M is NULL or not. If NULL, initialize based on K random points from X. If not NULL, check for compatibility with X dimensions.
  # Warning error if maximal iteration less than 1.
  if(numIter < 1) {
    stop(paste("Maximal number of iterations: ", numIter, "< 1"))  
  }
  
  # Warning if number of clusters is less than 1
  if(K < 1) {
    stop(paste("Number of clusters: ", numIter, "< 1"))
  }
  
  # Check whether number of clusters K exceed number of data points n.
  if (K > nrow(Xv)) {
    stop(paste("Number of clusters K should not exceed number of data points n: K = ", K, "> X =", nrow(X)))
  } 
  
  # Check if M is initialized
  # If initialized, then check that the dimension of centers in M match dimensions of points in X 
  # If initialized, also check that number of cluster centers in M matches the number of clusters K
  if(missing(M) | is.null(M)) {    
    M = Xv[sample(nrow(Xv), K), , drop = F]
  } else if (ncol(M) != ncol(Xv)) {
    stop(paste("Dimensions of centers M should be equal to dimensions of data points X: ncol(M) = ", ncol(M), " != ", ncol(X), "= ncol(X)"))
  } else if (nrow(M) != K) {
    stop(paste("Number of cluster centers should be equal to number of clusters K: nrow(M) = ", nrow(M), "!=", K, " = K"))
  }
  
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = as.vector(MyKmeans_c(Xv, K, M, numIter))
  
  # Return the class assignments
  return(Y)
}