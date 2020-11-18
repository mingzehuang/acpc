MyKmeans <- function(X, K, M = NULL, numIter = 100){
  
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
  if (K > nrow(X)) {
    stop(paste("Number of clusters K should not exceed number of data points n: K = ", K, "> X =", nrow(X)))
  } 
  
  # Check if M is initialized
  # If initialized, then check that the dimension of centers in M match dimensions of points in X 
  # If initialized, also check that number of cluster centers in M matches the number of clusters K
  if(missing(M) | is.null(M)) {    
    M = X[sample(nrow(X), K), , drop = F]
  } else if (ncol(M) != ncol(X)) {
    stop(paste("Dimensions of centers M should be equal to dimensions of data points X: ncol(M) = ", ncol(M), " != ", ncol(X), "= ncol(X)"))
  } else if (nrow(M) != K) {
    stop(paste("Number of cluster centers should be equal to number of clusters K: nrow(M) = ", nrow(M), "!=", K, " = K"))
  }
  
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = as.vector(MyKmeans_c(X, K, M, numIter))
  
  # Return the class assignments
  return(Y)
}