LRMultiClass <- function(X, y, numIter = 50, eta = 0.1, lambda = 1, lambda_spca, eps = 0.0001){
  U = sparsePCA(X, r, lambda_spca, eps)$U
  y = as.vector(y)  # Convert y into vector.
  numIter = as.numeric(numIter)  # Convert numIter into number.
  eta = as.numeric(eta)  # Convert eta into number.
  lambda = as.numeric(lambda)  # Convert lambda into number.
  n = nrow(U)  # Calculate rows of X as n.
  p = ncol(U)  # Calculate columns of X as p.
  ny = length(y)  # Calculate length of y as ny.
  class_y = sort(unique(y)) # Find unique values in y as classes.
  K = length(class_y)  # Calculate classes in training data as K.
  
  # Compatibility checks from HW3 and initialization of beta_init
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if (!(identical(U[ , 1], rep(1, n)))) {
    stop("The first column of X should be 1s to include intercepts.")
  } 
  # Check for compatibility of dimensions between X and Y
  if (n != ny) {
    stop(paste("X has", n, "rows but y has", ny, "elements."))
  } 
  # Check eta is positive
  if (eta <= 0) {
    stop(paste("eta should be positive, whereas eta =", eta, "is not greater than 0."))
  } 
  # Check lambda is non-negative
  if (lambda < 0) {
    stop(paste("lambda should be non-negative, whereas lambda =", lambda, "is less than 0."))
  } 
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
    beta_init = matrix(0, p, K)
  
  # Call C++ LRMultiClass_c function to implement the algorithm
  out = LRMultiClass_c(U, y, beta_init, numIter, eta, lambda)
  
  # Return the class assignments
  return(out)
}