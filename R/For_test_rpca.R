# Matrix soft-thresholding (applying soft-thresholding to each element of a matrix)
# INPUT
# A - n x p matrix
# lambda - non-negative thresholding parameter
# OUTPUT
# n x p matrix, each element is soft-thresholded element of A matrix
soft <- function(A, lambda){
  # Fill in
  soft_A = abs(A) - lambda
  soft_A[soft_A < 0] = 0
  return(sign(A) * soft_A)
}

# Proximal operator for nuclear norm (matrix with soft-thresholded singular values)
# INPUT
# A - n x p matrix
# lambda - non-negative thresholding parameter
# OUTPUT
# A list with
#  newA - n x p matrix, same singular vectors as A, but singular values soft-thresholded using lambda
#  newd - a vector of soft-thresholded singular values, that is singular values of newA (returning it here saves one SVD step later)
soft_nuclear <- function(A, lambda){
  # Fill in
  svd_A = La.svd(A) # Singular value decomposition.
  newd = soft(svd_A$d, lambda) # Soft-thresholding on singular values.
  newA = svd_A$u %*% (newd * svd_A$vt) # Compute new matrix with soft-tresholded singular values.
  # Returns a list
  # newd - soft-thresholded singular values, vector
  # newA - n x p matrix with singular values newd
  return(list(newd = newd, newA = newA))
}

# Objective function value of robustPCA, |L|_* + gamma|S|_1,
# INPUT
# Ld - a vector of singular values of L (this avoids recalculating svd more than needed)
# S - a n by p matrix, current value of S
# gamma - a positive scalar, current value of gamma
# OUTPUT
# A scalar corresponding to |L|_* + gamma|S|_1
robustPCAobj <- function(Ld, S, gamma = 0.1){
  # Fill in
  return(sum(Ld) + gamma * sum(abs(S)))
}


# Scaled ADMM algorithm for Robust PCA
# INPUT
# M - a n x p matrix
# gamma - positive scalar, default value 0.1
# Sinit - a n x p matrix, starting value for S. If none is supplied, initialize with matrix of zeros. If supplied, check for compatibility of dimensions with M.
# etainit - a n x p matrix, starting value of eta. If none is supplied, initialize with matrix of zeros. If supplied, check for compatibility of dimensions with M.
# tau - a positive scalar, ADMM parameter for scaled ADMM version, default value is 1.
# eps - a positive scalar, convergence tolerance criteria (difference in objective function values), default value is 0.001
# OUTPUT
# A list with
#   L - value of matrix L at convergence
#   S - value of matrix S at congergence
#   eta - value of matrix eta at convergence
robustPCAadmm <- function(M, gamma = 0.1, Sinit = NULL, etainit = NULL, tau = 1, eps = 0.001){
  # Check for compatibility between supplied Sinit, etainit and M. If any is NULL, initialize the corresponding matrix as a matrix of zeros.
  M = as.matrix(M)
  n = nrow(M) # Compute rows of M.
  p = ncol(M) # Compute columns of M.
  if (missing(Sinit) | is.null(Sinit)) {
    print(paste("Missing Sinit, initialized by matrix of zeros."))
    S = matrix(0, n, p) # Initialize S by zeros if missing.
  } else {
    Sinit = as.matrix(Sinit)
    n_Sinit = nrow(Sinit) # Compute rows of Sinit.
    p_Sinit = ncol(Sinit) # Compute columns of Sinit.
    if (n_Sinit != n | p_Sinit != p) {
      print(paste("Dimension of Sinit mismatches to M, whereas Sinit has", n_Sinit, "rows and", p_Sinit, "columns; but M has", n, "rows and", p, "columns; initialized by matrix of zeros."))
      S = matrix(0, n, p) # Initialize S by zeros if dimension mismatch.
    } else {
      S = Sinit # Initialize S by Sinit if it's proper.
    }
  }  
  if (missing(etainit) | is.null(etainit)) {
    print(paste("Missing etainit, initialized by matrix of zeros."))
    eta = matrix(0, n, p) # Initialize eta by zeros if missing.
  } else {
    etainit = as.matrix(etainit)
    n_etainit = nrow(etainit) # Compute rows of etainit.
    p_etainit = ncol(etainit) # Compute columns of etainit.
    if (n_etainit != n | p_etainit != p) {
      print(paste("Dimension of etainit mismatches to M, whereas etainit has", n_etainit, "rows and", p_etainit, "columns; but M has", n, "rows and", p, "columns; initialized by matrix of zeros."))
      eta = matrix(0, n, p) # Initialize eta by zeros if dimension mismatch.
    } else {
      eta = etainit # Initialize eta by etainit if it's proper.
    }
  } 
  gammatau = gamma * tau
  # Initialize L as L = M - S
  L = M - S
  obj_old = Inf # Initialize obj_old.
  ## Implement ADMM algorithm, which alternates updates of L, S and eta until convergence (first change in objective function values less than eps)
  repeat {
    sn = soft_nuclear(M - S - eta, tau) # Update L by proximal of nuclear norm.
    L = sn$newA
    S = soft(M - L - eta, gammatau) # Update S by soft-thresholding.
    eta = eta + S + L - M # Update eta.
    obj = robustPCAobj(sn$newd, S, gamma) # Compute objective function.
    if (abs(obj - obj_old) < eps) {
      print("Convergence criteria has been reached.")
      break
    } else {
      obj_old = obj # Update obj_old.
    }
  }
  # Return L, S and eta at convergence
  return(list(L = L, S = S, eta = eta))
}