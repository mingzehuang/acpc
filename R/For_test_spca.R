# Solve procrustes for given X and V
procrustesR <- function(X, V) {
  svdXV <- svd(X %*% V)
  U <- tcrossprod(svdXV$u, svdXV$v)
  return(U)
}
sparsePCAobj <- function(X, U, V, lambda){
  sum((X - tcrossprod(U, V))^2) / 2 + lambda * sum(abs(V))
}
softA <- function(A, lambda){
  sign(A) * pmax(abs(A) - lambda, 0)
}

sparsePCAR <-function(X, Vstart, lambda, tol){
  # Evaluate U for given Vstart, and the value of objective
  Ustart <- procrustesR(X, Vstart)
  obj <- sparsePCAobj(X, Ustart, Vstart, lambda)
  # While not converged, repeat
  error = 100
  while(error >= tol) {
    ## Update V via soft-thresholding of X'U
    Vnew <- softA(crossprod(X, Ustart), lambda)
    ## Update U via Procrustes# Return a list of U, V and error on solution
    Unew <- procrustesR(X, Vnew)
    ## Evaluate new objective value
    objnew <- sparsePCAobj(X, Unew, Vnew, lambda)
    ## Evaluate error
    error = abs(objnew - obj)
    obj = objnew
    Ustart = Unew
    print(obj)
  }
  return(list(U = Unew, V = Vnew, error = error))}
