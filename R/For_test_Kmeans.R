# Function that implements K-means algorithm. The default number of maximal iterations is 100.
MyKmeansR <- function(X, K, M = NULL, numIter = 100){
  X = as.matrix(X)  #Covert X into matrix.
  n = nrow(X)       #Calculate rows of X as n.
  p = ncol(X)       #calculate columns of X as p.
  numIter = round(numIter)     #Round number of maximal iteration in case the input might not be integer.
  if(numIter < 1) {
    stop(paste("Maximal number of iterations", numIter, "< 1"))  #Warning error if maximal iteration less than 1.
  }
  # Check whether number of clusters K exceed number of data points n.
  else if(K > n) {
    stop(paste("number of clusters K should not exceed number of data points n, whereas K = ", K, "greater than rows of X =", n))
  } 
  # Check whether M is NULL or not. If NULL, initialize based on K randomly selected points from X. If not NULL, check for compatibility with X dimensions.
  else if(missing(M) | is.null(M)) {    #Check if initial centers M is missing or null.
    M = X[sample(n, K), , drop = F]  #Sample initial centers from X.
    print("Initial centers M sampled from data points X")
  }
  # Check whether dimension of centers equal to dimension of data points.
  else {
    M = as.matrix(M) #Set initial center as M.
  }
  if (ncol(M) != p) {
    stop(paste("dimensions of centers M should be equal to dimensions of data points X, whereas M contains", ncol(M), "columns and X contains", p, "columns"))
  } 
  # Check whether number of centers equal to number of clusters.
  else if (nrow(M) != K) {
    stop(paste("number of Cluster centers should be equal to number of clusters K, whereas M contains", nrow(M), "rows, not equal to K =", K))
  } else {
    center = M  #initial centers.
  }
  # Implement K-means algorithm. It should stop when either (i) the centroids don't change from one iteration to the next, or (ii) the maximal number of iterations was reached.
  for (i in 1:numIter) {    #Loop starts.
    #I mathematically break the square of Euclidean distance, throw X_i' * X, so we only have to compare -2*X_i' * mu_k + mu_k' * mu_k 
    ssqcenter = diag(tcrossprod(center))   #Calculate mu_k' * mu_k for each center k.
    distance_cross = tcrossprod (X, (2 * center))    #Calculate 2*X_i' * mu_k for each data point i and each center k.
    negdistance_ss = distance_cross - matrix(ssqcenter, n, K, byrow = T)  #Calculate 2*X_i' * mu_k - mu_k' * mu_k
    Y = max.col(negdistance_ss, "first")   #Maximizing negative square distance is equivalent to minimizing square distance.
    rep_Y = matrix(Y, K, n, byrow = T)   #Replicate cluster labels K times as matrix.
    rep_Y[rep_Y != row(rep_Y)] = 0     #Using matrix row index as identify data points in each cluster.
    rep_Y[rep_Y == row(rep_Y)] = 1
    n_cluster = tabulate(Y)        #Calculate numbers of data points in each cluster.
    if (length(n_cluster) != K) {
      print("Problematic centers, resample from data points X")  #If problematic initial centers make some cluster empty, resample centers from data points.
      center_new = X[sample(n, K), , drop = F]
    } else {
      center_new = rep_Y %*% X / n_cluster   #Recompute centers by averaging data points assigned into each cluster.
    }
    if (i == numIter) {
      print(paste("maximal number of iterations =", numIter, "is reached"))  #Point out maximal number of iterations has been reached.
    }
    if (identical(center_new, center)) {
      print("convergence criterion is met")   #Break loop if center points has been steady (no further change).
      break
    }
    center = center_new   #Renew centers.
  }
  # Return the vector of assignments Y
  return(Y)
}