test_that("Input checks work", {
  n = 100
  p = 40
  set.seed(1234)
  out <- svd(matrix(rnorm(n*p), n, p), nu = 3, nv = 3)
  trueL <- out$u %*% diag(out$d[1:3]) %*% t(out$v)
  trueS <- matrix(rt(n*p, df = 1), n, p)
  trueS[abs(trueS) < 2.5] <- 0
  sum(trueS !=0)
  M <- trueL + trueS
  svd(M)$d
  K = 5
  X <- cbind(data.frame(as.character(1:n), M))
  
  # Check tolerance
  eps_r = 0
  expect_error(acpc(X, K, r = 2, eps_r, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  eps_s = 0
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  # Check maximum iteration
  MaxIter_r = 9
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  MaxIter_s = 9
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s,  lambda = 0.1, MaxIter_k = 1e+3))
  MaxIter_k = 9  
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k))
  # Check parameters for robust PCA
  gamma = 0
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  tau = 0
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  # Check parameters for sparse PCA
  lambda = -1
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda, MaxIter_k = 1e+3))
  # Check feature ID
  colnames(X) = NULL
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  
  X <- cbind(data.frame(as.character(1:n), M))
  # Check numeric features
  X[n, p] = "text"
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  
  X <- cbind(data.frame(as.character(1:n), M))
  # Check number of clusters K
  K = 1
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  K = n + 1
  expect_error(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  
  K = 5
  # Check number of principle components r
  r = 0
  expect_error(acpc(X, K, r, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  r = p + 1
  expect_error(acpc(X, K, r, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3))
  
  # Check output dimensions for Y
  expect_equal(length(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$Y), n)
  expect_equal(length(names(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$Y)), n)
  X[n, p] = NA
  expect_equal(length(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$Y), n - 1)
  expect_equal(length(names(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$Y)), n - 1)  
  # Check output dimensions for U
  X <- cbind(data.frame(as.character(1:n), M))
  expect_equal(nrow(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$U), n)
  X[n, p] = NA
  expect_equal(nrow(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$U), n - 1)
  X <- cbind(data.frame(as.character(1:n), M))  
  r = 3
  expect_equal(ncol(acpc(X, K, r, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$U), r)
  
  # Check output dimensions for center
  expect_equal(nrow(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$center), K)
  expect_equal(ncol(acpc(X, K, r, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$center), r)
  
  # Check output dimensions for V
  expect_equal(nrow(acpc(X, K, r = 2, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$V), p)
  expect_equal(ncol(acpc(X, K, r, eps_r = 1e-4, MaxIter_r = 1e+5, gamma = 0.1, tau = 1, eps_s = 1e-4, MaxIter_s = 1e+3,  lambda = 0.1, MaxIter_k = 1e+3)$V), r)
  
})