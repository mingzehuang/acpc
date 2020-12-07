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
})