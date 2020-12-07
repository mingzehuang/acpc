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
  X = scale(M)* sqrt(n/(n-1))
  expect_equal(robustPCA(X, 0.001, 1e+6, 0.1, 1), robustPCAadmm(X, gamma = 0.1, Sinit = NULL, etainit = NULL, tau = 1, eps = 0.001)$L)
})