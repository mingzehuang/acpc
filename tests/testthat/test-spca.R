test_that("Input checks work", {
  set.seed(1234)
  X <-matrix(rnorm(110), 11, 5)
  #V <-matrix(rnorm(30), 5, 3)
  V <-matrix(0, 5, 3)
  r = 3
  lambda = 1
  eps = 1e-15
  expect_equal(sparsePCA(X, eps, 1e+6, r, lambda)$U, sparsePCAR(X, V, lambda, eps)$U)
  expect_equal(sparsePCA(X, eps, 1e+6, r, lambda)$V, sparsePCAR(X, V, lambda, eps)$V)
})