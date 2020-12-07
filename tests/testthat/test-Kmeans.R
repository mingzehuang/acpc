test_that("Input checks work", {
  set.seed(NULL)
  # Three clusters; set the initial starting points to avoid randomness
  X = rbind(matrix(rnorm(100, 6, 3), ncol = 2), matrix(rnorm(100, -6, 3), ncol = 2), 
            matrix(rnorm(100, 20 ,3), ncol = 2))
  M = rbind(X[1, ], X[2, ], X[3, ])
  K = 3
  expect_equal(MyKmeansR(X, K, M, 1000) - 1, c(Kmeans(X, M, 1000)$Y))
})