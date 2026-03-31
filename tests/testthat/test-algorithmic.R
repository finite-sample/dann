test_that("DANN handles classes with different covariance structures", {
  set.seed(42)
  n <- 100

  # Class 1: elongated horizontally (high variance in x1, low in x2)
  class1_x1 <- rnorm(n, mean = 0, sd = 2)
  class1_x2 <- rnorm(n, mean = 0, sd = 0.3)

  # Class 2: elongated vertically (low variance in x1, high in x2)
  class2_x1 <- rnorm(n, mean = 0, sd = 0.3)
  class2_x2 <- rnorm(n, mean = 0, sd = 2)

  x <- rbind(cbind(class1_x1, class1_x2), cbind(class2_x1, class2_x2))
  y <- c(rep(1L, n), rep(2L, n))

  idx <- sample(2 * n)
  x <- x[idx, ]
  y <- y[idx]

  train_idx <- 1:160
  test_idx <- 161:200

  # DANN should learn to use local covariance structure
  dann_pred <- dann(x[train_idx, ], x[test_idx, ], y[train_idx],
                    k = 5, kmetric = 30, fullw = TRUE, iter = 1)

  dann_acc <- mean(dann_pred == y[test_idx])
  expect_gt(dann_acc, 0.6)
})

test_that("DANN down-weights irrelevant features", {
  set.seed(123)
  n <- 150

  # 2 informative features: class 1 centered at (0,0), class 2 at (2,2)
  x_informative <- rbind(
    matrix(rnorm(n * 2, mean = 0, sd = 0.5), n, 2),
    matrix(rnorm(n * 2, mean = 2, sd = 0.5), n, 2)
  )

  # 8 noise features: pure random, no class information
  x_noise <- matrix(rnorm(2 * n * 8), 2 * n, 8)

  x <- cbind(x_informative, x_noise)
  y <- c(rep(1L, n), rep(2L, n))

  idx <- sample(2 * n)
  x <- x[idx, ]
  y <- y[idx]

  train_idx <- 1:240
  test_idx <- 241:300

  # DANN should achieve good accuracy by focusing on informative features
  dann_pred <- dann(x[train_idx, ], x[test_idx, ], y[train_idx],
                    k = 5, kmetric = 40, fullw = TRUE, iter = 1)

  dann_acc <- mean(dann_pred == y[test_idx])
  expect_gt(dann_acc, 0.75)
})

test_that("epsilon parameter controls degree of adaptation", {
  set.seed(456)
  n <- 100

  # Simple well-separated classes
  x <- rbind(
    matrix(rnorm(n * 2, mean = 0, sd = 1), n, 2),
    matrix(rnorm(n * 2, mean = 3, sd = 1), n, 2)
  )
  y <- c(rep(1L, n), rep(2L, n))

  idx <- sample(2 * n)
  x <- x[idx, ]
  y <- y[idx]

  train_idx <- 1:160
  test_idx <- 161:200

  # Small epsilon: less adaptation (closer to standard kNN metric)
  pred_small_eps <- dann(x[train_idx, ], x[test_idx, ], y[train_idx],
                         k = 5, kmetric = 30, epsilon = 0.1, iter = 1)

  # Large epsilon: more adaptation
  pred_large_eps <- dann(x[train_idx, ], x[test_idx, ], y[train_idx],
                         k = 5, kmetric = 30, epsilon = 10, iter = 1)

  # Both should produce valid class predictions
  expect_true(all(pred_small_eps %in% 1:2))
  expect_true(all(pred_large_eps %in% 1:2))

  # Both should achieve reasonable accuracy on well-separated data
  acc_small <- mean(pred_small_eps == y[test_idx])
  acc_large <- mean(pred_large_eps == y[test_idx])
  expect_gt(acc_small, 0.7)
  expect_gt(acc_large, 0.7)
})

test_that("multiple epsilon values return predictions for each", {
  set.seed(789)
  n <- 50

  x <- rbind(
    matrix(rnorm(n * 2, mean = 0, sd = 1), n, 2),
    matrix(rnorm(n * 2, mean = 3, sd = 1), n, 2)
  )
  y <- c(rep(1L, n), rep(2L, n))

  train_idx <- 1:80
  test_idx <- 81:100

  # Multiple epsilon values
  epsilons <- c(0.5, 1.0, 2.0)
  result <- dann(x[train_idx, ], x[test_idx, ], y[train_idx],
                 k = 5, kmetric = 20, epsilon = epsilons, iter = 1)

  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 20)
})

test_that("cv=TRUE returns predictions for all training points", {
  set.seed(101)
  n <- 50

  x <- rbind(
    matrix(rnorm(n * 2, mean = 0, sd = 1), n, 2),
    matrix(rnorm(n * 2, mean = 3, sd = 1), n, 2)
  )
  y <- c(rep(1L, n), rep(2L, n))

  result <- dann(x, y = y, k = 5, kmetric = 20, cv = TRUE, iter = 1)

  expect_equal(nrow(result), 2 * n)
  expect_true(all(result %in% 1:2))
})

test_that("knn achieves perfect accuracy on perfectly separable data", {
  set.seed(111)
  n <- 50

  # Completely non-overlapping classes
  x <- rbind(
    matrix(rnorm(n * 2, mean = 0, sd = 0.5), n, 2),
    matrix(rnorm(n * 2, mean = 10, sd = 0.5), n, 2)
  )
  y <- c(rep(1L, n), rep(2L, n))

  train_idx <- 1:80
  test_idx <- 81:100

  result <- knn(x[train_idx, ], x[test_idx, ], y[train_idx], k = 3)

  acc <- mean(result == y[test_idx])
  expect_equal(acc, 1.0)
})

test_that("nndist returns covariance matrix with correct dimensions", {
  set.seed(222)
  n <- 60
  p <- 4

  x <- matrix(rnorm(n * p), n, p)
  y <- c(rep(1L, 30), rep(2L, 30))
  x0 <- matrix(colMeans(x), nrow = 1)

  result <- nndist(x, y, x0, k = 20, epsilon = 1, fullw = TRUE)

  expect_equal(dim(result$covw), c(p, p))
  expect_length(result$dist, n)
  expect_length(result$which, n)
})
