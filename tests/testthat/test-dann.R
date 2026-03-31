test_that("knn works correctly", {
  set.seed(42)
  # Generate two well-separated clusters
  x1 <- matrix(rnorm(50 * 2, mean = 0, sd = 1), 50, 2)
  x2 <- matrix(rnorm(50 * 2, mean = 3, sd = 1), 50, 2)
  x <- rbind(x1, x2)
  y <- c(rep(1L, 50), rep(2L, 50))

  # Shuffle
  idx <- sample(100)
  x <- x[idx, ]
  y <- y[idx]

  result <- knn(x[1:80, ], x[81:100, ], y[1:80], k = 3)
  expect_length(result, 20)
  expect_true(all(result %in% 1:2))
  # With well-separated clusters, accuracy should be high
  accuracy <- sum(result == y[81:100]) / 20
  expect_gt(accuracy, 0.9)
})

test_that("dann works with scalar covariance", {
  set.seed(42)
  x1 <- matrix(rnorm(50 * 2, mean = 0, sd = 1), 50, 2)
  x2 <- matrix(rnorm(50 * 2, mean = 3, sd = 1), 50, 2)
  x <- rbind(x1, x2)
  y <- c(rep(1L, 50), rep(2L, 50))

  idx <- sample(100)
  x <- x[idx, ]
  y <- y[idx]

  result <- dann(x[1:80, ], x[81:100, ], y[1:80], k = 3, kmetric = 20, scalar = TRUE, iter = 1)
  expect_equal(nrow(result), 20)
  expect_true(all(result %in% 1:2))
  accuracy <- sum(result == y[81:100]) / 20
  expect_gt(accuracy, 0.9)
})

test_that("dann works with full covariance (triggers rs eigenvalue)", {
  set.seed(123)
  x1 <- matrix(rnorm(30 * 3, mean = 0, sd = 1), 30, 3)
  x2 <- matrix(rnorm(30 * 3, mean = 3, sd = 1), 30, 3)
  x <- rbind(x1, x2)
  y <- c(rep(1L, 30), rep(2L, 30))

  idx <- sample(60)
  x <- x[idx, ]
  y <- y[idx]

  result <- dann(x[1:50, ], x[51:60, ], y[1:50], k = 5, kmetric = 15,
                 fullw = TRUE, scalar = FALSE, iter = 1)
  expect_equal(nrow(result), 10)
  expect_true(all(result %in% 1:2))
})

test_that("knncv works", {
  set.seed(101)
  x1 <- matrix(rnorm(25 * 2, mean = 0, sd = 1), 25, 2)
  x2 <- matrix(rnorm(25 * 2, mean = 3, sd = 1), 25, 2)
  x <- rbind(x1, x2)
  y <- c(rep(1L, 25), rep(2L, 25))

  result <- knncv(x, y, k = 5)
  expect_length(result, 50)
  expect_true(all(result %in% 1:2))
})
