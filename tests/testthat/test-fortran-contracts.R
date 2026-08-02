## Defects where R and the compiled code disagreed about sizes or argument
## lists. Each of these crashed or returned a value that was not a class.

test_that("nndist2 returns, and computes the metric nndist computes", {
  # withmean2 declared a `which` mapping in position 8 that nndist2 never
  # passed, shifting every later argument: covw was reinterpreted as integer
  # subscripts and tmean got no argument at all. R aborted with a bus error.
  set.seed(11); n <- 80; p <- 3
  x <- matrix(rnorm(n * p), n, p)
  y <- rep(1:2, length.out = n)
  x[y == 2, 1] <- x[y == 2, 1] + 2
  x0 <- colMeans(x)

  for (km in c(20, 40, 60)) {
    # ktarget < kmetric so the shrinking loop actually runs one pass; with
    # ktarget >= kmetric the guard is false and nndist2 does nothing.
    got <- nndist2(x, y, x0, kmetric = km, ktarget = 5, fullw = TRUE, iter = 1)
    ref <- nndist(x, y, matrix(x0, 1), k = km, fullw = TRUE)
    expect_true(all(is.finite(got$covw)))
    expect_equal(as.numeric(got$covw), as.numeric(ref$covw))
    expect_equal(as.numeric(got$means), as.numeric(ref$means))
  }
})

test_that("knn is correct for k past the old fixed buffer sizes", {
  skip_if_not_installed("class")
  # pos[50] and nndist[100] were fixed-size while k was checked only against
  # the row count: k >= 101 returned wrong neighbours and a label of 0, and
  # k = 128 aborted R.
  set.seed(7); n <- 150
  x <- rbind(matrix(rnorm(n * 2, 0, 1), n, 2), matrix(rnorm(n * 2, 4, 1), n, 2))
  y <- c(rep(1L, n), rep(2L, n))
  i <- sample(2 * n); x <- x[i, ]; y <- y[i]
  xtr <- x[1:250, ]; ytr <- y[1:250]; xte <- x[251:300, ]

  for (kk in c(5, 49, 51, 101, 150, 249)) {
    got <- knn(xtr, xte, ytr, k = kk)
    expect_true(all(got %in% c(1L, 2L)), info = paste("k =", kk))
    ref <- class::knn(xtr, xte, factor(ytr), k = kk)
    expect_equal(as.character(got), as.character(ref), info = paste("k =", kk))
  }
})

test_that("knn survives a k that used to abort R", {
  set.seed(1)
  xtr <- rbind(matrix(runif(130 * 2, -1, 1), 130, 2),
               matrix(runif(70 * 2, 100, 101), 70, 2))
  ytr <- c(rep(1L, 130), rep(2L, 70))
  for (kk in 121:130) {
    expect_true(knn(xtr, matrix(0, 1, 2), ytr, k = kk) %in% c(1L, 2L))
  }
})

test_that("min2d locates the minimum, including in the last row", {
  # trunc(i / nrow) + 1 and i %% nrow returned row 0 whenever the minimum fell
  # in the last row, and put the column one too far right.
  expect_equal(min2d(matrix(c(5, 4, 3, 2, 1), 5, 1)), c(5, 1))
  expect_equal(min2d(matrix(16, 1, 1)), c(1, 1))
  expect_equal(min2d(matrix(c(9, 9, 1, 9, 9, 9), 3, 2)), c(3, 1))
  expect_equal(min2d(matrix(c(9, 9, 9, 9, 9, 1), 3, 2)), c(3, 2))
  expect_equal(min2d(matrix(c(1, 9, 9, 9), 2, 2)), c(1, 1))

  # And agrees with base R's own answer on random matrices.
  set.seed(3)
  for (i in 1:20) {
    nr <- sample(1:6, 1); nc <- sample(1:6, 1)
    m <- matrix(rnorm(nr * nc), nr, nc)
    expect_equal(min2d(m), as.vector(which(m == min(m), arr.ind = TRUE)[1, ]))
  }
})

test_that("dann's own default kmetric passes its own assertion", {
  # kmetric defaulted to max(50, 0.2 * n), a non-integer double for every n
  # that is not a multiple of 5, which assertCount() then rejected.
  for (n in c(250, 251, 252, 255, 260, 999)) {
    x <- matrix(rnorm(n * 2), n, 2)
    y <- rep(1:2, length.out = n)
    expect_no_error(dann(x, x[1:2, ], y, k = 5))
  }
})
