## Class labels are passed straight to Fortran, where they index means(class,p)
## and sumw(class) in withmean.f and votes[] in knn.c. Nothing mapped them to
## 1..K first, and nclass was only length(unique(y)), so any gap or offset ran
## out of bounds -- silently.
##
## With labels {1,3} the between-class matrix B came back identically zero, so
## DANN degenerated to Mahalanobis k-NN with no discriminant adaptation and no
## error; with {5,9} it did not terminate. Relabelling is a pure renaming, so
## every quantity below must be invariant to it.

make_two_class <- function(seed = 11, n = 80, p = 3) {
  set.seed(seed)
  x <- matrix(rnorm(n * p), n, p)
  y <- rep(1:2, length.out = n)
  x[y == 2, 1] <- x[y == 2, 1] + 2
  list(x = x, y = y)
}

test_that("the local metric does not depend on how the classes are numbered", {
  d <- make_two_class()
  x0 <- matrix(colMeans(d$x), 1)

  base <- nndist(d$x, d$y, x0, k = 30, fullw = TRUE)
  expect_true(any(base$means != 0))

  # Order-preserving renamings leave every row of means where it was.
  for (relabel in list(c(1L, 3L), c(5L, 9L), c(10L, 20L))) {
    got <- nndist(d$x, relabel[d$y], x0, k = 30, fullw = TRUE)
    expect_equal(got$means, base$means, info = paste(relabel, collapse = ","))
    expect_equal(got$covw, base$covw, info = paste(relabel, collapse = ","))
  }

  # Swapping the two names permutes the rows of means, since they are ordered
  # by class, and leaves the pooled within-class covariance alone.
  swapped <- nndist(d$x, c(2L, 1L)[d$y], x0, k = 30, fullw = TRUE)
  expect_equal(swapped$means, base$means[2:1, ])
  expect_equal(swapped$covw, base$covw)
})

test_that("dann predictions are invariant to relabelling, and keep the labels", {
  set.seed(7); n <- 120
  x <- rbind(matrix(rnorm(n * 2, 0, 1), n, 2), matrix(rnorm(n * 2, 2.5, 1), n, 2))
  y <- c(rep(1L, n), rep(2L, n))
  i <- sample(2 * n); x <- x[i, ]; y <- y[i]
  xtr <- x[1:200, ]; ytr <- y[1:200]; xte <- x[201:240, ]

  base <- dann(xtr, xte, ytr, k = 5, kmetric = 40)

  for (relabel in list(c(1L, 3L), c(5L, 9L))) {
    got <- dann(xtr, xte, relabel[ytr], k = 5, kmetric = 40)
    # Same decisions, expressed in the caller's own labels.
    expect_equal(as.vector(relabel[base]), as.vector(got))
    expect_true(all(got %in% relabel))
  }
})

test_that("knn agrees with class::knn regardless of label coding", {
  skip_if_not_installed("class")
  set.seed(7); n <- 150
  x <- rbind(matrix(rnorm(n * 2, 0, 1), n, 2), matrix(rnorm(n * 2, 4, 1), n, 2))
  y <- c(rep(1L, n), rep(2L, n))
  i <- sample(2 * n); x <- x[i, ]; y <- y[i]
  xtr <- x[1:250, ]; ytr <- y[1:250]; xte <- x[251:300, ]

  # 60 indexed past the end of the fixed votes[51] buffer.
  for (relabel in list(c(1L, 2L), c(1L, 60L), c(7L, 9L))) {
    got <- knn(xtr, xte, relabel[ytr], k = 5)
    expect_true(all(got %in% relabel))
    ref <- class::knn(xtr, xte, factor(relabel[ytr]), k = 5)
    expect_equal(as.character(got), as.character(ref))
  }
})
