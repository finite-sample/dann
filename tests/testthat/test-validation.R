test_that("knn rejects NA in train", {
  x <- matrix(c(1, NA, 3, 4), 2, 2)
  expect_error(knn(x, matrix(1:2, 1), c(1L, 2L), k = 1), "missing")
})

test_that("knn rejects NA in test", {
  train <- matrix(1:4, 2, 2)
  test <- matrix(c(1, NA), 1, 2)
  expect_error(knn(train, test, c(1L, 2L), k = 1), "missing")
})

test_that("knn rejects wrong dimension test", {
  train <- matrix(1:4, 2, 2)
  test <- matrix(1:3, 1, 3)
  expect_error(knn(train, test, c(1L, 2L), k = 1), "cols")
})

test_that("knn rejects k = 0", {
  train <- matrix(1:4, 2, 2)
  expect_error(knn(train, train, c(1L, 2L), k = 0), ">= 1")
})

test_that("knn rejects k > n", {
  train <- matrix(1:4, 2, 2)
  expect_error(knn(train, train, c(1L, 2L), k = 10), "exceed")
})

test_that("knn rejects mismatched cl length", {
  train <- matrix(1:4, 2, 2)
  expect_error(knn(train, train, c(1L, 2L, 3L), k = 1), "len")
})

test_that("dann rejects NA in x", {
  x <- matrix(c(1, NA, 3, 4, 5, 6), 3, 2)
  expect_error(dann(x, y = 1:3), "missing")
})

test_that("dann rejects single class", {
  x <- matrix(1:6, 3, 2)
  expect_error(dann(x, y = c(1L, 1L, 1L)), "at least 2 classes")
})

test_that("dann rejects k = 0", {
  x <- matrix(1:6, 3, 2)
  expect_error(dann(x, y = c(1L, 1L, 2L), k = 0), ">= 1")
})

test_that("dann rejects negative epsilon", {
  x <- matrix(1:6, 3, 2)
  expect_error(dann(x, y = c(1L, 1L, 2L), epsilon = -1), ">= 0")
})

test_that("dann rejects negative covmin", {
  x <- matrix(1:6, 3, 2)
  expect_error(dann(x, y = c(1L, 1L, 2L), covmin = -1), ">= 0")
})

test_that("stand rejects mismatched columns", {
  x <- matrix(1:6, 2, 3)
  xx <- matrix(1:4, 2, 2)
  expect_error(stand(x, xx), "cols")
})

test_that("knncv rejects NA", {
  x <- matrix(c(1, NA, 3, 4), 2, 2)
  expect_error(knncv(x, c(1L, 2L), k = 1), "missing")
})

test_that("knncv rejects k > n", {
  x <- matrix(1:4, 2, 2)
  expect_error(knncv(x, c(1L, 2L), k = 10), "exceed")
})
