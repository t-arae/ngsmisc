
test_that("integer_breaks", {
  # Check interface
  expect_equal(names(formals(integer_breaks)), c("n", "..."))
  expect_equal(formals(integer_breaks)$n, 5)

  # Check output
  expect_no_message(integer_breaks())
  expect_true(is.function(integer_breaks()))

  f <- integer_breaks()
  expect_equal(f(c(0, 10)), c(0, 2, 4, 6, 8, 10))
  expect_equal(f(c(0, -10)), c(-10, -8, -6, -4, -2, 0))
  expect_equal(f(c(0, 1.5)), c(0, 1))
  expect_equal(f(c(0, 0, 1, 1)), c(0, 1))
  expect_equal(f(c(1)), c(1))
  expect_equal(f(c(0.5)), numeric())
})

