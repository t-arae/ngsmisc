
set.seed(123)
rc <- rnbinom(n = 100, size = 5, prob = 0.3)
len <- sample.int(n = 1000, size= 100)

test_that("norm_count", {
  # Check interface
  expect_equal(names(formals(calc_rpm)), "readcount")
  expect_equal(names(formals(calc_rpkm)), c("readcount", "len"))
  expect_equal(names(formals(calc_tpm)), c("readcount", "len"))

  # Check output
  expect_true(all(is.numeric(calc_rpm(rc))))
  expect_equal(length(calc_rpm(rc)), length(rc))
  expect_true(is.nan(calc_rpm(0)))

  expect_true(all(is.numeric(calc_rpkm(rc, len))))
  expect_equal(length(calc_rpkm(rc, len)), length(rc))
  expect_true(is.nan(calc_rpkm(0, 1000)))

  expect_true(all(is.numeric(calc_tpm(rc, len))))
  expect_equal(length(calc_tpm(rc, len)), length(rc))
  expect_true(is.nan(calc_tpm(0, 1000)))
})

