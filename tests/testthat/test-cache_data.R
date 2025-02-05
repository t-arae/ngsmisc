
test_that("cache_data()", {
  # Check interface
  expect_equal(names(formals(cache_data)), c("fpath", "object", "fun_load", "fun_save"))
  expect_equal(formals(cache_data)$fun_load, quote(readRDS))
  expect_equal(
    formals(cache_data)$fun_save,
    quote(\(x, y) saveRDS(object = x, file = y))
  )

  # Check side-effect
  temp_wd <- tempdir(check = TRUE)
  outf <- fs::path(temp_wd, "iris_head.rds")
  expect_true(fs::dir_exists(fs::path_dir(outf)))
  if(fs::file_exists(outf)) fs::file_delete(outf)
  expect_false("iris_head" %in% ls())

  expect_no_error(cache_data(outf, iris |> head()))
  expect_true(fs::file_exists(outf))
  expect_true("iris_head" %in% ls(envir = .GlobalEnv))
  rm("iris_head", envir = .GlobalEnv)

  expect_false("iris_head" %in% ls(envir = .GlobalEnv))
  old <- fs::file_info(outf)$modification_time
  cache_data(outf, iris |> head())
  expect_equal(fs::file_info(outf)$modification_time, old)
  expect_true("iris_head" %in% ls(envir = .GlobalEnv))

  outf2 <- fs::path(temp_wd, "iris_head.csv")
  expect_no_error(
    cache_data(
      outf2,
      iris |> head(1),
      fun_load = read.csv,
      fun_save = \(x, y) write.csv(x, y)
    )
  )
  expect_equal(iris_head, data.frame(X = 1, iris[1,1:4], Species = "setosa"))
  rm("iris_head", envir = .GlobalEnv)
  cache_data(
    outf2,
    iris |> head(1),
    fun_load = read.csv
  )
  expect_equal(iris_head, data.frame(X = 1, iris[1,1:4], Species = "setosa"))

  fs::dir_delete(temp_wd)
})
