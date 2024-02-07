tbl <-
  data.frame(
    "dir_log/sample1.log" = 1,
    "dir_fastq/sample1.fastq" = 1,
    check.names = FALSE
  )

test_that("rename_nth_col()", {
  # Check interface
  expect_equal(names(formals(rename_nth_col)), c("tbl", "nth", "renamer"))

  # Check output
  expect_no_message(rename_nth_col(tbl, 1, function(x) x))
  expect_no_message(rename_nth_col(tbl, 1:2, function(x) x))

  expect_error(rename_nth_col(tbl, NULL, function(x) x))
  expect_error(rename_nth_col(tbl, 3, function(x) x))
  expect_error(rename_nth_col(tbl, "1", function(x) x))
  expect_error(rename_nth_col(tbl, 1, iris))

  expect_equal(rename_nth_col(tbl, 1, function(x) x), tbl)
  expect_equal(colnames(rename_nth_col(tbl, 1, basename)),
               c("sample1.log", "dir_fastq/sample1.fastq"))
  expect_equal(colnames(rename_nth_col(tbl, 1:2, basename)),
               c("sample1.log", "sample1.fastq"))
  expect_equal(colnames(rename_nth_col(tbl, -1, basename)),
               c("dir_log/sample1.log", "sample1.fastq"))
})

test_that("renamer_fpath()", {
  # Check interface
  expect_equal(names(formals(renamer_fpath)), c("prefix", "suffix"))
  expect_equal(formals(renamer_fpath)[["prefix"]], "")
  expect_equal(formals(renamer_fpath)[["suffix"]], "")

  # Check output
  expect_no_message(renamer_fpath())
  expect_no_message(renamer_fpath()("test"))

  expect_error(renamer_fpath(prefix = NULL)("test"))
  expect_error(renamer_fpath(prefix = NA)("test"))
  expect_error(renamer_fpath(prefix = 1)("test"))
  expect_error(renamer_fpath(suffix = NULL)("test"))
  expect_error(renamer_fpath(suffix = NA)("test"))
  expect_error(renamer_fpath(suffix = 1)("test"))

  expect_true(is.function(renamer_fpath()))
  expect_equal(renamer_fpath()("path/to/file.txt"), "file.txt")
  expect_equal(renamer_fpath(suffix = ".txt")("path/to/file.txt"), "file")
  expect_equal(renamer_fpath(prefix = "file")("path/to/file.txt"), ".txt")

  input <- c("path/to/file.txt", "path/to/to/to/picture.jpeg",
             "path/to/something")
  expect_equal(renamer_fpath(suffix = "[.].*$")(input),
               c("file", "picture", "something"))
})

tbl <-
  data.frame(
    "dir_log/sample1.log" = 1,
    "dir_fastq/sample1.fastq" = 1,
    "dir_fastq/sample1.fq" = 1,
    "dir_fastq/sample1.fastq.gz" = 1,
    "dir_fastq/sample1.fq.gz" = 1,
    "dir_bam/sample1.bam" = 1,
    "dir_bam/sample1.sort.bam" = 1,
    check.names = FALSE
  )

test_that("rename_fpath", {
  # Check interface
  expect_equal(names(formals(rename_fpath)), c("tbl", "nth", "prefix", "suffix"))
  expect_equal(formals(rename_fpath)[["prefix"]], "")
  expect_equal(formals(rename_fpath)[["suffix"]], "")
  expect_equal(names(formals(rename_fpath_fastq)), c("tbl", "nth", "prefix"))
  expect_equal(formals(rename_fpath_fastq)[["prefix"]], "")
  expect_equal(names(formals(rename_fpath_bam)), c("tbl", "nth", "prefix"))
  expect_equal(formals(rename_fpath_bam)[["prefix"]], "")

  # Check output
  expect_no_message(rename_fpath(tbl, nth = 1))
  expect_no_message(rename_fpath(tbl, nth = 1:7))

  expect_equal(
    names(rename_fpath(tbl, 1:7)),
    paste0("sample1.", c("log", "fastq", "fq", "fastq.gz", "fq.gz", "bam", "sort.bam"))
  )

  renamed <-
    rename_fpath(tbl, 1, suffix = ".log") %>%
    rename_fpath_fastq(2:5) %>%
    rename_fpath_bam(6:7)
  expect_true(all(names(renamed) == "sample1"))
})


test_that("rename_col_fpath", {
  lifecycle::expect_deprecated(rename_col_fpath(tbl, 1, FALSE))
})

test_that("rename_col_AGI", {
  lifecycle::expect_deprecated(rename_col_AGI(tbl, 1))
})
