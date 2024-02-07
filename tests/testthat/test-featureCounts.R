
fC_count_fpath <-
  system.file(package = "ngsmisc", "featurecounts") %>%
  fs::dir_ls(regexp = "counts.txt$")

fC_summary_fpath <-
  system.file(package = "ngsmisc", "featurecounts") %>%
  fs::dir_ls(regexp = "gene_counts.txt.summary$")

test_that("fC_read_count()", {
  # Check interface
  expect_equal(names(formals(fC_read_count)), "fpath")

  # Check output
  expect_no_message(fC_read_count(fC_count_fpath[1]))

  tbl <- fC_read_count(fC_count_fpath[1])
  expect_equal(class(tbl), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(tbl), c(133L, 7L))
  expect_equal(
    tbl,
    readr::read_tsv(file = fC_count_fpath[1], skip = 1L, col_types = "cccccii")
  )
  expect_equal(colnames(tbl),
               c("Geneid", "Chr", "Start", "End", "Strand", "Length",
                 "some/sample_1.sort.bam"))

  # test the checking of file header
  tempf <- tempfile("temp.txt")
  file.copy(fC_count_fpath[1], tempf)
  readLines(tempf) %>%
    sub(pattern = "^# Program:featureCounts v", replacement = "#Program:featureCounts v") %>%
    writeLines(tempf)
  expect_error(fC_read_count(tempf))
  file.remove(tempf)
})

test_that("fC_merge_count()", {
  # Check interface
  expect_equal(names(formals(fC_merge_count)), "li_tbl")

  # Check output
  expect_no_message(fC_merge_count(lapply(fC_count_fpath, fC_read_count)))
  li_tbl <- lapply(fC_count_fpath, fC_read_count)
  li_tbl[[3]]$Strand[1] <- ""
  expect_error(fC_merge_count(li_tbl))

  tbl_merged <- fC_merge_count(lapply(fC_count_fpath, fC_read_count))
  expect_equal(class(tbl_merged), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(tbl_merged), c(133L, 9L))
})

test_that("fC_write_count()", {
  # Check interface
  expect_equal(names(formals(fC_write_count)), c("tbl_fC", "fpath"))

  # Check output
  tbl <- fC_read_count(fC_count_fpath[1])
  tempf <- tempfile("temp.txt")
  expect_no_message(fC_write_count(tbl, tempf))
  expect_error(fC_write_count(tbl))

  expect_equal(readr::read_csv(tempf, col_types = "ci"), tbl[,c(1,7)])
  file.remove(tempf)
})

test_that("fC_read_summary()", {
  # Check interface
  expect_equal(names(formals(fC_read_summary)), "fpath")

  # Check output
  expect_no_message(fC_read_summary(fC_summary_fpath[1]))

  tbl <- fC_read_summary(fC_summary_fpath[1])
  expect_equal(class(tbl), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(tbl), c(14L, 2L))
  expect_equal(
    tbl,
    readr::read_tsv(file = fC_summary_fpath[1], col_types = "ci")
  )
})

test_that("fC_rename_col()", {
  lifecycle::expect_deprecated(
    fC_rename_col(fC_read_count(fC_count_fpath[1]))
  )
})

test_that("fC_merge_summary()", {
  # Check interface
  expect_equal(names(formals(fC_merge_summary)), "li_tbl")

  # Check output
  expect_no_message(fC_merge_summary(lapply(fC_summary_fpath, fC_read_summary)))
  li_tbl <- lapply(fC_summary_fpath, fC_read_summary)
  li_tbl[[3]]$Status[1] <- ""
  expect_error(fC_merge_summary(li_tbl))

  tbl_merged <- fC_merge_summary(lapply(fC_summary_fpath, fC_read_summary))
  expect_equal(class(tbl_merged), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(tbl_merged), c(14L, 4L))
})

tbl_merged <- fC_merge_count(lapply(fC_count_fpath, fC_read_count))
test_that("fC_calc_rpm()", {
  # Check interface
  expect_equal(names(formals(fC_calc_rpm)), "tbl_fC")

  # Check output
  expect_no_message(fC_calc_rpm(tbl_merged))

  expect_equal(
    ncol(fC_calc_rpm(tbl_merged)),
    ncol(tbl_merged) + 3L
  )
  expect_true(
    colnames(fC_calc_rpm(tbl_merged))[-(1:6)] %>%
      sub(pattern = "^rpm_", replacement = "") %>%
      table() %>%
      {all(. == 2L)}
  )
})

test_that("fC_calc_rpkm()", {
  # Check interface
  expect_equal(names(formals(fC_calc_rpkm)), "tbl_fC")

  # Check output
  expect_no_message(fC_calc_rpkm(tbl_merged))

  expect_equal(
    ncol(fC_calc_rpkm(tbl_merged)),
    ncol(tbl_merged) + 3L
  )
  expect_true(
    colnames(fC_calc_rpkm(tbl_merged))[-(1:6)] %>%
      sub(pattern = "^rpkm_", replacement = "") %>%
      table() %>%
      {all(. == 2L)}
  )
})

test_that("fC_calc_tpm()", {
  # Check interface
  expect_equal(names(formals(fC_calc_tpm)), "tbl_fC")

  # Check output
  expect_no_message(fC_calc_tpm(tbl_merged))

  expect_equal(
    ncol(fC_calc_tpm(tbl_merged)),
    ncol(tbl_merged) + 3L
  )
  expect_true(
    colnames(fC_calc_tpm(tbl_merged))[-(1:6)] %>%
      sub(pattern = "^tpm_", replacement = "") %>%
      table() %>%
      {all(. == 2L)}
  )
})

test_that("all calc", {
  expect_true(
    tbl_merged %>%
      fC_calc_rpm() %>%
      fC_calc_rpkm() %>%
      fC_calc_tpm() %>%
      {colnames(.)[-(1:6)]} %>%
      sub(pattern = "^(rpm|rpkm|tpm)_", replacement = "") %>%
      table() %>%
      {all(. == 4L)}
  )
})
