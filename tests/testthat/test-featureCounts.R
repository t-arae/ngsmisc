
fC_count_fpath <-
  system.file(package = "ngsmisc") %>%
  fs::dir_ls(regexp = "counts.txt$")

fC_summary_fpath <-
  system.file(package = "ngsmisc") %>%
  fs::dir_ls(regexp = "gene_counts.txt.summary$")

# Test fC_read_count() ---------------------------------------------------------
fC_tbl_li <- lapply(fC_count_fpath, fC_read_count)
test_that("fC_read_count() test 1", {
  expect_equal(
    fC_tbl_li[[1]],
    readr::read_tsv(file = fC_count_fpath[1], skip = 1L, col_types = "cccccii")
  )
  expect_equal(
    colnames(fC_tbl_li[[1]])[1:6],
    c("Geneid", "Chr", "Start", "End", "Strand", "Length")
  )
})

# Test fC_read_summary() -------------------------------------------------------
fC_sum_li <- fC_read_summary(fpath = fC_summary_fpath)
test_that("fC_read_summary() test 1", {
  expect_equal(dim(fC_sum_li), c(14, 4))
  expect_true(is.character(fC_sum_li[[1]]))
  expect_equal(colnames(fC_sum_li)[1], "Status")
  expect_true(all(unlist(lapply(fC_sum_li[-1], is.integer))))
})

# Test fC_rename_col() ---------------------------------------------------------
temp1 <- fC_tbl_li[[1]]
temp2 <- fC_rename_col(fC_tbl_li[[1]])
test_that("fC_rename_col() test 1", {
  # 列名以外は変えない
  expect_true(purrr::map2_lgl(temp1, temp2, ~ all(.x == .y)) %>% all())
  #
  expect_equal(colnames(temp1)[1:6], colnames(temp2)[1:6])
  expect_equal(colnames(temp2)[7], "sample_1")
})

temp1 <- fC_rename_col(fC_tbl_li[[1]], col_fpath = 1L) %>% colnames()
temp2 <- fC_rename_col(fC_tbl_li[[1]], file_suffix = ".sort") %>% colnames()
test_that("fC_rename_col() test 2", {
  expect_equal(temp1[7], "some/sample_1.sort.bam")
  expect_equal(temp2[7], "sample_1.bam")
})

# Test fC_merge_tbl_li() ---------------------------------------------------------
temp1 <- fC_tbl_li[[1]]
temp2 <- fC_merge_tbl_li(fC_tbl_li)
test_that("fC_merge_tbl_li() test 1", {
  expect_equal(dim(temp2), c(133, 9))
  expect_true(identical(temp1[,1:7], temp2[,1:7]))
})

# Test fC_calc_rpm() ---------------------------------------------------------
fC_tbl_merged <- lapply(fC_count_fpath, fC_read_count) %>% fC_merge_tbl_li()
test_that("fC_calc_rpm()", {
  expect_no_error(fC_calc_rpm(fC_tbl_merged))
  expect_equal(
    ncol(fC_calc_rpm(fC_tbl_merged)),
    ncol(fC_tbl_merged) + 3L
  )
  expect_equal(
    colnames(fC_calc_rpm(fC_tbl_merged)),
    c(
      colnames(fC_tbl_merged),
      paste0("rpm_", colnames(fC_tbl_merged)[-(1:6)])
    )
  )
})

# Test fC_calc_rpkm() ---------------------------------------------------------
fC_tbl_merged <- lapply(fC_count_fpath, fC_read_count) %>% fC_merge_tbl_li()
test_that("fC_calc_rpkm()", {
  expect_no_error(fC_calc_rpkm(fC_tbl_merged))
  expect_equal(
    ncol(fC_calc_rpkm(fC_tbl_merged)),
    ncol(fC_tbl_merged) + 3L
  )
  expect_equal(
    colnames(fC_calc_rpkm(fC_tbl_merged)),
    c(
      colnames(fC_tbl_merged),
      paste0("rpkm_", colnames(fC_tbl_merged)[-(1:6)])
    )
  )
})

# Test fC_calc_tpm() ---------------------------------------------------------
fC_tbl_merged <- lapply(fC_count_fpath, fC_read_count) %>% fC_merge_tbl_li()
test_that("fC_calc_tpm()", {
  expect_no_error(fC_calc_tpm(fC_tbl_merged))
  expect_equal(
    ncol(fC_calc_tpm(fC_tbl_merged)),
    ncol(fC_tbl_merged) + 3L
  )
  expect_equal(
    colnames(fC_calc_tpm(fC_tbl_merged)),
    c(
      colnames(fC_tbl_merged),
      paste0("tpm_", colnames(fC_tbl_merged)[-(1:6)])
    )
  )
})
