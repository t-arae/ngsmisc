
ST_final_log_fpath <-
  system.file(package = "ngsmisc", "star") %>%
  fs::dir_ls(regexp = ".final.log$")

ST_SJ_tab_fpath <-
  system.file(package = "ngsmisc", "star") %>%
  fs::dir_ls(regexp = ".sj.tsv$")

# Test ST_parse_final_log() ----------------------------------------------------
test_that("ST_parse_final_log() test", {
  # Check arguments
  expect_equal(names(formals(ST_parse_final_log)), c("fpath"))

  li <- ST_parse_final_log(ST_final_log_fpath[1])
  expect_true(is.list(li))
  expect_equal(length(li), 5)
  expect_equal(
    names(li),
    c("Summary", "Unique Reads", "Multi-mapping Reads", "Unmapped Reads", "Chimeric Reads")
  )
  expect_equal(length(unlist(li)), 32)
})

# Test ST_read_final_log() -----------------------------------------------------
test_that("ST_read_final_log() test", {
  # Check arguments
  expect_equal(names(formals(ST_read_final_log)), c("fpath"))
  # expect_equal(formals(ST_read_final_log)[["rename_col"]], quote(function(x) x))

  tbl <- ST_read_final_log(ST_final_log_fpath[1]) %>% rename_fpath(nth = 3)
  expect_equal(nrow(tbl), 32)
  expect_equal(ncol(tbl), 3)
  expect_equal(colnames(tbl), c("contents_group", "contents", "sample1.final.log"))
  expect_true(is.factor(tbl$contents_group))
  expect_true(is.factor(tbl$contents))
})

# Test ST_merge_final_log() ----------------------------------------------------
test_that("ST_merge_final_log() test", {
  # Check arguments
  expect_equal(names(formals(ST_merge_final_log)), "li_tbl")

  li_tbl <-
    lapply(ST_final_log_fpath, ST_read_final_log) %>%
    lapply(rename_fpath, nth = 3)
  tbl <- ST_merge_final_log(li_tbl)
  expect_equal(
    colnames(tbl),
    c("contents_group", "contents", "sample1.final.log", "sample2.final.log", "sample3.final.log")
  )
})

# Test ST_read_sj_tab() --------------------------------------------------------
test_that("ST_read_sj_tab() test", {
  # Check arguments
  expect_equal(names(formals(ST_read_sj_tab)), c("fpath", "decode"))
  expect_false(formals(ST_read_sj_tab)[["decode"]])

  tbl <- ST_read_sj_tab(ST_SJ_tab_fpath[1])
  expect_equal(nrow(tbl), 100)
  expect_equal(ncol(tbl), 9)
  expect_equal(
    colnames(tbl),
    c("seqnames", "start", "end", "strand", "intron_motif", "annotated",
      "num_uniq_map_jc", "num_multi_map_jc", "max_overhang")
  )
})
