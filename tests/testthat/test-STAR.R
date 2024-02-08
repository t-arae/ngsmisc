
ST_final_log_fpath <-
  system.file(package = "ngsmisc", "star") %>%
  fs::dir_ls(regexp = ".final.log$")

ST_SJ_tab_fpath <-
  system.file(package = "ngsmisc", "star") %>%
  fs::dir_ls(regexp = ".sj.tsv$")

test_that("ST_parse_final_log()", {
  # Check interface
  expect_equal(names(formals(ST_parse_final_log)), c("fpath"))

  # Check output
  expect_no_message(ST_parse_final_log(ST_final_log_fpath[1]))

  li <- ST_parse_final_log(ST_final_log_fpath[1])
  expect_true(is.list(li))
  expect_equal(length(li), 5)
  expect_equal(
    names(li),
    c("Summary", "Unique Reads", "Multi-mapping Reads", "Unmapped Reads", "Chimeric Reads")
  )
  expect_equal(length(unlist(li)), 32)

  # test the checking of file header
  tempf <- tempfile("temp.txt")
  file.copy(ST_final_log_fpath[1], tempf)
  readLines(tempf) %>%
    sub(pattern = "Started job on \\|", replacement = "Started job on  |") %>%
    writeLines(tempf)
  expect_error(ST_parse_final_log(tempf))
  file.remove(tempf)
})

test_that("ST_read_final_log()", {
  # Check interface
  expect_equal(names(formals(ST_read_final_log)), c("fpath"))

  # Check output
  expect_no_message(ST_read_final_log(ST_final_log_fpath[1]))

  tbl <- ST_read_final_log(ST_final_log_fpath[1]) %>% rename_fpath(nth = 3)
  expect_equal(nrow(tbl), 32)
  expect_equal(ncol(tbl), 3)
  expect_equal(colnames(tbl), c("contents_group", "contents", "sample1.final.log"))
  expect_true(is.factor(tbl$contents_group))
  expect_true(is.factor(tbl$contents))
})

li_tbl <-
  lapply(ST_final_log_fpath, ST_read_final_log) %>%
  lapply(rename_fpath, nth = 3)
test_that("ST_merge_final_log()", {
  # Check interface
  expect_equal(names(formals(ST_merge_final_log)), "li_tbl")

  # Check output
  expect_no_message(ST_merge_final_log(li_tbl))

  tbl <- ST_merge_final_log(li_tbl)
  expect_equal(
    colnames(tbl),
    c("contents_group", "contents", "sample1.final.log", "sample2.final.log", "sample3.final.log")
  )
})

test_that("ST_read_sj_tab()", {
  # Check interface
  expect_equal(names(formals(ST_read_sj_tab)), c("fpath", "decode"))
  expect_equal(formals(ST_read_sj_tab)[["decode"]], FALSE)

  # Check output
  expect_no_message(ST_read_sj_tab(ST_SJ_tab_fpath[1]))
  expect_no_message(ST_read_sj_tab(ST_SJ_tab_fpath[1], decode = TRUE))

  tbl <- ST_read_sj_tab(ST_SJ_tab_fpath[1])
  expect_equal(nrow(tbl), 100)
  expect_equal(ncol(tbl), 9)
  expect_equal(
    colnames(tbl),
    c("seqnames", "start", "end", "strand", "intron_motif", "annotated",
      "num_uniq_map_jc", "num_multi_map_jc", "max_overhang")
  )

  tbl <- ST_read_sj_tab(ST_SJ_tab_fpath[1], decode = TRUE)
  expect_equal(nrow(tbl), 100)
  expect_equal(
    colnames(tbl),
    c("seqnames", "start", "end", "strand", "intron_motif", "annotated",
      "num_uniq_map_jc", "num_multi_map_jc", "max_overhang")
  )
  expect_equal(levels(tbl$strand), c("+", "-", "*"))
  expect_equal(levels(tbl$intron_motif),
               c("GT/AG", "CT/AC", "GC/AG", "CT/GC",
                 "AT/AC", "GT/AT", "non-canonical"))
  expect_equal(levels(tbl$annotated), c("annotated", "unannotated"))
})
