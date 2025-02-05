
skip_if_not_installed("DESeq2")

tbl_count <-
  system.file(package = "ngsmisc", "deseq2") |>
  fs::dir_ls(regexp = "count.csv$") |>
  readr::read_csv(show_col_types = FALSE)

test_that("ds2_tbl_to_rcdf()", {
  # Check interface
  expect_equal(names(formals(ds2_tbl_to_rcdf)), c("tbl", "id_col"))
  expect_equal(formals(ds2_tbl_to_rcdf)$id_col, "Geneid")

  # Check output
  rcdf <- ds2_tbl_to_rcdf(tbl_count)
  expect_equal(class(rcdf), "data.frame")
  expect_equal(dim(rcdf), c(nrow(tbl_count), ncol(tbl_count) - 1L))
  expect_equal(colnames(rcdf), colnames(tbl_count)[-1])
  expect_equal(rownames(rcdf), tbl_count$Geneid)
  expect_equal(as.logical(apply(rcdf, 2, is.integer)), rep(TRUE, 3))
})

test_that("ds2_mat_select_col()", {
  # Check interface
  expect_equal(names(formals(ds2_mat_select_col)), c("mat", "pattern", "negate"))
  expect_equal(formals(ds2_mat_select_col)$negate, FALSE)

  # Check output
  mat <- head(iris[, 1:4]) |> as.matrix()
  expect_equal(ds2_mat_select_col(mat, "Length"), mat[,c(1,3)])
  expect_equal(ds2_mat_select_col(mat, "Length", negate = TRUE), mat[,c(2,4)])
})

test_that("ds2_mat_plot_design()", {
  # Check interface
  expect_equal(names(formals(ds2_mat_plot_design)), c("mat_design", "flip"))
  expect_equal(formals(ds2_mat_plot_design)$flip, TRUE)
})

rcdf <- ds2_tbl_to_rcdf(tbl_count)
test_that("ds2_rcdf_filter_by_rownames()", {
  # Check interface
  expect_equal(names(formals(ds2_rcdf_filter_by_rownames)),
               c("rcdf", "pattern", "negate"))
  expect_equal(formals(ds2_rcdf_filter_by_rownames)$negate, FALSE)

  # Check output
  expect_equal(ds2_rcdf_filter_by_rownames(rcdf, "."), rcdf)
  expect_equal(ds2_rcdf_filter_by_rownames(rcdf, "000..$"),
               rcdf[grepl("000..$", rownames(rcdf)),])
  expect_equal(ds2_rcdf_filter_by_rownames(rcdf, "000..$", negate = TRUE),
               rcdf[!grepl("000..$", rownames(rcdf)),])
})

coldata <-
  data.frame(group = factor(c("A", "A", "B")), row.names = colnames(rcdf))
test_that("ds2_rcdf_to_dds()", {
  # Check interface
  expect_equal(names(formals(ds2_rcdf_to_dds)),
               c("rcdf", "coldata", "design"))
  expect_equal(formals(ds2_rcdf_to_dds)$design, quote(~1))

  # Check output
  expect_no_message(ds2_rcdf_to_dds(rcdf, coldata))

  dds <- ds2_rcdf_to_dds(rcdf, coldata, ~ group)
  expect_equal(class(dds),
               `attr<-`("DESeqDataSet", which = "package", value = "DESeq2"))
  expect_equal(dds@design, ~group)
  expect_equal(as.data.frame(dds@assays@data$counts), rcdf)
  expect_equal(as.data.frame(dds@colData), coldata)
})

dds <- ds2_rcdf_to_dds(rcdf, coldata, ~ group)
test_that("ds2_sizefactor", {
  # Check interface
  expect_equal(names(formals(ds2_dds_estimate_sizefactor)), c("dds", "..."))
  expect_equal(names(formals(ds2_dds_get_sizefactor)), c("dds", "..."))
  expect_equal(names(formals(ds2_dds_set_sizefactor)), c("dds", "sizefactor"))

  # Check output
  expect_no_message(ds2_dds_estimate_sizefactor(dds))
  expect_no_message(ds2_dds_get_sizefactor(dds))
  expect_no_message(ds2_dds_set_sizefactor(dds, 1:3))

  expect_error(ds2_dds_estimate_sizefactor(rcdf))
  expect_error(ds2_dds_get_sizefactor(rcdf))
  expect_error(ds2_dds_set_sizefactor(dds))
  expect_error(ds2_dds_set_sizefactor(dds, letters[1:3]))
  expect_error(ds2_dds_set_sizefactor(dds, c(1,2,NA)))

  dds_after <- ds2_dds_estimate_sizefactor(dds)
  expect_equal(class(dds_after),
               `attr<-`("DESeqDataSet", which = "package", value = "DESeq2"))
  expect_equal(colnames(dds_after@colData), c("group", "sizeFactor"))
  expect_equal(dds_after@colData$sizeFactor,
               DESeq2::estimateSizeFactorsForMatrix(rcdf))

  expect_equal(ds2_dds_get_sizefactor(dds),
               dds_after@colData$sizeFactor)

  sf <- 1:3
  expect_equal(
    ds2_dds_set_sizefactor(dds_after, sf) |> ds2_dds_get_sizefactor(),
    setNames(sf, paste0("sample_", 1:3)))
})

dds_w_sf <-
  ds2_rcdf_to_dds(rcdf, coldata, ~ group) |>
  ds2_dds_estimate_sizefactor()
test_that("ds2_dds_estimate_disp", {
  # Check interface
  expect_equal(names(formals(ds2_dds_estimate_disp)), c("dds", "..."))

  # Check output
  suppressMessages(expect_message(ds2_dds_estimate_disp(dds_w_sf)))
  expect_no_message(ds2_dds_estimate_disp(dds_w_sf, quiet = TRUE))
  expect_error(ds2_dds_estimate_disp(dds))

  dds_w_disp <- ds2_dds_estimate_disp(dds_w_sf, quiet = TRUE)
  expect_true(all(is.numeric(DESeq2::dispersions(dds_w_disp))))
  expect_equal(length(DESeq2::dispersions(dds_w_disp)), dim(dds_w_disp)[1])
})

dds_w_disp <- ds2_dds_estimate_disp(dds_w_sf, quiet = TRUE)
test_that("ds2_test", {
  # Check interface
  expect_equal(names(formals(ds2_dds_test_nbinomLRT)), c("dds", "reduced", "..."))
  expect_equal(formals(ds2_dds_test_nbinomLRT)$reduced, quote(~1))
  expect_equal(names(formals(ds2_dds_test_nbinomWaldTest)), c("dds", "..."))

  # Check output
  expect_no_message(ds2_dds_test_nbinomLRT(dds_w_disp))
  expect_no_message(ds2_dds_test_nbinomLRT(dds_w_disp, ~ 1 + 1))
  expect_no_message(ds2_dds_test_nbinomWaldTest(dds_w_disp))
  expect_error(ds2_dds_test_nbinomLRT(dds_w_disp, ~ group))

  dds_lrt <- ds2_dds_test_nbinomLRT(dds_w_disp)
  expect_equal(class(dds_lrt),
               `attr<-`("DESeqDataSet", which = "package", value = "DESeq2"))
  expect_equal(dds_lrt@test, "LRT")

  dds_wald <- ds2_dds_test_nbinomWaldTest(dds_w_disp)
  expect_equal(class(dds_wald),
               `attr<-`("DESeqDataSet", which = "package", value = "DESeq2"))
  expect_equal(dds_wald@test, "Wald")
})

test_that("ds2_design", {
  # Check interface
  expect_equal(names(formals(ds2_dds_get_design)), c("dds"))
  expect_equal(names(formals(ds2_dds_set_design)), c("dds", "design"))

  # Check output
  expect_no_message(ds2_dds_get_design(dds))
  expect_no_message(ds2_dds_set_design(dds, ~ 1))
  expect_no_message(ds2_dds_set_design(dds, model.matrix(~ 1, coldata)))
  expect_no_message(ds2_dds_set_design(dds, model.matrix(~ group, coldata)))
  expect_no_message(ds2_dds_set_design(dds, model.matrix(~ group, coldata)[,1:2]))

  expect_equal(class(ds2_dds_get_design(dds)), "formula")
  expect_equal(class(ds2_dds_set_design(dds, ~ 1)),
               `attr<-`("DESeqDataSet", which = "package", value = "DESeq2"))
  expect_equal(class(ds2_dds_set_design(dds, model.matrix(~ group, coldata))),
               `attr<-`("DESeqDataSet", which = "package", value = "DESeq2"))

  dds_w_mat <- ds2_dds_set_design(dds, model.matrix(~ group, coldata))
  expect_equal(class(ds2_dds_get_design(dds_w_mat)), c("matrix", "array"))
})

test_that("ds2_dds_get_normalized_count_tbl", {
  # Check interface
  expect_equal(names(formals(ds2_dds_get_normalized_count_tbl)),
               c("dds", "rownames"))
  expect_equal(formals(ds2_dds_get_normalized_count_tbl)$rownames, "Geneid")

  # Check output
  expect_no_message(ds2_dds_get_normalized_count_tbl(dds_w_sf))
  expect_no_message(ds2_dds_get_normalized_count_tbl(dds))
  expect_no_message(ds2_dds_get_normalized_count_tbl(dds_w_sf, rownames = "sample_1"))
  expect_no_message(ds2_dds_get_normalized_count_tbl(dds_w_sf, rownames = NA))
  expect_no_message(ds2_dds_get_normalized_count_tbl(dds_w_sf, rownames = NULL))

  expect_equal(class(ds2_dds_get_normalized_count_tbl(dds_w_sf)),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(ds2_dds_get_normalized_count_tbl(dds_w_sf)), c(133L, 4L))
  expect_equal(ds2_dds_get_normalized_count_tbl(dds_w_sf),
               ds2_dds_get_normalized_count_tbl(dds))
})

dds_lrt <- ds2_dds_test_nbinomLRT(dds_w_disp)
dds_wald <- ds2_dds_test_nbinomWaldTest(dds_w_disp)
test_that("ds2_dds_to", {
  # Check interface
  expect_equal(names(formals(ds2_dds_to_ddr)), c("dds", "..."))
  expect_equal(names(formals(ds2_dds_to_tbl)), c("dds", "rownames"))
  expect_equal(formals(ds2_dds_to_tbl)$rownames, "Geneid")

  # Check output
  expect_no_message(ds2_dds_to_ddr(dds_lrt))
  expect_no_message(ds2_dds_to_tbl(dds_lrt))
  expect_error(ds2_dds_to_tbl(dds))

  ddr <- ds2_dds_to_ddr(dds_lrt)
  expect_equal(class(ddr),
               `attr<-`("DESeqResults", which = "package", value = "DESeq2"))
  expect_equal(class(ds2_dds_to_tbl(dds_lrt)),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(ds2_dds_to_tbl(dds_wald)),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(ds2_dds_to_tbl(dds_lrt)), c(133L, 23L))
  expect_equal(dim(ds2_dds_to_tbl(dds_wald)), c(133L, 24L))
})

ddr_lrt <- ds2_dds_to_ddr(dds_lrt)
ddr_wald <- ds2_dds_to_ddr(dds_wald)
test_that("ds2_ddr_to_tbl()", {
  # Check interface
  expect_equal(names(formals(ds2_ddr_to_tbl)), c("ddr", "rownames"))
  expect_equal(formals(ds2_ddr_to_tbl)$rownames, "Geneid")

  # Check output
  expect_no_message(ds2_ddr_to_tbl(ddr_lrt))
  expect_no_message(ds2_ddr_to_tbl(ddr_wald))

  tbl_lrt <- ds2_ddr_to_tbl(ddr_lrt)
  tbl_wald <- ds2_ddr_to_tbl(ddr_wald)
  expect_equal(class(tbl_lrt), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(tbl_wald), c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(tbl_lrt), c(133L, 7L))
  expect_equal(dim(tbl_wald), c(133L, 7L))
  expect_equal(tbl_lrt[,1:4], tbl_wald[,1:4])
})

test_that("ds2_ddr_plot_independent_filtering()", {
  # Check interface
  expect_equal(names(formals(ds2_ddr_plot_independent_filtering)), c("ddr", "title"))
  expect_equal(formals(ds2_ddr_plot_independent_filtering)$title, "")

  # Check output
  expect_no_message(ds2_ddr_plot_independent_filtering(ddr_lrt))
  expect_no_message(ds2_ddr_plot_independent_filtering(ddr_wald))
})

test_that("ds2_rcdf_filter_organella()", {
  lifecycle::expect_deprecated(ds2_rcdf_filter_organella(rcdf))
})
