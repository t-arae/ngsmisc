
# Convert tibble to a readcount data.frame -------------------------------------

#' Prepare input readcount data.frame for DESeq2 from a tibble
#' @param tbl a tibble containing read count columns and a gene name column
#' @param genename_col character specifying the gene name column. default: "Geneid"
#' @export
#'
ds2_tbl_to_rcdf <- function(tbl, genename_col = "Geneid") {
  purrr::modify_if(tbl, is.double, as.integer) %>%
    tibble::column_to_rownames(var = genename_col)
}

# Functions for the readcount data.frame ---------------------------------------

#' Filter readcount data.frame by rownames
#' @param rcdf a data.frame created by ds2_prepare_rcdf()
#' @param pattern a regex pattern to look for.
#' @param negate If TRUE, return non-matching elements. default: TRUE
#' @export
#'
ds2_rcdf_filter_by_rownames <- function(rcdf, pattern, negate = TRUE) {
  rows_filtered <-
    row.names(rcdf) %>%
    stringr::str_detect(pattern = pattern, negate = negate)
  rcdf[rows_filtered,]
}

#' Filter organella reads from readcount data.frame
#' @inheritParams ds2_rcdf_filter_by_rownames
#' @export
#'
ds2_rcdf_filter_organella <- function(rcdf) {
  ds2_rcdf_filter_by_rownames(rcdf, "^AT[CM]G")
}

#' Prepare DESeqDataSet object from readcount data.frame
#' @inheritParams ds2_rcdf_filter_by_rownames
#' @param coldata a data.frame specifying sample info
#' @param design design. default: ~ 1 (no design)
#' @export
#'
ds2_rcdf_to_dds <- function(rcdf, coldata, design = ~ 1) {
  DESeq2::DESeqDataSetFromMatrix(
    countData = rcdf,
    colData = coldata,
    design = design
  )
}

# Functions for the DESeqDataSet object ----------------------------------------
## Size factor -----------------------------------------------------------------

#' Estimate library size factors
#' @param dds a DESeqDataSet object
#' @param ... ...
#' @export
#'
ds2_dds_estimate_sizefactor <- function(dds, ...) {
  DESeq2:::estimateSizeFactors.DESeqDataSet(object = dds, ...)
}

#' Set library size factors manually
#' @inheritParams ds2_dds_estimate_sizefactor
#' @param sizefactor a double vector. its length must be equal to ncol(dds)
#' @export
#'
ds2_dds_set_sizefactor <- function(dds, sizefactor) {
  DESeq2::sizeFactors(dds) <- sizefactor
  dds
}

#' Get library sizeFactors
#' @inheritParams ds2_dds_estimate_sizefactor
#' @export
#'
ds2_dds_get_sizefactor <- function(dds, ...) {
  sf <- DESeq2::sizeFactors(dds)
  if(is.null(sf)) {
    sf <-
      DESeq2:::estimateSizeFactors.DESeqDataSet(object = dds, ...) %>%
      DESeq2::sizeFactors()
  }
  sf
}

## Dispersion ------------------------------------------------------------------

#' Estimate dispersions
#' @inheritParams ds2_dds_estimate_sizefactor
#' @export
#'
ds2_dds_estimate_disp <- function(dds) {
  DESeq2:::estimateDispersions.DESeqDataSet(dds)
}

## Run test --------------------------------------------------------------------

#' Run Likelihood ratio test for GLMs
#' @inheritParams ds2_dds_estimate_sizefactor
#' @param reduced reduced model
#' @export
#'
ds2_dds_test_nbinomLRT <- function(dds, reduced = ~ 1) {
  DESeq2::nbinomLRT(dds, reduced = reduced)
}

#' Run Wald test for GLM coefficients
#' @inheritParams ds2_dds_estimate_sizefactor
#' @param ... ...
#' @export
#'
ds2_dds_test_nbinomWaldTest <- function(dds, ...) {
  DESeq2::nbinomWaldTest(object = dds, ...)
}


## Experimental design ---------------------------------------------------------

#' Get experimtal design
#' @inheritParams ds2_dds_estimate_sizefactor
#' @export
#'
ds2_dds_get_design <- function(dds) {
  DESeq2::design(dds)
}

#' Set experimental design
#' @inheritParams ds2_dds_estimate_sizefactor
#' @param design design
#' @export
#'
ds2_dds_set_design <- function(dds, design) {
  DESeq2::design(dds) <- design
  dds
}

## Misc ------------------------------------------------------------------------

#' Get normalized count data as a tibble
#' @inheritParams ds2_dds_estimate_sizefactor
#' @param rownames a column name. default: "Geneid"
#' @export
#'
ds2_dds_get_normalized_count_tbl <- function(dds, rownames = "Geneid") {
  df <- as.data.frame(DESeq2::counts(dds))
  purrr::modify2(
    .x = df,
    .y = DESeq2::sizeFactors(dds),
    .f = ~ .x / .y
  ) %>%
    `row.names<-`(row.names(df)) %>%
    tibble::as_tibble(rownames = rownames)
}

#' Get a DESeqResults object from a DESeqDataSet object
#' @inheritParams ds2_dds_estimate_sizefactor
#' @export
ds2_dds_to_ddr <- function(dds, ...) {
  DESeq2::results(object = dds, ...)
}

#' Convert a DESeqDataSet object to tibble
#' @inheritParams ds2_dds_estimate_sizefactor
#' @param rownames a column name. default: "Geneid"
#' @importFrom rlang :=
#' @export
#'
ds2_dds_to_tbl <- function(dds, rownames = "Geneid") {
  test <- attr(dds, which = "test")
  if(test %in% c("Wald", "LRT")) {
    dds@rowRanges@elementMetadata %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        !!rownames := row.names(DESeq2::counts(dds)),
        .before = 1
      ) %>%
      dplyr::mutate(padj = DESeq2::results(dds)$padj) %>%
      return()
  } else {
    stop("dds have to be tested.")
  }
}

# Functions for the DESeqResults object ----------------------------------------

#' Convert a DESeqResults object to tibble
#' @param ddr a DESeqResult object
#' @param rownames a column name. default: "Geneid"
#' @export
#'
ds2_ddr_to_tbl <- function(ddr, rownames = "Geneid") {
  log2FoldChange <- NULL
  tibble::as_tibble(ddr, rownames = rownames) %>%
    dplyr::rename(l2fc = log2FoldChange)
}

#' Plot independent filtering
#' @param ddr a DESeqResult object
#' @param title plot title
#' @export
#'
ds2_ddr_plot_independent_filtering <- function(ddr, title = "") {
  theta <- numRej <- x <- y <- log2FoldChange <- NULL
  ddr@metadata$filterNumRej %>%
    ggplot2::ggplot(ggplot2::aes(theta, numRej)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      data = as.data.frame(ddr@metadata$lo.fit),
      ggplot2::aes(x, y), color = "red") +
    ggplot2::geom_vline(xintercept = ddr@metadata$filterTheta) +
    ggplot2::labs(title = title, x = "quantiles of filter",
                  y = "number of rejections") +
    ggplot2::theme_linedraw() +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, .1))
    )
}

