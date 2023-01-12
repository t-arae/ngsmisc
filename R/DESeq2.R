
#' Prepare input readcount data.frame for DESeq2 from a tibble
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_tbl_to_rcdf()` convert a read-count data tibble into a data.frame.
#'
#' @param tbl a tibble containing read count columns and a gene name column
#' @param id_col character specifying the gene name column. (default: `"Geneid"`)
#'
#' @examples
#' # Example read-count csv file
#' inf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$")
#'
#' # Read read-count csv file
#' tbl_count <- readr::read_csv(inf, show_col_types = FALSE)
#' tbl_count
#'
#' # Convert tibble to data.frame
#' rcdf <- ds2_tbl_to_rcdf(tbl_count)
#' head(rcdf)
#'
#' rownames(head(rcdf))
#'
#' @export
ds2_tbl_to_rcdf <- function(tbl, id_col = "Geneid") {
  purrr::modify_if(tbl, is.double, as.integer) %>%
    tibble::column_to_rownames(var = id_col)
}

#' Filter read-count data.frame by rownames
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_rcdf_filter_by_rownames()` filter a read-count data.frame by rownames.
#'
#' @param rcdf a read-count data.frame created by `ds2_tbl_to_rcdf()`
#' @param pattern a regex pattern to look for.
#' @param negate If `TRUE`, return non-matching elements. (default: `FALSE`)
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Original read-count data.frame
#' head(rcdf)
#'
#' # Filtering read-count data.frame by rownames.
#' ds2_rcdf_filter_by_rownames(head(rcdf), pattern = "[123]0")
#'
#' # Invert filter by setting as `negate = TRUE`.
#' ds2_rcdf_filter_by_rownames(head(rcdf), pattern = "[123]0", negate = TRUE)
#'
#' @export
ds2_rcdf_filter_by_rownames <- function(rcdf, pattern, negate = FALSE) {
  rows_filtered <-
    row.names(rcdf) %>%
    stringr::str_detect(pattern = pattern, negate = negate)
  rcdf[rows_filtered,]
}

#' Filter organella reads from readcount data.frame
#' @description
#' `r lifecycle::badge("deprecated")`
#' @inheritParams ds2_rcdf_filter_by_rownames
#' @export
ds2_rcdf_filter_organella <- function(rcdf) {
  lifecycle::deprecate_warn(
    when = "0.3.0",
    what = "ds2_rcdf_filter_organella()"
  )
  ds2_rcdf_filter_by_rownames(rcdf, "^AT[CM]G")
}

#' Prepare DESeqDataSet object from read-count data.frame
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_rcdf_to_dds()` does the same thing as `DESeq2::DESeqDataSetFromMatrix()`.
#'
#' @inheritParams ds2_rcdf_filter_by_rownames
#' @param coldata a data.frame specifying sample info
#' @param design design. (default: `~ 1` (no design))
#'
#' @examples
#' # Example read-count data.frame.
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Convert read-count data.frame to a `DESeq2::DESeqDataSet-class` object.
#' ds2_rcdf_to_dds(
#'   rcdf = rcdf,
#'   coldata = data.frame(sample = colnames(rcdf))
#' )
#'
#' @export
ds2_rcdf_to_dds <- function(rcdf, coldata, design = ~ 1) {
  DESeq2::DESeqDataSetFromMatrix(
    countData = rcdf,
    colData = coldata,
    design = design
  )
}

#' Manipulate (estimate/get/set) library size factors
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_dds_estimate_sizefactor()` does the same thing as `DESeq2::estimateSizeFactors()`.
#'
#' `ds2_dds_get_sizefactor()` gets size-factor as a named numeric vector.
#'
#' `ds2_dds_set_sizefactor()` manually sets specified size-factor to the given object.
#'
#' @param dds a `DESeqDataSet` object
#' @param sizefactor a double vector. the length must be equal to `ncol(dds)`.
#' @param ... further arguments passed to `DESeq2:::estimateSizeFactors.DESeqDataSet()`. see Details.
#'
#' @details
#' To see the advanced settings, run `` ?DESeq2::`estimateSizeFactors,DESeqDataSet-method` ``.
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Prepare `DESeqDataSet` object without estimating size-factor.
#' dds_wo_sf <-
#'   ds2_rcdf_to_dds(
#'     rcdf = rcdf,
#'     coldata = data.frame(sample = colnames(rcdf))
#'   )
#'
#' # Estimate and set library size factors.
#' dds_w_sf <- dds_wo_sf %>% ds2_dds_estimate_sizefactor()
#'
#' # If the specfied `dds` object has not computed size-factor before,
#' # it will automatically compute size-factor and return it.
#' dds_w_sf %>% ds2_dds_get_sizefactor()
#'
#' dds_wo_sf %>% ds2_dds_get_sizefactor()
#'
#' # The size-factor can be set manually.
#' dds_wo_sf %>% ds2_dds_set_sizefactor(1:3) %>% ds2_dds_get_sizefactor()
#'
#' @name ds2_sizefactor

#' @rdname ds2_sizefactor
#' @export
ds2_dds_estimate_sizefactor <- function(dds, ...) {
  DESeq2:::estimateSizeFactors.DESeqDataSet(object = dds, ...)
}

#' @rdname ds2_sizefactor
#' @export
ds2_dds_get_sizefactor <- function(dds, ...) {
  sf <- DESeq2::sizeFactors(dds)
  if(is.null(sf)) {
    sf <-
      DESeq2:::estimateSizeFactors.DESeqDataSet(object = dds, ...) %>%
      DESeq2::sizeFactors()
  }
  sf
}

#' @rdname ds2_sizefactor
#' @export
ds2_dds_set_sizefactor <- function(dds, sizefactor) {
  DESeq2::sizeFactors(dds) <- sizefactor
  dds
}

#' Estimate dispersions
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_dds_estimate_disp()` does the same thing as `DESeq2::estimateDispersions()`.
#'
#' @param dds a DESeqDataSet object
#' @param ... pass to the DESeq2::estimateDispersions()
#' @param ... further arguments are passed to `DESeq2::estimateDispersions()`. see Details.
#'
#' @details
#' To see the advanced settings, run `` ?DESeq2::`estimateDispersions,DESeqDataSet-method` ``.
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#' dds <-
#'   ds2_rcdf_to_dds(
#'     rcdf = rcdf,
#'     coldata = data.frame(sample = colnames(rcdf))
#'   ) %>%
#'   ds2_dds_estimate_sizefactor() %>%
#'   ds2_dds_estimate_disp()
#'
#' dds
#'
#' @export
ds2_dds_estimate_disp <- function(dds, ...) {
  DESeq2:::estimateDispersions.DESeqDataSet(dds, ...)
}

#' Run Likelihood ratio test or Wald test
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_dds_test_nbinomLRT()` does the same thing as `DESeq2::nbinomLRT()`.
#'
#' `ds2_dds_test_nbinomWaldTest()` does the same thing as `DESeq2::nbinomWaldTest()`.
#'
#' @param dds a `DESeq2::DESeqDataSet` class object
#' @param reduced reduced model. (default: `~ 1`)
#' @param ... further arguments are passed to `DESeq2::nbinomLRT()` or `DESeq2::nbinomWaldTest()`. See Details.
#'
#' @details
#' To see the advanced settings, run `?DESeq2::nbinomLRT()` or `?DESeq2::nbinomWaldTest()`.
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#' dds <-
#'   ds2_rcdf_to_dds(
#'     rcdf = rcdf,
#'     coldata = data.frame(
#'       sample = colnames(rcdf),
#'       group = as.factor(c("A", "A", "B"))
#'     ),
#'     design = ~ group
#'   ) %>%
#'   ds2_dds_estimate_sizefactor() %>%
#'   ds2_dds_estimate_disp()
#'
#' # Likelihood ratio test
#' ds2_dds_test_nbinomLRT(dds) %>% ds2_dds_to_ddr()
#'
#' # Wald test
#' ds2_dds_test_nbinomWaldTest(dds) %>% ds2_dds_to_ddr()
#'
#' @name ds2_test
NULL

#' @rdname ds2_test
#' @export
ds2_dds_test_nbinomLRT <- function(dds, reduced = ~ 1, ...) {
  DESeq2::nbinomLRT(dds, reduced = reduced, ...)
}

#' @rdname ds2_test
#' @export
ds2_dds_test_nbinomWaldTest <- function(dds, ...) {
  DESeq2::nbinomWaldTest(object = dds, ...)
}

#' Get/Set the experimental design
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_dds_get_design()` gets design.
#'
#' `ds2_dds_set_design()` sets design.
#'
#' @param dds a DESeqDataSet object
#' @param design design
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Prepare `DESeqDataSet` object, without specifying `design`.
#' cd <-
#'   data.frame(
#'     sample = colnames(rcdf),
#'     group = as.factor(c("A", "A", "B"))
#'   )
#' dds <- ds2_rcdf_to_dds(rcdf = rcdf, coldata = cd)
#'
#' # Get design formula
#' dds %>% ds2_dds_get_design()
#'
#' # Set design formula
#' dds %>% ds2_dds_set_design(~ group) %>% ds2_dds_get_design()
#'
#' @name ds2_design
NULL

#' @rdname ds2_design
#' @export
ds2_dds_get_design <- function(dds) {
  DESeq2::design(dds)
}

#' @rdname ds2_design
#' @export
ds2_dds_set_design <- function(dds, design) {
  DESeq2::design(dds) <- design
  dds
}

#' Compute the normalized count using size-factor, and return as a tibble
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_dds_get_normalized_count_tbl()` extracts normalized count data as a tibble.
#'
#' @param dds a DESeqDataSet object
#' @param rownames a column name. (default: `"Geneid"`)
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#' dds <-
#'   ds2_rcdf_to_dds(
#'     rcdf = rcdf,
#'     coldata = data.frame(sample = colnames(rcdf))
#'   ) %>%
#'   ds2_dds_estimate_sizefactor()
#'
#' # Get normalized counts as a tibble
#' dds %>% ds2_dds_get_normalized_count_tbl()
#'
#' # If you set a manual size-factor, then use it for computing.
#' dds %>%
#'   ds2_dds_set_sizefactor(1:3) %>%
#'   ds2_dds_get_normalized_count_tbl()
#'
#' @export
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

#' Convert `DESeq2::DESeqDataSet` class object to other class object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_dds_to_ddr()` converts to a `DESeq2::DESeqResults` class object.
#'
#' `ds2_dds_to_tbl()` converts to a tibble.
#'
#' @param dds a DESeqDataSet object
#' @param rownames a column name. (default: `"Geneid"`)
#' @param ... further arguments are passed to `DESeq2::results()`. See Details.
#'
#' @details
#' To see the advanced settings, run `?DESeq2::results()`.
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#' dds <-
#'   ds2_rcdf_to_dds(
#'     rcdf = rcdf,
#'     coldata = data.frame(
#'       sample = colnames(rcdf),
#'       group = as.factor(c("A", "A", "B"))
#'     ),
#'     design = ~ group
#'   ) %>%
#'   ds2_dds_estimate_sizefactor() %>%
#'   ds2_dds_estimate_disp()
#'
#' ## Convert `DESeqDataSet` to `DESeqResults` or tibble
#' # Likelihood ratio test
#' ds2_dds_test_nbinomLRT(dds) %>% ds2_dds_to_ddr()
#' ds2_dds_test_nbinomLRT(dds) %>% ds2_dds_to_tbl()
#'
#' # Wald test
#' ds2_dds_test_nbinomWaldTest(dds) %>% ds2_dds_to_ddr()
#' ds2_dds_test_nbinomWaldTest(dds) %>% ds2_dds_to_tbl()
#'
#' @name ds2_dds_to
NULL

#' @rdname ds2_dds_to
#' @export
ds2_dds_to_ddr <- function(dds, ...) {
  DESeq2::results(object = dds, ...)
}

#' @importFrom rlang :=
#' @rdname ds2_dds_to
#' @export
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

#' Convert a DESeqResults object to a tibble
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_ddr_to_tbl()` converts from a `DESeq2::DESeqResults` class object to a tibble.
#'
#' @param ddr a `DESeq2::DESeqResults` class object object
#' @param rownames a column name. (default: `"Geneid"`)
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#' dds <-
#'   ds2_rcdf_to_dds(
#'     rcdf = rcdf,
#'     coldata = data.frame(
#'       sample = colnames(rcdf),
#'       group = as.factor(c("A", "A", "B"))
#'     ),
#'     design = ~ group
#'   ) %>%
#'   ds2_dds_estimate_sizefactor() %>%
#'   ds2_dds_estimate_disp()
#'
#' # Likelihood ratio test
#' ddr <- ds2_dds_test_nbinomLRT(dds) %>% ds2_dds_to_ddr()
#'
#' # Convert `DESeqResults` to tibble
#' ddr %>% ds2_ddr_to_tbl()
#'
#' @export
ds2_ddr_to_tbl <- function(ddr, rownames = "Geneid") {
  log2FoldChange <- NULL
  tibble::as_tibble(ddr, rownames = rownames) %>%
    dplyr::rename(l2fc = log2FoldChange)
}

#' Plot independent filtering
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ds2_ddr_plot_independent_filtering()` plots the independent filtering result.
#'
#' @param ddr a `DESeq2::DESeqResults` class object object
#' @param title plot title. (default: `""`)
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") %>%
#'   fs::dir_ls(regexp = "count.csv$") %>%
#'   readr::read_csv(show_col_types = FALSE) %>%
#'   ds2_tbl_to_rcdf()
#'
#' # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#' dds <-
#'   ds2_rcdf_to_dds(
#'     rcdf = rcdf,
#'     coldata = data.frame(
#'       sample = colnames(rcdf),
#'       group = as.factor(c("A", "A", "B"))
#'     ),
#'     design = ~ group
#'   ) %>%
#'   ds2_dds_estimate_sizefactor() %>%
#'   ds2_dds_estimate_disp()
#'
#' # Likelihood ratio test
#' ddr <- ds2_dds_test_nbinomLRT(dds) %>% ds2_dds_to_ddr()
#'
#' # Plot the result of independent filtering
#' ddr %>% ds2_ddr_plot_independent_filtering()
#' @export
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
    ggplot2::theme_linedraw(base_size = 14) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, .1))
    )
}
