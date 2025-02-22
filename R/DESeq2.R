
#' Prepare input readcount data.frame for DESeq2 from a tibble
#'
#' @description
#' `ds2_tbl_to_rcdf()` converts a read-count data tibble into a data.frame.
#'
#' @param tbl a tibble containing read count columns and a gene name column
#' @param id_col character specifying the gene name column. (default: `"Geneid"`)
#'
#' @examples
#' # Example read-count csv file
#' inf <-
#'   system.file(package = "ngsmisc", "deseq2") |>
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
  purrr::modify_if(tbl, is.double, as.integer) |>
    tibble::column_to_rownames(var = id_col)
}

#' Select matrix columns matching column names using Regex.
#'
#' @description
#' `ds2_mat_select_col()` selects columns in a matrix using regex matching to the column names.
#'
#' @param mat A matrix.
#' @param pattern Pattern to look for.
#' @param negate If `TRUE`, return non-matching columns. (default: `FALSE`)
#'
#' @examples
#' mat <- head(iris[, 1:4]) |> as.matrix()
#'
#' ds2_mat_select_col(mat, "Length")
#' ds2_mat_select_col(mat, "Length", negate = TRUE)
#'
#' @export
ds2_mat_select_col <- function(mat, pattern, negate = FALSE) {
  which_col <- stringr::str_detect(colnames(mat), pattern, negate)
  as.matrix(mat[, which_col])
}

#' Plot a design matrix to check it visually
#'
#' @description
#' `ds2_mat_plot_design()` plots a design matrix.
#'
#' @param mat_design A design matrix. Typically an output from `stats::model.matrix()`.
#' @param flip If `TRUE`, flip the x and y-axis. (default: `TRUE`)
#'
#' @examples
#' df <-
#'   data.frame(
#'     genotype = c("WT", "mutant") |> rep(each = 4),
#'     treatment = c(rep("mock", 2), rep("poison", 2)) |> rep(2),
#'     time = c("0 h", "2 h") |> rep(4)
#'   )
#' row.names(df) <- apply(df, 1, paste, collapse = "_")
#' df
#'
#' mat_independent <- model.matrix(~ genotype * treatment * time, df)
#' ds2_mat_plot_design(mat_independent)
#'
#' mat_dependent <- mat_independent[3:8,]
#' ds2_mat_plot_design(mat_dependent)
#'
#' @export
ds2_mat_plot_design <- function(mat_design, flip = TRUE) {
  value <- NULL
  tbl_plot <-
    mat_design |>
    tibble::as_tibble(rownames = "sample") |>
    tidyr::pivot_longer(cols = !sample, names_to = "factor") |>
    dplyr::mutate(
      sample = forcats::fct_inorder(sample),
      factor = forcats::fct_inorder(factor),
      value = as.character(value)
    )

  if(flip) {
    gp <- ggplot2::ggplot(tbl_plot, ggplot2::aes(factor, sample))
  } else {
    gp <- ggplot2::ggplot(tbl_plot, ggplot2::aes(sample, factor))
  }
  gp +
    ggplot2::geom_tile(ggplot2::aes(fill = value), color = "grey50") +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_manual(values = c("1" = "grey20", "0" = "#00000000")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::theme(
      axis.text.x.top = ggplot2::element_text(angle = 45, hjust = 0, vjust = 0),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}


#' Filter read-count data.frame by rownames
#'
#' @description
#' `ds2_rcdf_filter_by_rownames()` filter a read-count data.frame by rownames.
#'
#' @param rcdf a read-count data.frame created by `ds2_tbl_to_rcdf()`
#' @param pattern a regex pattern to look for.
#' @param negate If `TRUE`, return non-matching elements. (default: `FALSE`)
#'
#' @examples
#' # Example read-count data.frame
#' rcdf <-
#'   system.file(package = "ngsmisc", "deseq2") |>
#'   fs::dir_ls(regexp = "count.csv$") |>
#'   readr::read_csv(show_col_types = FALSE) |>
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
    row.names(rcdf) |>
    stringr::str_detect(pattern = pattern, negate = negate)
  rcdf[rows_filtered,]
}

#' Prepare DESeqDataSet object from read-count data.frame
#'
#' @description
#' `ds2_rcdf_to_dds()` does the same thing as `DESeq2::DESeqDataSetFromMatrix()`.
#'
#' @inheritParams ds2_rcdf_filter_by_rownames
#' @param coldata a data.frame specifying sample info
#' @param design design. (default: `~ 1` (no design))
#'
#' @examples
#' if(requireNamespace("DESeq2", quietly = TRUE)) {
#'   # Example read-count data.frame.
#'   rcdf <-
#'     system.file(package = "ngsmisc", "deseq2") |>
#'     fs::dir_ls(regexp = "count.csv$") |>
#'     readr::read_csv(show_col_types = FALSE) |>
#'     ds2_tbl_to_rcdf()
#'
#'   # Convert read-count data.frame to a `DESeq2::DESeqDataSet-class` object.
#'   ds2_rcdf_to_dds(
#'     rcdf = rcdf,
#'     coldata = data.frame(sample = colnames(rcdf))
#'   )
#' }
#' @export
ds2_rcdf_to_dds <- function(rcdf, coldata, design = ~ 1) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_rcdf_to_dds()`")
  DESeq2::DESeqDataSetFromMatrix(
    countData = rcdf,
    colData = coldata,
    design = design
  )
}

#' Manipulate (estimate/get/set) library size factors
#'
#' @description
#' `ds2_dds_estimate_sizefactor()` does the same thing as `DESeq2::estimateSizeFactors()`.
#'
#' `ds2_dds_get_sizefactor()` gets size-factor as a named numeric vector.
#'
#' `ds2_dds_set_sizefactor()` manually sets specified size-factor to the given object.
#'
#' @param dds a `DESeqDataSet` object
#' @param sizefactor a double vector. the length must be equal to `ncol(dds)`.
#' @param ... further arguments passed to `DESeq2::estimateSizeFactors()`. see Details.
#'
#' @details
#' To see the advanced settings, run `` ?DESeq2::`estimateSizeFactors,DESeqDataSet-method` ``.
#'
#' @examples
#' if(requireNamespace("DESeq2", quietly = TRUE)) {
#'   # Example read-count data.frame
#'   rcdf <-
#'     system.file(package = "ngsmisc", "deseq2") |>
#'     fs::dir_ls(regexp = "count.csv$") |>
#'     readr::read_csv(show_col_types = FALSE) |>
#'     ds2_tbl_to_rcdf()
#'
#'   # Prepare `DESeqDataSet` object without estimating size-factor.
#'   dds_wo_sf <-
#'     ds2_rcdf_to_dds(
#'       rcdf = rcdf,
#'       coldata = data.frame(sample = colnames(rcdf))
#'     )
#'
#'   # Estimate and set library size factors.
#'   dds_w_sf <- dds_wo_sf |> ds2_dds_estimate_sizefactor()
#'
#'   # If the specfied `dds` object has not computed size-factor before,
#'   # it will automatically compute size-factor and return it.
#'   dds_w_sf |> ds2_dds_get_sizefactor()
#'
#'   dds_wo_sf |> ds2_dds_get_sizefactor()
#'
#'   # The size-factor can be set manually.
#'   dds_wo_sf |> ds2_dds_set_sizefactor(1:3) |> ds2_dds_get_sizefactor()
#' }
#'
#' @name ds2_sizefactor

#' @rdname ds2_sizefactor
#' @export
ds2_dds_estimate_sizefactor <- function(dds, ...) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_estimate_sizefactor()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  DESeq2::estimateSizeFactors(object = dds, ...)
}

#' @rdname ds2_sizefactor
#' @export
ds2_dds_get_sizefactor <- function(dds, ...) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_get_sizefactor()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  sf <- DESeq2::sizeFactors(dds)
  if(is.null(sf)) {
    sf <-
      DESeq2::estimateSizeFactors(object = dds, ...) |>
      DESeq2::sizeFactors()
  }
  sf
}

#' @rdname ds2_sizefactor
#' @export
ds2_dds_set_sizefactor <- function(dds, sizefactor) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_set_sizefactor()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  stopifnot(all(is.numeric(sizefactor)))
  stopifnot(all(!is.na(sizefactor)))

  DESeq2::sizeFactors(dds) <- sizefactor
  dds
}

#' Estimate dispersions
#'
#' @description
#' `ds2_dds_estimate_disp()` does the same thing as `DESeq2::estimateDispersions()`.
#'
#' @param dds a DESeqDataSet object
#' @param ... further arguments are passed to `DESeq2::estimateDispersions()`. see Details.
#'
#' @details
#' To see the advanced settings, run `` ?DESeq2::`estimateDispersions,DESeqDataSet-method` ``.
#'
#' @examples
#' if(requireNamespace("DESeq2", quietly = TRUE)) {
#'   # Example read-count data.frame
#'   rcdf <-
#'     system.file(package = "ngsmisc", "deseq2") |>
#'     fs::dir_ls(regexp = "count.csv$") |>
#'     readr::read_csv(show_col_types = FALSE) |>
#'     ds2_tbl_to_rcdf()
#'
#'   # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#'   dds <-
#'     ds2_rcdf_to_dds(
#'       rcdf = rcdf,
#'       coldata = data.frame(sample = colnames(rcdf))
#'     ) |>
#'     ds2_dds_estimate_sizefactor() |>
#'     ds2_dds_estimate_disp()
#'
#'   dds
#' }
#'
#' @export
ds2_dds_estimate_disp <- function(dds, ...) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_estimate_disp()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  DESeq2::estimateDispersions(dds, ...)
}

#' Run Likelihood ratio test or Wald test
#'
#' @description
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
#' if(requireNamespace("DESeq2", quietly = TRUE)) {
#'   # Example read-count data.frame
#'   rcdf <-
#'     system.file(package = "ngsmisc", "deseq2") |>
#'     fs::dir_ls(regexp = "count.csv$") |>
#'     readr::read_csv(show_col_types = FALSE) |>
#'     ds2_tbl_to_rcdf()
#'
#'   # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#'   dds <-
#'     ds2_rcdf_to_dds(
#'       rcdf = rcdf,
#'       coldata = data.frame(
#'         sample = colnames(rcdf),
#'         group = as.factor(c("A", "A", "B"))
#'       ),
#'       design = ~ group
#'     ) |>
#'     ds2_dds_estimate_sizefactor() |>
#'     ds2_dds_estimate_disp()
#'
#'   # Likelihood ratio test
#'   ds2_dds_test_nbinomLRT(dds) |> ds2_dds_to_ddr()
#'
#'   # Wald test
#'   ds2_dds_test_nbinomWaldTest(dds) |> ds2_dds_to_ddr()
#' }
#'
#' @name ds2_test
NULL

#' @rdname ds2_test
#' @export
ds2_dds_test_nbinomLRT <- function(dds, reduced = ~ 1, ...) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_test_nbinomLRT()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  DESeq2::nbinomLRT(dds, reduced = reduced, ...)
}

#' @rdname ds2_test
#' @export
ds2_dds_test_nbinomWaldTest <- function(dds, ...) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_test_nbinomWaldTest()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  DESeq2::nbinomWaldTest(object = dds, ...)
}

#' Get/Set the experimental design
#'
#' @description
#' `ds2_dds_get_design()` gets design.
#'
#' `ds2_dds_set_design()` sets design.
#'
#' @param dds a DESeqDataSet object
#' @param design design
#'
#' @examples
#' if(requireNamespace("DESeq2", quietly = TRUE)) {
#'   # Example read-count data.frame
#'   rcdf <-
#'     system.file(package = "ngsmisc", "deseq2") |>
#'     fs::dir_ls(regexp = "count.csv$") |>
#'     readr::read_csv(show_col_types = FALSE) |>
#'     ds2_tbl_to_rcdf()
#'
#'   # Prepare `DESeqDataSet` object, without specifying `design`.
#'   cd <-
#'     data.frame(
#'       sample = colnames(rcdf),
#'       group = as.factor(c("A", "A", "B"))
#'     )
#'   dds <- ds2_rcdf_to_dds(rcdf = rcdf, coldata = cd)
#'
#'   # Get design formula
#'   dds |> ds2_dds_get_design()
#'
#'   # Set design formula
#'   dds |> ds2_dds_set_design(~ group) |> ds2_dds_get_design()
#' }
#'
#' @name ds2_design
NULL

#' @rdname ds2_design
#' @export
ds2_dds_get_design <- function(dds) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_get_design()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  DESeq2::design(dds)
}

#' @rdname ds2_design
#' @export
ds2_dds_set_design <- function(dds, design) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_set_design()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  DESeq2::design(dds) <- design
  dds
}

#' Compute the normalized count using size-factor, and return as a tibble
#'
#' @description
#' `ds2_dds_get_normalized_count_tbl()` extracts normalized count data as a tibble.
#'
#' @param dds a DESeqDataSet object
#' @param rownames a column name. (default: `"Geneid"`)
#'
#' @examples
#' if(requireNamespace("DESeq2", quietly = TRUE)) {
#'   # Example read-count data.frame
#'   rcdf <-
#'     system.file(package = "ngsmisc", "deseq2") |>
#'     fs::dir_ls(regexp = "count.csv$") |>
#'     readr::read_csv(show_col_types = FALSE) |>
#'     ds2_tbl_to_rcdf()
#'
#'   # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#'   dds <-
#'     ds2_rcdf_to_dds(
#'       rcdf = rcdf,
#'       coldata = data.frame(sample = colnames(rcdf))
#'     ) |>
#'     ds2_dds_estimate_sizefactor()
#'
#'   # Get normalized counts as a tibble
#'   dds |> ds2_dds_get_normalized_count_tbl()
#'
#'   # If you set a manual size-factor, then use it for computing.
#'   dds |>
#'     ds2_dds_set_sizefactor(1:3) |>
#'     ds2_dds_get_normalized_count_tbl()
#' }
#'
#' @export
ds2_dds_get_normalized_count_tbl <- function(dds, rownames = "Geneid") {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_get_normalized_count_tbl()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  sf <- ds2_dds_get_sizefactor(dds)
  df <- as.data.frame(DESeq2::counts(dds))

  if(!identical(sort(colnames(df)), sort(names(sf))))
    stop("Sample names and names of size factors does not match.")

  purrr::modify2(
    .x = df,
    .y = sf[colnames(df)],
    .f = ~ .x / .y
  ) |>
    `row.names<-`(row.names(df)) |>
    tibble::as_tibble(rownames = rownames)
}

#' Convert `DESeq2::DESeqDataSet` class object to other class object
#'
#' @description
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
#' if(requireNamespace("DESeq2", quietly = TRUE)) {
#'   # Example read-count data.frame
#'   rcdf <-
#'     system.file(package = "ngsmisc", "deseq2") |>
#'     fs::dir_ls(regexp = "count.csv$") |>
#'     readr::read_csv(show_col_types = FALSE) |>
#'     ds2_tbl_to_rcdf()
#'
#'   # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#'   dds <-
#'     ds2_rcdf_to_dds(
#'       rcdf = rcdf,
#'       coldata = data.frame(
#'         sample = colnames(rcdf),
#'         group = as.factor(c("A", "A", "B"))
#'       ),
#'       design = ~ group
#'     ) |>
#'     ds2_dds_estimate_sizefactor() |>
#'     ds2_dds_estimate_disp()
#'
#'   ## Convert `DESeqDataSet` to `DESeqResults` or tibble
#'   # Likelihood ratio test
#'   ds2_dds_test_nbinomLRT(dds) |> ds2_dds_to_ddr()
#'   ds2_dds_test_nbinomLRT(dds) |> ds2_dds_to_tbl()
#'
#'   # Wald test
#'   ds2_dds_test_nbinomWaldTest(dds) |> ds2_dds_to_ddr()
#'   ds2_dds_test_nbinomWaldTest(dds) |> ds2_dds_to_tbl()
#' }
#'
#' @name ds2_dds_to
NULL

#' @rdname ds2_dds_to
#' @export
ds2_dds_to_ddr <- function(dds, ...) {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_to_ddr()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  DESeq2::results(object = dds, ...)
}

#' @importFrom rlang :=
#' @rdname ds2_dds_to
#' @export
ds2_dds_to_tbl <- function(dds, rownames = "Geneid") {
  rlang::check_installed("DESeq2", reason = "to use `ds2_dds_to_tbl()`")
  stopifnot(inherits(dds, "DESeqDataSet"))
  test <- attr(dds, which = "test")
  if(!is.null(test) && test %in% c("Wald", "LRT")) {
    dds@rowRanges@elementMetadata |>
      tibble::as_tibble() |>
      dplyr::mutate(
        !!rownames := row.names(DESeq2::counts(dds)),
        .before = 1
      ) |>
      dplyr::mutate(padj = DESeq2::results(dds)$padj)
  } else {
    stop("dds has to be tested.")
  }
}

#' Convert a DESeqResults object to a tibble
#'
#' @description
#' `ds2_ddr_to_tbl()` converts from a `DESeq2::DESeqResults` class object to a tibble.
#'
#' @param ddr a `DESeq2::DESeqResults` class object object
#' @param rownames a column name. (default: `"Geneid"`)
#'
#' @examples
#' if(requireNamespace("DESeq2", quietly = TRUE)) {
#'   # Example read-count data.frame
#'   rcdf <-
#'     system.file(package = "ngsmisc", "deseq2") |>
#'     fs::dir_ls(regexp = "count.csv$") |>
#'     readr::read_csv(show_col_types = FALSE) |>
#'     ds2_tbl_to_rcdf()
#'
#'   # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#'   dds <-
#'     ds2_rcdf_to_dds(
#'       rcdf = rcdf,
#'       coldata = data.frame(
#'         sample = colnames(rcdf),
#'         group = as.factor(c("A", "A", "B"))
#'       ),
#'       design = ~ group
#'     ) |>
#'     ds2_dds_estimate_sizefactor() |>
#'     ds2_dds_estimate_disp()
#'
#'   # Likelihood ratio test
#'   ddr <- ds2_dds_test_nbinomLRT(dds) |> ds2_dds_to_ddr()
#'
#'   # Convert `DESeqResults` to tibble
#'   ddr |> ds2_ddr_to_tbl()
#' }
#'
#' @export
ds2_ddr_to_tbl <- function(ddr, rownames = "Geneid") {
  rlang::check_installed("DESeq2", reason = "to use `ds2_ddr_to_tbl()`")
  stopifnot(inherits(ddr, "DESeqResults"))
  log2FoldChange <- NULL
  tibble::as_tibble(ddr, rownames = rownames) |>
    dplyr::rename(l2fc = log2FoldChange)
}

#' Plot independent filtering
#'
#' @description
#' `ds2_ddr_plot_independent_filtering()` plots the independent filtering result.
#'
#' @param ddr a `DESeq2::DESeqResults` class object object
#' @param title plot title. (default: `""`)
#'
#' @examples
#' if(requireNamespace("DESeq2", quietly = TRUE)) {
#'   # Example read-count data.frame
#'   rcdf <-
#'     system.file(package = "ngsmisc", "deseq2") |>
#'     fs::dir_ls(regexp = "count.csv$") |>
#'     readr::read_csv(show_col_types = FALSE) |>
#'     ds2_tbl_to_rcdf()
#'
#'   # Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
#'   dds <-
#'     ds2_rcdf_to_dds(
#'       rcdf = rcdf,
#'       coldata = data.frame(
#'         sample = colnames(rcdf),
#'         group = as.factor(c("A", "A", "B"))
#'       ),
#'       design = ~ group
#'     ) |>
#'     ds2_dds_estimate_sizefactor() |>
#'     ds2_dds_estimate_disp()
#'
#'   # Likelihood ratio test
#'   ddr <- ds2_dds_test_nbinomLRT(dds) |> ds2_dds_to_ddr()
#'
#'   # Plot the result of independent filtering
#'   ddr |> ds2_ddr_plot_independent_filtering()
#' }
#' @export
ds2_ddr_plot_independent_filtering <- function(ddr, title = "") {
  rlang::check_installed("DESeq2", reason = "to use `ds2_ddr_plot_independent_filtering()`")
  stopifnot(inherits(ddr, "DESeqResults"))
  theta <- numRej <- x <- y <- log2FoldChange <- NULL
  ddr@metadata$filterNumRej |>
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

#' Filter organella reads from readcount data.frame
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#' @inheritParams ds2_rcdf_filter_by_rownames
ds2_rcdf_filter_organella <- function(rcdf) {
  lifecycle::deprecate_warn(
    when = "0.3.0",
    what = "ds2_rcdf_filter_organella()",
    with = "ds2_rcdf_filter_by_rownames()"
  )
  ds2_rcdf_filter_by_rownames(rcdf, "^AT[CM]G")
}
