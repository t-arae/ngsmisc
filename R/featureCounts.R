
#' Read featureCounts read count table
#' @param fpath featureCounts output file path
#' @export
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt$")
#' fC_read_count(infs[1])
#'
fC_read_count <- function(fpath) {
  # Check the first of lines
  fl <- readr::read_lines(file = fpath, n_max = 1L)
  if(!stringr::str_detect(fl, "^# Program:featureCounts v"))
    stop(paste0(fpath, " is not an output from featureCounts"))

  suppressMessages(
    readr::read_tsv(fpath, skip = 1L, col_types = "cccccii")
  )
}

#' Read featureCounts read count summary
#' @param fpath featureCounts output file path
#' @export
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt.summary$")
#' fC_read_summary(infs[1])
#'
fC_read_summary <- function(fpath) {
  purrr::map(fpath, readr::read_tsv, col_types = "ci") %>%
    purrr::reduce(dplyr::left_join, by = "Status")
}

#' Rename featureCounts read count table
#' @param tbl_fC featureCounts read counts tibble
#' @param col_fpath integer. default = 7L
#' @param file_suffix regex pattern. default = ".sort.bam$"
#' @export
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt$")
#' fC_read_count(infs[1]) %>% fC_rename_col()
#'
fC_rename_col <- function(tbl_fC, col_fpath = 7L, file_suffix = ".sort.bam$") {
  tbl_fC <-
    rename_col_fpath(tbl = tbl_fC, col_fpath = col_fpath,
                     rm_file_suffix = TRUE, file_suffix = file_suffix)
  tbl_fC
}

#' Merge featureCounts read count table to a tibble
#' @param tbl_li_fC list of featureCounts read counts tibbles
#' @export
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt$")
#' lapply(infs, fC_read_count) %>% fC_merge_tbl_li()
#'
#'
fC_merge_tbl_li <- function(tbl_li_fC) {
  # Check data identity
  temp <- purrr::map(tbl_li_fC, dplyr::select, -7)
  is_all_equal <-
    purrr::map_lgl(temp, ~ identical(temp[[1]], .x)) %>%
    all()
  if(!is_all_equal)
    stop("All metadata of data.frame in tbl_li_fC must be identical")

  purrr::map(tbl_li_fC[-1], dplyr::select, 7) %>%
    purrr::reduce(dplyr::bind_cols, .init = tbl_li_fC[[1]])
}

#' Calculate RPM/RPKM/TPM from featureCounts read counts
#' @param tbl_fC featureCounts read counts tibble
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt$")
#'
#' lapply(infs, fC_read_count) %>%
#'   fC_merge_tbl_li() %>%
#'   fC_rename_col(col_fpath = -c(1:6)) %>%
#'   fC_calc_rpm() %>%
#'   fC_calc_rpkm() %>%
#'   fC_calc_tpm() %>%
#'   dplyr::glimpse()
#' @name fC_calc
NULL

#' @rdname fC_calc
#' @export
fC_calc_rpm <- function(tbl_fC) {
  len_col <- colnames(tbl_fC) %>% stringr::str_which("^[Ll]ength$")
  dplyr::mutate(
    tbl_fC,
    dplyr::across(
      .cols = !1:6 & !dplyr::matches("^(rpm|rpkm|tpm)_"),
      .fns = ~ calc_rpm(readcount = .x),
      .names = "rpm_{.col}"
    ))
}

#' @rdname fC_calc
#' @export
fC_calc_rpkm <- function(tbl_fC) {
  len_col <- colnames(tbl_fC) %>% stringr::str_which("^[Ll]ength$")
  dplyr::mutate(
    tbl_fC,
    dplyr::across(
      .cols = !1:6 & !dplyr::matches("^(rpm|rpkm|tpm)_"),
      .fns = ~ calc_rpkm(readcount = .x, len = tbl_fC[[!!len_col]]),
      .names = "rpkm_{.col}"
    ))
}

#' @rdname fC_calc
#' @export
fC_calc_tpm <- function(tbl_fC) {
  len_col <- colnames(tbl_fC) %>% stringr::str_which("^[Ll]ength$")
  dplyr::mutate(
    tbl_fC,
    dplyr::across(
      .cols = !1:6 & !dplyr::matches("^(rpm|rpkm|tpm)_"),
      .fns = ~ calc_tpm(readcount = .x, len = tbl_fC[[!!len_col]]),
      .names = "tpm_{.col}"
    ))
}

