#' Read featureCounts read count table as tibbles and merge them
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `fC_read_count()` reads a featureCounts read-count output.
#'
#' `fC_merge_count()` merges a list of tibbles read by `fC_read_count()` to a tibble.
#'
#' @param fpath featureCounts output file path
#' @param li_tbl A list of featureCounts read counts tibbles
#'
#' @examples
#' # example read-count files
#' infs <-
#'   system.file(package = "ngsmisc", "featurecounts") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt$")
#'
#' # show first six lines
#' readLines(infs[1], n = 6) %>% cat(sep = "\n")
#'
#' # read a read-count file
#' fC_read_count(infs[1])
#'
#' # read read-count files and merge them into a tibble
#' lapply(infs, fC_read_count) %>%
#'   lapply(rename_fpath_bam, nth = 7) %>%
#'   fC_merge_count()
#'
#' @name fC_count
NULL

#' @rdname fC_count
#' @export
fC_read_count <- function(fpath) {
  # Check the first of lines
  fl <- readr::read_lines(file = fpath, n_max = 1L)
  if(!stringr::str_detect(fl, "^# Program:featureCounts v"))
    stop(paste0("fpath must be an output from featureCounts"))

  suppressMessages(
    readr::read_tsv(fpath, skip = 1L, col_types = "cccccii")
  )
}

#' @rdname fC_count
#' @export
fC_merge_count <- function(li_tbl) {
  # Check data identity
  temp <- purrr::map(li_tbl, dplyr::select, -7)
  is_all_equal <-
    purrr::map_lgl(temp, ~ identical(temp[[1]], .x)) %>%
    all()
  if(!is_all_equal)
    stop("All metadata of data.frame in li_tbl must be identical")

  purrr::map(li_tbl[-1], dplyr::select, 7) %>%
    purrr::reduce(dplyr::bind_cols, .init = li_tbl[[1]])
}

#' Write selected columns (id/column/rpm/rpkm/tpm) in a tibble to a csv file.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `fC_write_count()` writes several columns in featureCounts read-count tibble to csv file.
#'
#' @param tbl_fC a tibble read from featureCounts read-count output.
#' @param fpath an output csv file path.
#'
#' @examples
#' # example read-count files
#' infs <-
#'   system.file(package = "ngsmisc", "featurecounts") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt$")
#'
#' # read read-count files and merge them into a tibble
#' lapply(infs, fC_read_count) %>%
#'   lapply(rename_fpath_bam, nth = 7) %>%
#'   fC_merge_count() %>%
#'   fC_write_count("./count.csv")
#' readr::read_csv("./count.csv")
#' fs::file_delete("./count.csv")
#'
#' @export
fC_write_count <- function(tbl_fC, fpath) {
  dplyr::select(tbl_fC, !c(2:6)) %>%
    readr::write_csv(file = fpath)
}

#' Read featureCounts read count summary as tibbles and merge them
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `fC_read_summary()` reads a featureCounts summary file.
#'
#' @param fpath featureCounts summary output file path
#' @param li_tbl A list of featureCounts summary tibbles
#'
#' @examples
#' # example summary files
#' infs <-
#'   system.file(package = "ngsmisc", "featurecounts") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt.summary$")
#'
#' # show first six lines
#' readLines(infs[1], n = 6) %>% cat(sep = "\n")
#'
#' # read a summary file
#' fC_read_summary(infs[1])
#'
#' # read summary files and merge them into a tibble
#' lapply(infs, fC_read_summary) %>%
#'   lapply(rename_fpath_bam, nth = 2) %>%
#'   fC_merge_summary()
#'
#' @name fC_summary
NULL

#' @rdname fC_summary
#' @export
fC_read_summary <- function(fpath) {
  purrr::map(fpath, readr::read_tsv, col_types = "ci") %>%
    purrr::reduce(dplyr::left_join, by = "Status")
}

#' @rdname fC_summary
#' @export
fC_merge_summary <- function(li_tbl) {
  # Check data identity
  temp <- purrr::map(li_tbl, dplyr::select, 1)
  is_all_equal <-
    purrr::map_lgl(temp, ~ identical(temp[[1]], .x)) %>%
    all()
  if(!is_all_equal)
    stop("All metadata of data.frame in li_tbl must be identical")

  purrr::map(li_tbl[-1], dplyr::select, 2) %>%
    purrr::reduce(dplyr::bind_cols, .init = li_tbl[[1]])
}

#' Calculate RPM/RPKM/TPM from featureCounts read counts
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `fC_calc_rpm/rpkm/tpm()` calculate normalized expression levels.
#'
#' @param tbl_fC featureCounts read counts tibble
#'
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc", "featurecounts") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt$")
#'
#' lapply(infs, fC_read_count) %>%
#'   fC_merge_count() %>%
#'   rename_fpath_bam(nth = -c(1:6)) %>%
#'   fC_calc_rpm() %>%
#'   fC_calc_rpkm() %>%
#'   fC_calc_tpm() %>%
#'   dplyr::glimpse()
#'
#' @name fC_calc
NULL

#' @rdname fC_calc
#' @export
fC_calc_rpm <- function(tbl_fC) {
  len_col <- colnames(tbl_fC) %>% stringr::str_which("^[Ll]ength$")
  dplyr::mutate(
    tbl_fC,
    dplyr::across(
      .cols = !seq_len(6) & !dplyr::matches("^(rpm|rpkm|tpm)_"),
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
      .cols = !seq_len(6) & !dplyr::matches("^(rpm|rpkm|tpm)_"),
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
      .cols = !seq_len(6) & !dplyr::matches("^(rpm|rpkm|tpm)_"),
      .fns = ~ calc_tpm(readcount = .x, len = tbl_fC[[!!len_col]]),
      .names = "tpm_{.col}"
    ))
}

#' Rename featureCounts read count table
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `fC_rename_col()` renames the 7th column (file path).
#'
#' @keywords internal
#'
#' @param tbl_fC featureCounts read counts tibble
#' @param col_fpath integer. default = 7L
#' @param file_suffix regex pattern. default = ".sort.bam$"
#'
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc", "featurecounts") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt$")
#' fC_read_count(infs[1]) %>% fC_rename_col()
#'
#' @export
fC_rename_col <- function(tbl_fC, col_fpath = 7L, file_suffix = ".sort.bam$") {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "fC_rename_col()",
    with = "rename_fpath_bam()"
  )
  tbl_fC <-
    rename_col_fpath(tbl = tbl_fC, col_fpath = col_fpath,
                     rm_file_suffix = TRUE, file_suffix = file_suffix)
  tbl_fC
}
