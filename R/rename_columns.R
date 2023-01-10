
#' Rename the Nth column name of a data.frame.
#'
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `rename_nth_col()` renames the Nth column name by applying a renamer function.
#'
#' @param tbl a data.frame to rename.
#' @param nth a numeric vector to specify the column index(es) to rename.
#' @param renamer a function to convert column name.
#'
#' @examples
#' infs <-
#'   system.file(package = "ngsmisc", "featurecounts") %>%
#'   fs::dir_ls(regexp = "_gene_counts.txt$")
#'
#' tbl <- fC_read_count(infs[1]) %>% dplyr::select(1, 7)
#' tbl
#' rename_nth_col(tbl, 2, basename)
#' rename_nth_col(tbl, 2, function(x) substr(basename(x), 1, 8))
#'
#' tbl_merge <-
#'   lapply(infs, fC_read_count) %>%
#'   fC_merge_count() %>%
#'   dplyr::select(!c(2:6))
#' tbl_merge
#' rename_nth_col(tbl_merge, 2:4, function(x) substr(basename(x), 1, 8))
#' rename_nth_col(tbl_merge, -c(1:2), function(x) substr(basename(x), 1, 8))
#'
#' @export
rename_nth_col <- function(tbl, nth, renamer) {
  temp <- colnames(tbl)[nth]
  stopifnot(length(colnames(tbl)[nth]) != 0)
  if(length(temp) == 1) {
    if(is.na(temp)) stop("invalid column selection")
  }
  colnames(tbl)[nth] <- renamer(temp)
  tbl
}

#' Return a renaming function for extract labels from file name.
#'
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `renamer_fpath()` returns a utility function to extract a label from a file path.
#'
#' @param prefix a string to specify the removing file prefix. (default: `""`)
#' @param suffix a string to specify the removing file suffix. (default: `""`)
#'
#' @examples
#' renamer <- renamer_fpath()
#' renamer("/path/to/the/example_file1.txt")
#'
#' renamer <- renamer_fpath(prefix = "example_")
#' renamer("/path/to/the/example_file1.txt")
#'
#' renamer <- renamer_fpath(prefix = "example_", suffix = ".txt")
#' renamer("/path/to/the/example_file1.txt")
#'
#' @export
renamer_fpath <- function(prefix = "", suffix = "") {
  function(x) {
    temp <- fs::path_file(x)
    if(prefix != "") {
      temp <- stringr::str_remove(temp, pattern = prefix)
    }
    if(suffix != "") {
      temp <- stringr::str_remove(temp, pattern = suffix)
    }
    temp
  }
}

#' Rename file path in the column name to the label
#'
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `rename_fpath_fastq()`, `rename_fpath_bam` are the renaming functions for
#' a column containing fastq and bam file paths, respectively.
#'
#' @inheritParams rename_nth_col
#'
#' @param prefix a string to specify the removing file prefix. (default: `""`)
#' @param suffix a string to specify the removing file suffix. (defaults are bellow)
#' \itemize{
#'   \item for `rename_fpath()`: `""`
#'   \item for `rename_fpath_fastq()`: `"([.](fastq|fq))([.]gz){0,1}$"`
#'   \item for `rename_fpath_bam()`: `"([.]sort){0,1}([.]bam)$"`
#' }
#'
#' @examples
#' f <- function(...) data.frame(..., check.names = FALSE)
#'
#' f("dir_log/sample1.log" = 1) %>% rename_fpath(nth = 1, suffix = ".log")
#'
#' f("dir_fastq/sample1.fastq" = 1) %>% rename_fpath_fastq(nth = 1)
#' f("dir_fastq/sample1.fq" = 1) %>% rename_fpath_fastq(nth = 1)
#' f("dir_fastq/sample1.fastq.gz" = 1) %>% rename_fpath_fastq(nth = 1)
#' f("dir_fastq/sample1.fq.gz" = 1) %>% rename_fpath_fastq(nth = 1)
#'
#' f("dir_bam/sample1.bam" = 1) %>% rename_fpath_bam(nth = 1)
#' f("dir_bam/sample1.sort.bam" = 1) %>% rename_fpath_bam(nth = 1)
#'
#' @name rename_fpath
NULL

#' @rdname rename_fpath
#' @export
rename_fpath <- function(tbl, nth, prefix = "", suffix = "") {
  rename_nth_col(
    tbl = tbl,
    nth = nth,
    renamer = renamer_fpath(prefix = prefix, suffix = suffix)
  )
}

#' @rdname rename_fpath
#' @export
rename_fpath_fastq <- function(tbl, nth, prefix = "") {
  rename_nth_col(
    tbl = tbl,
    nth = nth,
    renamer = renamer_fpath(prefix = prefix, suffix = "([.](fastq|fq))([.]gz){0,1}$")
  )
}

#' @rdname rename_fpath
#' @export
rename_fpath_bam <- function(tbl, nth, prefix = "") {
  rename_nth_col(
    tbl = tbl,
    nth = nth,
    renamer = renamer_fpath(prefix = prefix, suffix = "([.]sort){0,1}([.]bam)$")
  )
}


#' Rename tibble column names which containing file path
#' @description
#' `r lifecycle::badge("deprecated")`
#' @param tbl data.frame.
#' @param col_fpath integer vector. specified column number which containing file path in its column name
#' @param rm_file_suffix logical. if TRUE, specified file suffix are removed from column name
#' @param file_suffix regex pattern which specified file suffix to remove
#' @export
#'
rename_col_fpath <- function(tbl, col_fpath, rm_file_suffix, file_suffix) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "rename_col_fpath()",
    with = "rename_fpath()"
  )
  temp <-
    colnames(tbl)[col_fpath] %>%
    fs::path_file()
  if(rm_file_suffix) {
    temp <- stringr::str_remove(temp, pattern = file_suffix)
  }
  colnames(tbl)[col_fpath] <- temp
  tbl
}

#' Rename tibble column names which containing AGI
#' @description
#' `r lifecycle::badge("deprecated")`
#' @param tbl data.frame.
#' @param col_AGI integer vector. specified column number which containing file path in its column name
#' @export
#'
rename_col_AGI <- function(tbl, col_AGI) {
  colnames(tbl)[col_AGI] <- "AGI"
  tbl
}
