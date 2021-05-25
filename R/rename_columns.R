
#' Rename tibble column names which containing file path
#' @param tbl data.frame.
#' @param col_fpath integer vector. specified column number which containing file path in its column name
#' @param rm_file_suffix logical. if TRUE, specified file suffix are removed from column name
#' @param file_suffix regex pattern which specified file suffix to remove
#' @export
#'
rename_col_fpath <- function(tbl, col_fpath, rm_file_suffix, file_suffix) {
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
#' @param tbl data.frame.
#' @param col_AGI integer vector. specified column number which containing file path in its column name
#' @export
#'
rename_col_AGI <- function(tbl, col_AGI) {
  colnames(tbl)[col_AGI] <- "AGI"
  tbl
}
