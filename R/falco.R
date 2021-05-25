
#' Read falco or fastqc output file (fastqc_data.txt)
#' @param fpath path to file
#' @export
#'
falco_read_falco_data <- function(fpath) {
  input <- readr::read_lines(fpath)
  stringr::str_which(input, "^>>")

  cn_list <- c(MATS.JC_COLNAMES, MATS.JCEC_COLNAMES)
  ct_list <- c(MATS.JC_COLTYPES, MATS.JCEC_COLTYPES)
  temp_file <- fs::path_file(path = fpath)

  # Check column name
  cn <- cn_list[[temp_file]]
  header <-
    readr::read_lines(file = fpath, n_max = 1L) %>%
    strsplit(split = "\t")
  if(!identical(cn, header[[1]])) stop("found column name mismatch")

  df <- suppressWarnings(
    readr::read_tsv(file = fpath, col_types = ct_list[[temp_file]])
  )
  # colnames(df) <- cn
  df
}
