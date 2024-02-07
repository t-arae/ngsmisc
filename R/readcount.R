#' Calculate normalized read-counts
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `calc_rpm()` calculates Reads Per Million mapped reads (RPM).
#'
#' `calc_rpkm()` calculates Reads Per Kilobase of exon per Million mappled reads (RPKM).
#'
#' `calc_tpm()` calculates Transcripts Per kilobase Million (TPM).
#'
#' @param readcount read count. atomic vector
#' @param len feature length. atomic vector
#'
#' @examples
#'
#' set.seed(123)
#' tbl <- tibble::tibble(
#'   readcount = rnbinom(n = 100, size = 5, prob = 0.3),
#'   len = sample.int(n = 1000, size= 100)
#' )
#'
#' tbl %>%
#'   dplyr::mutate(
#'     rpm = calc_rpm(readcount),
#'     rpkm = calc_rpkm(readcount, len),
#'     tpm = calc_tpm(readcount, len),
#'   )
#'
#' @name norm_count
NULL

#' @rdname norm_count
#' @export
calc_rpm <- function(readcount) {
  (readcount / sum(readcount, na.rm = TRUE)) * 10^6
}

#' @rdname norm_count
#' @export
calc_rpkm <- function(readcount, len) {
  readcount * (10^3 / len) * (10^6 / sum(readcount, na.rm = TRUE))
}

#' @rdname norm_count
#' @export
calc_tpm <- function(readcount, len) {
  t <- readcount / len * 10^3
  t / sum(t, na.rm = TRUE) * 10^6
}
