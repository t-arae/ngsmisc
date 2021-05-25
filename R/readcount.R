
#' Calculate Reads Per Million mapped reads (RPM)
#' @param readcount read count. atomic vector
#' @export
calc_rpm <- function(readcount) {
  (readcount / sum(readcount, na.rm = T)) * 10^6
}

#' Calculate Reads Per Kilobase of exon per Million mappled reads (RPKM)
#' @param readcount read count. atomic vector
#' @param len feature length. atomic vector
#' @export
calc_rpkm <- function(readcount, len) {
  readcount * (10^3 / len) * (10^6 / sum(readcount, na.rm = T))
}

#' Calculate Transcripts Per kilobase Million (TPM)
#' @param readcount read count. atomic vector
#' @param len feature length. atomic vector
#' @export
calc_tpm <- function(readcount, len) {
  t <- readcount / len * 10^3
  t / sum(t, na.rm = T) * 10^6
}
