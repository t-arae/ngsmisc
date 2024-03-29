% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readcount.R
\name{norm_count}
\alias{norm_count}
\alias{calc_rpm}
\alias{calc_rpkm}
\alias{calc_tpm}
\title{Calculate normalized read-counts}
\usage{
calc_rpm(readcount)

calc_rpkm(readcount, len)

calc_tpm(readcount, len)
}
\arguments{
\item{readcount}{read count. atomic vector}

\item{len}{feature length. atomic vector}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{calc_rpm()} calculates Reads Per Million mapped reads (RPM).

\code{calc_rpkm()} calculates Reads Per Kilobase of exon per Million mapped reads (RPKM).

\code{calc_tpm()} calculates Transcripts Per Million (TPM).
}
\examples{

set.seed(123)
tbl <- tibble::tibble(
  readcount = rnbinom(n = 100, size = 5, prob = 0.3),
  len = sample.int(n = 1000, size= 100)
)

tbl \%>\%
  dplyr::mutate(
    rpm = calc_rpm(readcount),
    rpkm = calc_rpkm(readcount, len),
    tpm = calc_tpm(readcount, len),
  )

}
