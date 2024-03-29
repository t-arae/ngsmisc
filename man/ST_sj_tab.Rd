% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/STAR.R
\name{ST_sj_tab}
\alias{ST_sj_tab}
\alias{ST_read_sj_tab}
\title{Read STAR SJ.out.tab file as a tibble}
\usage{
ST_read_sj_tab(fpath, decode = FALSE)
}
\arguments{
\item{fpath}{a path to the STAR final.log file}

\item{decode}{logical. If \code{TRUE} decode values in 3rd-5th columns. (default: \code{FALSE})}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Read (and decode data) STAR SJ.out.tab file as a tibble.
}
\details{
See the section 5.5 "Splice junctions" in the STAR manual.
}
\examples{
infs <-
  system.file(package = "ngsmisc", "star") \%>\%
  fs::dir_ls(regexp = ".sj.tsv$")
ST_read_sj_tab(infs[1])
ST_read_sj_tab(infs[1], decode = TRUE)

# it can be converted to the `GenomicRanges::GRanges-class` object
# ST_read_sj_tab(infs[1], decode = TRUE) \%>\% plyranges::as_granges()

tbl_merge <-
  infs \%>\%
  lapply(ST_read_sj_tab, decode = TRUE) \%>\%
  lapply(head) \%>\%
  purrr::imap(~ dplyr::mutate(.x, sample = fs::path_file(.y))) \%>\%
  dplyr::bind_rows()
tbl_merge

}
