% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DESeq2.R
\name{ds2_dds_get_normalized_count_tbl}
\alias{ds2_dds_get_normalized_count_tbl}
\title{Compute the normalized count using size-factor, and return as a tibble}
\usage{
ds2_dds_get_normalized_count_tbl(dds, rownames = "Geneid")
}
\arguments{
\item{dds}{a DESeqDataSet object}

\item{rownames}{a column name. (default: \code{"Geneid"})}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{ds2_dds_get_normalized_count_tbl()} extracts normalized count data as a tibble.
}
\examples{
# Example read-count data.frame
rcdf <-
  system.file(package = "ngsmisc", "deseq2") \%>\%
  fs::dir_ls(regexp = "count.csv$") \%>\%
  readr::read_csv(show_col_types = FALSE) \%>\%
  ds2_tbl_to_rcdf()

# Prepare `DESeqDataSet` object, estimate size-factor, and compute dispersion.
dds <-
  ds2_rcdf_to_dds(
    rcdf = rcdf,
    coldata = data.frame(sample = colnames(rcdf))
  ) \%>\%
  ds2_dds_estimate_sizefactor()

# Get normalized counts as a tibble
dds \%>\% ds2_dds_get_normalized_count_tbl()

# If you set a manual size-factor, then use it for computing.
dds \%>\%
  ds2_dds_set_sizefactor(1:3) \%>\%
  ds2_dds_get_normalized_count_tbl()

}
