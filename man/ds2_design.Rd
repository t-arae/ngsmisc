% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DESeq2.R
\name{ds2_design}
\alias{ds2_design}
\alias{ds2_dds_get_design}
\alias{ds2_dds_set_design}
\title{Get/Set the experimental design}
\usage{
ds2_dds_get_design(dds)

ds2_dds_set_design(dds, design)
}
\arguments{
\item{dds}{a DESeqDataSet object}

\item{design}{design}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{ds2_dds_get_design()} gets design.

\code{ds2_dds_set_design()} sets design.
}
\examples{
# Example read-count data.frame
rcdf <-
  system.file(package = "ngsmisc", "deseq2") \%>\%
  fs::dir_ls(regexp = "count.csv$") \%>\%
  readr::read_csv(show_col_types = FALSE) \%>\%
  ds2_tbl_to_rcdf()

# Prepare `DESeqDataSet` object, without specifying `design`.
cd <-
  data.frame(
    sample = colnames(rcdf),
    group = as.factor(c("A", "A", "B"))
  )
dds <- ds2_rcdf_to_dds(rcdf = rcdf, coldata = cd)

# Get design formula
dds \%>\% ds2_dds_get_design()

# Set design formula
dds \%>\% ds2_dds_set_design(~ group) \%>\% ds2_dds_get_design()

}
