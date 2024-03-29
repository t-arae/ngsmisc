% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rMATS.R
\name{rM_filter_by_count}
\alias{rM_filter_by_count}
\title{Filter MATS tibble by readcount filtering function}
\usage{
rM_filter_by_count(tbl_MATS, filter_func)
}
\arguments{
\item{tbl_MATS}{A MATS event tibble}

\item{filter_func}{A function which receive a vector of count data and return a bool.}
}
\description{
Filter MATS tibble by readcount filtering function
}
