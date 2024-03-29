% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/STAR.R
\name{ST_parse_final_log}
\alias{ST_parse_final_log}
\title{Parse STAR final.log files as a list}
\usage{
ST_parse_final_log(fpath)
}
\arguments{
\item{fpath}{a path to the STAR final.log file}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Parse STAR final.log file as a list.
}
\examples{
infs <-
  system.file(package = "ngsmisc", "star") \%>\%
  fs::dir_ls(regexp = ".final.log$")
ST_parse_final_log(infs[1]) \%>\% str(vec.len = 1)

}
