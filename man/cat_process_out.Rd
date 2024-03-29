% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docker_helper_fns.R
\name{cat_process_out}
\alias{cat_process_out}
\alias{cat_stdout}
\alias{cat_stderr}
\title{Show the stdout/stderr message in processx::run() ouput.}
\usage{
cat_stdout(ps_out)

cat_stderr(ps_out)
}
\arguments{
\item{ps_out}{an output from \code{processx::run()}}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
cmd_ok <- "echo 'hoge hoge'"
cmd_ok \%>\% sep_by_blank() \%>\% cmd_run() \%>\% cat_stdout()

cmd_err <- "cat sonzai_shinai_file.txt"
cmd_err \%>\% sep_by_blank() \%>\% cmd_run(error_on_status = FALSE) \%>\% cat_stderr()

}
