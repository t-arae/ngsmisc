
#' common arguments for docker_helpler_fns.R
#'
#' @param statement a command line statement.
#' @param sep_cmd separated command created by `tokenize_cmd()`.
#' @param ps_out an output from `processx::run()`
#' @param ... further options are passed to `processx::run()`.
#'
#' @keywords internal
#'
#' @name common_args_docker_helpler_fns
NULL

#' Separate a shell command statement by white spaces
#'
#' @inheritParams common_args_docker_helpler_fns
#'
#' @examples
#' tokenize_cmd("ls -l -a")
#' tokenize_cmd("echo 'hoge hoge'")
#'
#' @export
tokenize_cmd <- function(statement) {
  chars <- strsplit(statement, "")[[1]]
  tokens <- character()
  current_token <- character()

  flush_token <- function() {
    if (length(current_token) > 0) {
      tokens <<- c(tokens, paste0(current_token, collapse = ""))
      current_token <<- character()
    }
  }
  
  quote_mode <- NULL
  i <- 1L
  while(i < length(chars)+1L) {
    ch <- chars[i]
    if (is.null(quote_mode)) {
      if (ch %in% c("'", '"')) {
        quote_mode <- if (ch == "'") "'" else '"' 
        current_token <- c(current_token, quote_mode)
      } else if (grepl("\\s", ch)) {
        flush_token()
      } else {
        current_token <- c(current_token, ch)
      }
    } else {
      if (quote_mode == ch) {
        current_token <- c(current_token, quote_mode)
        quote_mode <- NULL
      } else {
        current_token <- c(current_token, ch)
      }
    }
    i <- i + 1L
  }
  flush_token()
  return(tokens)
}

#' Run a separated command in external process
#'
#' @inheritParams common_args_docker_helpler_fns
#' @param error_on_status see `processx::run()`
#' @param echo see `processx::run()`
#' @param spinner see `processx::run()`
#'
#' @examples
#' rlang::with_interactive({
#'   cmd_ok <- "echo 'hoge hoge'"
#'   run_cmd(tokenize_cmd(cmd_ok))
#' })
#'
#' rlang::with_interactive({
#'   cmd_err <- "cat sonzai_shinai_file.txt"
#'   run_cmd(tokenize_cmd(cmd_err), error_on_status = FALSE)
#' })
#' 
#' str(run_cmd(tokenize_cmd(cmd_ok)))
#' 
#' str(run_cmd(tokenize_cmd(cmd_err), error_on_status = FALSE))
#'
#' @export
run_cmd <- function(sep_cmd, ..., error_on_status = TRUE, echo = rlang::is_interactive(), spinner = rlang::is_interactive()) {
  invisible(
    processx::run(
      command = sep_cmd[1],
      args = sep_cmd[-1],
      error_on_status = error_on_status,
      echo = echo,
      spinner = spinner,
      ...
    )
  )
}

#' Run a command line and get stdout/stderr outputs from the result.
#'
#' @inheritParams common_args_docker_helpler_fns
#'
#' @examples
#' cmd_ok <- "echo 'hoge hoge'"
#' run_get_stdout(cmd_ok)
#'
#' cmd_err <- "cat sonzai_shinai_file.txt"
#' run_get_stderr(cmd_err, error_on_status = FALSE)
#'
#' @name run_process_and
NULL

#' @rdname run_process_and
#' @export
run_get_stdout <- function(statement, ...) {
  rlang::with_interactive(
    run_cmd(tokenize_cmd(statement), ...)$stdout,
    value = FALSE
  )
}

#' @rdname run_process_and
#' @export
run_get_stderr <- function(statement, ...) {
  rlang::with_interactive(
    run_cmd(tokenize_cmd(statement), ...)$stderr,
    value = FALSE
  )
}

#' Create file path to save the command line output
#'
#' @description
#' Make a file path by inserting a directory between the working directory and
#' specified file path.
#'
#' @param wd A path or character vector to the working directory.
#' @param ... Arguments passed on to `fs::path()`.
#' @param create_dir A logical. if set to `TRUE`, will create directory of the
#'  created file path. (default: `TRUE`)
#' @param save_dir A path or character vector of directory name to save the
#'  cache files. (default: `"cmdout_cache"`, default can be overridden by
#'  setting `options("ngsmisc.path_cmdout.save_dir")`.)
#'
#' @examples
#' path_cmdout("/path/to/wd", "cmd_out.txt", create_dir = FALSE)
#' path_cmdout("/path/to/wd", "cmd_out.txt", create_dir = FALSE, save_dir = "other")
#' path_cmdout("/path/to/wd", "level1", "level2", "cmd_out.txt", create_dir = FALSE)
#' 
#' options("ngsmisc.path_cmdout.save_dir")
#' options(ngsmisc.path_cmdout.save_dir = "new_dir")
#' path_cmdout("/path/to/wd", "cmd_out.txt", create_dir = FALSE)
#'
#' @export
path_cmdout <- function(
    wd, ..., create_dir = TRUE,
    save_dir = getOption("ngsmisc.path_cmdout.save_dir", "cmdout_cache")
) {
  skip_wd <- is.null(wd)
  skip_save_dir <- is.null(save_dir)

  new_fpath <- fs::path(wd, save_dir, ...)
  if(skip_wd) new_fpath <- fs::path(save_dir, ...)
  if(skip_save_dir) new_fpath <- fs::path(wd, ...)
  if(skip_wd & skip_save_dir) new_fpath <- fs::path(...)

  if(create_dir) {
    fs::dir_create(fs::path_dir(new_fpath))
  }

  new_fpath
}

#' Read/Write cached command-line outputs.
#'
#' @description
#' Wrapper functions of `path_cmdout()` to help the text file I/O.
#'
#' @param x A character vector to write to a cache file.
#' @inheritParams path_cmdout
#'
#' @examples
#' NULL
#'
#' @name cache_io

#' @rdname cache_io
#' @export
cache_write <- function(
    x, wd, ..., create_dir = TRUE,
    save_dir = getOption("ngsmisc.path_cmdout.save_dir", "cmdout_cache")
) {
  readr::write_lines(
    x = x,
    file = path_cmdout(wd, ..., create_dir = TRUE, save_dir = save_dir)
  )
}

#' @rdname cache_io
#' @export
cache_read <- function(
    wd, ...,
    save_dir = getOption("ngsmisc.path_cmdout.save_dir", "cmdout_cache")
) {
  readr::read_lines(
    file = path_cmdout(wd, ..., create_dir = FALSE, save_dir = save_dir)
  )
}

#' Separate a statement by white spaces
#'
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams common_args_docker_helpler_fns
#' @export
sep_by_blank <- function(statement) {
  lifecycle::deprecate_warn("0.5.0", "sep_by_blank()", "tokenize_cmd()")
  tokenize_cmd(statement)
}

#' Run a separated command in external process
#'
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams common_args_docker_helpler_fns
#'
#' @export
cmd_run <- function(sep_cmd, ...) {
  lifecycle::deprecate_warn("0.5.0", "cmd_run()", "run_cmd()")
  run_cmd(sep_cmd, ...)
}

#' Show the stdout/stderr message in processx::run() ouput.
#'
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams common_args_docker_helpler_fns
#'
#' @name cat_process_out
NULL

#' @rdname cat_process_out
#' @export
cat_stdout <- function(ps_out) {
  lifecycle::deprecate_warn("0.5.0", "cat_stdout()", "run_cmd(echo = TRUE)")
  cat(ps_out$stdout)
}

#' @rdname cat_process_out
#' @export
cat_stderr <- function(ps_out) {
  lifecycle::deprecate_warn("0.5.0", "cat_stderr()", "run_cmd(echo = TRUE)")
  cat(ps_out$stderr)
}

#' Run a command line and show stdout outputs from the result.
#'
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams common_args_docker_helpler_fns
run_cat_stdout <- function(statement, ...) {
  lifecycle::deprecate_warn("0.5.0", "run_cat_stdout()", "run_cmd(echo = TRUE)")
  rlang::with_interactive(
    cat_stdout(run_cmd(tokenize_cmd(statement), ...)),
    value = FALSE
  )
}

#' Run a command line and show stderr outputs from the result.
#'
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams common_args_docker_helpler_fns
run_cat_stderr <- function(statement, ...) {
  lifecycle::deprecate_warn("0.5.0", "run_cat_stderr()", "run_cmd(echo = TRUE)")
  rlang::with_interactive(
    cat_stderr(run_cmd(tokenize_cmd(statement), ...)),
    value = FALSE
  )
}
