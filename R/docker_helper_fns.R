### Some helper functions for running docker

#' Separate a statement by white spaces
#' @param statement a command line statement. (character)
#' @export
#' @examples
#' cmd <- "echo 'hoge hoge'"
#' sep_by_blank(cmd)
#'
sep_by_blank <- function(statement) {
  pos_ws <- stringr::str_locate_all(statement, " ")[[1]][,1]
  pos_bt <- stringr::str_locate_all(statement, "`[^`]*`")[[1]]
  pos_sq <- stringr::str_locate_all(statement, "'[^']*'")[[1]]
  pos_dq <- stringr::str_locate_all(statement, '"[^"]*"')[[1]]
  pos_all <- rbind(pos_bt, pos_sq, pos_dq)

  temp <- tibble::tibble(pos_ws, sub = TRUE)
  if(nrow(pos_all) > 0) {
    for(i in 1:nrow(pos_all)) {
      temp <-
        dplyr::mutate(
          temp,
          sub = sub & !(pos_all[i,1] < pos_ws & pos_ws < pos_all[i,2])
        )
    }
  }
  for(i in rev(dplyr::filter(temp, sub)$pos_ws)) {
    stringr::str_sub(statement, i, i) <- "WAKACHI"
  }
  stringr::str_split(statement, "WAKACHI")[[1]]
}

#' Run a separated command in external process
#' @param sep_cmd separated command created by `sep_by_blank()`
#' @param ... options for pass to `processx::run()`
#' @export
#' @examples
#' cmd_ok <- "echo 'hoge hoge'"
#' cmd_run(sep_by_blank(cmd_ok))
#'
#' cmd_err <- "cat sonzai_shinai_file.txt"
#' cmd_run(sep_by_blank(cmd_err), error_on_status = FALSE)
#'
cmd_run <- function(sep_cmd, ...) {
  processx::run(command = sep_cmd[1], args = sep_cmd[-1], ...)
}

#' Show the stdout/stderr message in processx::run() ouput.
#' @description
#' `r lifecycle::badge("experimental")`
#' @param ps_out output from processx::run()
#' @examples
#' cmd_ok <- "echo 'hoge hoge'"
#' cat_stdout(cmd_run(sep_by_blank(cmd_ok)))
#'
#' cmd_err <- "cat sonzai_shinai_file.txt"
#' cat_stderr(cmd_run(sep_by_blank(cmd_err), error_on_status = FALSE))
#'
#' @name cat_process_out
NULL

#' @rdname cat_process_out
#' @export
cat_stdout <- function(ps_out) {
  cat(ps_out$stdout)
}

#' @rdname cat_process_out
#' @export
cat_stderr <- function(ps_out) {
  cat(ps_out$stderr)
}

#' Run a command line and show/get output
#' @description
#' `r lifecycle::badge("experimental")`
#' @inheritParams sep_by_blank
#' @inheritParams cmd_run
#' @examples
#' cmd_ok <- "echo 'hoge hoge'"
#' run_cat_stdout(cmd_ok)
#' run_get_stdout(cmd_ok)
#'
#' cmd_err <- "cat sonzai_shinai_file.txt"
#' run_cat_stderr(cmd_err, error_on_status = FALSE)
#' run_get_stderr(cmd_err, error_on_status = FALSE)
#'
#' @name run_process_and
NULL

#' @rdname run_process_and
#' @export
run_cat_stdout <- function(statement, ...) {
  cat_stdout(cmd_run(sep_by_blank(statement), ...))
}

#' @rdname run_process_and
#' @export
run_cat_stderr <- function(statement, ...) {
  cat_stderr(cmd_run(sep_by_blank(statement), ...))
}

#' @rdname run_process_and
#' @export
run_get_stdout <- function(statement, ...) {
  ngsmisc::cmd_run(ngsmisc::sep_by_blank(statement), ...)$stdout
}

#' @rdname run_process_and
#' @export
run_get_stderr <- function(statement, ...) {
  ngsmisc::cmd_run(ngsmisc::sep_by_blank(statement), ...)$stderr
}

#' Create file path to save the command line output
#' @param wd a path to the working directory
#' @param ... ...
#' @param create_dir logical. if TRUE create directory of the returned path. (default: TRUE)
#' @param save_dir directory name to save the output. (default: "cmdout_cache")
#' @export
#' @examples
#' path_cmdout("/path/to/wd", "cmd_out.txt", create_dir = FALSE)
#' path_cmdout("/path/to/wd", "cmd_out.txt", create_dir = FALSE, save_dir = "other")
#' path_cmdout("/path/to/wd", "level1", "level2", "cmd_out.txt", create_dir = FALSE)
#'
path_cmdout <- function(wd, ..., create_dir = TRUE, save_dir = "cmdout_cache") {
  fpath <- fs::path(wd, ...)
  new_fpath <- stringr::str_replace(fpath, wd, fs::path(wd, save_dir))
  if(create_dir) {
    fs::dir_create(fs::path_dir(new_fpath))
  }
  new_fpath
}

