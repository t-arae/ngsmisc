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
#' @param sep_cmd separated command created by sep_by_blank()
#' @param ... options for pass to processx::run()
#' @export
#' @examples
#' cmd_ok <- "echo 'hoge hoge'"
#' cmd_run(sep_by_blank(cmd_ok))
#'
#' cmd_err <- "sleep 100"
#' cmd_run(sep_by_blank(cmd_err), timeout = 1, error_on_status = FALSE)
#'
cmd_run <- function(sep_cmd, ...) {
  processx::run(command = sep_cmd[1], args = sep_cmd[-1], ...)
}

#' Show stdout messages in the process output
#' @param ps_out output from processx::run()
#' @export
#'
cat_stdout <- function(ps_out) {
  cat(ps_out$stdout)
}

#' Show stderr messages in the process output
#' @param ps_out output from processx::run()
#' @export
#'
cat_stderr <- function(ps_out) {
  cat(ps_out$stderr)
}

#' Run a statement with showing stdout messages
#' @inheritParams sep_by_blank
#' @inheritParams cmd_run
#' @export
#'
run_cat_stdout <- function(statement, ...) {
  cat_stdout(cmd_run(sep_by_blank(statement), ...))
}

#' Run a statement with showing stdout messages
#' @inheritParams sep_by_blank
#' @inheritParams cmd_run
#' @export
#'
run_cat_stderr <- function(statement, ...) {
  cat_stderr(cmd_run(sep_by_blank(statement), ...))
}

