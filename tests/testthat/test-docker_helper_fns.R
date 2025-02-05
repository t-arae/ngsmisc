
cmd_docker <- "docker run --rm -v {pwd}:/data {image} {command}"
pwd <- "`pwd`"
image <- "some_ngstool"
command <- "some cmd --help"
full_cmd <- glue::glue(cmd_docker)
sep_full_cmd <-
  c("docker", "run", "--rm", "-v", "`pwd`:/data", "some_ngstool",
    "some", "cmd", "--help")

test_that("tokenize_cmd()", {
  # Check interface
  expect_equal(names(formals(tokenize_cmd)), c("statement"))

  # Check output
  expect_no_message(tokenize_cmd(""))

  ws_path <- "'/a/b/c/space is included !'"
  expect_equal(tokenize_cmd(full_cmd), sep_full_cmd)
  expect_equal(tokenize_cmd(glue::glue("cd {ws_path}")),
               c("cd", "'/a/b/c/space is included !'"))
})

stmt1 <- "echo 'hello'"
sep_cmd1 <- tokenize_cmd(statement = stmt1)
stmt2 <- "cat ''"
sep_cmd2 <- tokenize_cmd(statement = stmt2)

test_that("run_cmd()", {
  # Check interface
  expect_equal(names(formals(run_cmd)), c("sep_cmd", "...", "error_on_status", "echo", "spinner"))
  expect_equal(formals(run_cmd)$error_on_status, TRUE)
  expect_equal(eval(formals(run_cmd)$echo), FALSE)
  expect_equal(eval(formals(run_cmd)$spinner), FALSE)
  rlang::with_interactive({
    expect_equal(eval(formals(run_cmd)$echo), TRUE)
    expect_equal(eval(formals(run_cmd)$spinner), TRUE)
  })

  # Check output
  expect_no_message(run_cmd(sep_cmd1))
  expect_no_message(run_cmd(sep_cmd1[1]))
  expect_equal(capture.output(run_cmd(sep_cmd1)), character())
  expect_invisible(run_cmd(sep_cmd1))
  rlang::with_interactive(
    expect_equal(capture.output(run_cmd(sep_cmd1))[1], "'hello'")
  )

  expect_error(run_cmd(sep_cmd2))
  expect_no_message(run_cmd(sep_cmd2, error_on_status = FALSE))

  out <- run_cmd(sep_cmd1)
  expect_equal(class(out), "list")
  expect_equal(out[["status"]], 0L)
  expect_equal(out[["stdout"]], "'hello'\n")
  expect_equal(out[["stderr"]], "")
  expect_equal(out[["timeout"]], FALSE)

  out2 <- run_cmd(sep_cmd2, error_on_status = FALSE)
  expect_equal(out2[["status"]], 1L)
  expect_equal(out2[["stdout"]], "")
  expect_equal(out2[["stderr"]], "cat: '': No such file or directory\n")
  expect_equal(out2[["timeout"]], FALSE)
})

test_that("run_process_and", {
  # Check interface
  expect_equal(names(formals(run_get_stdout)), c("statement", "..."))
  expect_equal(names(formals(run_get_stderr)), c("statement", "..."))

  # Check output
  expect_no_message(run_get_stdout(stmt1))
  expect_no_message(run_get_stderr(stmt2, error_on_status = FALSE))

  expect_equal(run_get_stdout(stmt1), "'hello'\n")
  expect_equal(run_get_stderr(stmt2, error_on_status = FALSE),
               "cat: '': No such file or directory\n")
})

test_that("path_cmdout", {
  # Check interface
  expect_equal(names(formals(path_cmdout)), c("wd", "...", "create_dir", "save_dir"))
  expect_true(formals(path_cmdout)$create_dir)
  expect_equal(
    formals(path_cmdout)$save_dir,
    quote(getOption("ngsmisc.path_cmdout.save_dir", "cmdout_cache"))
  )

  # Check output
  expect_equal(path_cmdout("/path/to/wd", "cmd_out.txt", create_dir = FALSE),
               fs::path("/path/to/wd/cmdout_cache/cmd_out.txt"))
  expect_equal(path_cmdout("/path/to/wd", "cmd_out.txt", create_dir = FALSE, save_dir = "other"),
               fs::path("/path/to/wd/other/cmd_out.txt"))
  expect_equal(path_cmdout("/path/to/wd", "level1", "level2", "cmd_out.txt", create_dir = FALSE, save_dir = "other"),
               fs::path("/path/to/wd/other/level1/level2/cmd_out.txt"))
  expect_equal(path_cmdout(NULL, "level1", "level2", "cmd_out.txt", create_dir = FALSE),
               fs::path("cmdout_cache/level1/level2/cmd_out.txt"))
  expect_equal(path_cmdout("/path/to/wd", "level1", "level2", "cmd_out.txt", create_dir = FALSE, save_dir = NULL),
               fs::path("/path/to/wd/level1/level2/cmd_out.txt"))
  expect_equal(path_cmdout(NULL, "level1", "level2", "cmd_out.txt", create_dir = FALSE, save_dir = NULL),
               fs::path("level1/level2/cmd_out.txt"))

  # Check side-effect
  temp_wd <- tempdir()
  if(fs::dir_exists(temp_wd)) fs::dir_delete(temp_wd)
  path_cmdout(temp_wd, "file")
  expect_true(fs::dir_exists(fs::path(temp_wd, "cmdout_cache")))
  fs::dir_delete(temp_wd)
})

test_that("cache_io", {
  # Check interface
  expect_equal(names(formals(cache_write)), c("x", "wd", "...", "create_dir", "save_dir"))
  expect_true(formals(path_cmdout)$create_dir)
  expect_equal(
    formals(path_cmdout)$save_dir,
    quote(getOption("ngsmisc.path_cmdout.save_dir", "cmdout_cache"))
  )
  expect_equal(names(formals(cache_read)), c("wd", "...", "save_dir"))
  expect_equal(
    formals(path_cmdout)$save_dir,
    quote(getOption("ngsmisc.path_cmdout.save_dir", "cmdout_cache"))
  )

  # Check side-effect
  temp_wd <- tempdir()
  if(fs::dir_exists(temp_wd)) fs::dir_delete(temp_wd)
  expect_no_message(cache_write("hello world.", wd = temp_wd, "test.txt"))
  expect_true(fs::dir_exists(fs::path(temp_wd, "cmdout_cache")))
  expect_true(fs::file_exists(fs::path(temp_wd, "cmdout_cache", "test.txt")))
  expect_equal(readLines(fs::path(temp_wd, "cmdout_cache", "test.txt")),
               "hello world.")
  expect_equal(cache_read(wd = temp_wd, "test.txt"),
               "hello world.")
  fs::dir_delete(temp_wd)
})

test_that("sep_by_blank()", {
  lifecycle::expect_deprecated(sep_by_blank(""))
})

test_that("cmd_run()", {
  lifecycle::expect_deprecated(cmd_run(c("echo", "")))
})
