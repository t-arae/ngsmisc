
# Test sep_by_blank() ----------------------------------------------------------
cmd_docker <-
  "docker run --rm -v {pwd}:/data {image} {command}"
pwd <- "`pwd`"
image <- "some_ngstool"
command <- "some cmd --help"
full_cmd <- stringr::str_glue(cmd_docker)
sep_full_cmd <-
  c("docker", "run", "--rm", "-v", "`pwd`:/data", "some_ngstool",
    "some", "cmd", "--help")

test_that("sep_by_blank 1", {
  expect_equal(
    sep_by_blank(statement = full_cmd),
    sep_full_cmd
  )
})

ws_path <- "'/a/b/c/space is included !'"

test_that("sep_by_blank 2", {
  expect_equal(
    sep_by_blank(statement = stringr::str_glue("cd {ws_path}")),
    c("cd", "'/a/b/c/space is included !'")
  )
})

# Test cmd_run() ---------------------------------------------------------------
stmt1 <- "echo 'hello'"
out <- sep_by_blank(statement = stmt1) %>% cmd_run(sep_cmd = .)

test_that("cmd_run 1", {
  expect_equal(class(out), "list")
  expect_equal(out[["status"]], 0L)
  expect_equal(out[["stdout"]], "'hello'\n")
  expect_equal(out[["stderr"]], "")
  expect_equal(out[["timeout"]], FALSE)
})

stmt2 <- "cat "
out2 <-
  sep_by_blank(statement = stmt2) %>%
  cmd_run(sep_cmd = ., error_on_status = FALSE)

test_that("cmd_run 2", {
  expect_equal(out2[["status"]], 1L)
  expect_equal(out2[["stdout"]], "")
  expect_equal(out2[["stderr"]], "cat: : No such file or directory\n")
  expect_equal(out2[["timeout"]], FALSE)
})

# Test cat_stdout() ------------------------------------------------------------
test_that("cat_stdout 1", {
  expect_output(cat_stdout(ps_out = out), "'hello'")
})

# Test cat_stderr() ------------------------------------------------------------
test_that("cat_stderr 1", {
  expect_output(cat_stderr(ps_out = out2), "cat: : No such file or directory")
})

# Test run_cat_stdout() ------------------------------------------------------------
test_that("run_cat_stdout 1", {
  expect_output(run_cat_stdout(statement = stmt1), "'hello'")
})

# Test run_cat_stderr() ------------------------------------------------------------
test_that("run_cat_stderr 1", {
  expect_output(run_cat_stderr(statement = stmt2, error_on_status = FALSE),
                "cat: : No such file or directory")
})

# Test run_get_stdout() --------------------------------------------------------
test_that("run_get_stdout", {
  expect_equal(run_get_stdout(statement = stmt1), "'hello'\n")
})

# Test run_get_stderr() --------------------------------------------------------
test_that("run_get_stderr", {
  expect_equal(run_get_stderr(statement = stmt2, error_on_status = FALSE),
               "cat: : No such file or directory\n")
})

# Test path_cmdout() -----------------------------------------------------------
test_that("path_cmdout", {
  expect_equal(path_cmdout("/path/to/wd", "cmd_out.txt", create_dir = FALSE),
               "/path/to/wd/cmdout_cache/cmd_out.txt")
  expect_equal(path_cmdout("/path/to/wd", "cmd_out.txt", create_dir = FALSE, save_dir = "other"),
               "/path/to/wd/other/cmd_out.txt")
  expect_equal(path_cmdout("/path/to/wd", "level1", "level2", "cmd_out.txt", create_dir = FALSE, save_dir = "other"),
               "/path/to/wd/other/level1/level2/cmd_out.txt")

  temp_wd <- fs::path(".", "temptemptemp")
  if(fs::dir_exists(temp_wd)) fs::dir_delete(temp_wd)
  path_cmdout(temp_wd, "file")
  expect_true(fs::dir_exists(fs::path(temp_wd, "cmdout_cache")))
  fs::dir_delete(temp_wd)
})
