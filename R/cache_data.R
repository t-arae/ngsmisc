#' Cache an object into a file and load it into global environment
#' @param fpath file path
#' @param object an object or an expression which return object
#' @param fun_load a function to load object from file
#' @param fun_save a function to write object into file.
#' @export 
#' @examples
#' \dontrun{
#' # cache iris to the file
#' cache_data("iris.rds", iris)
#'
#' # cache head or iris to the file
#' cache_data("iris_head.rds", {
#'   iris |>
#'     head(10)
#' })
#' iris_head
#'
#' # cache head of iris to the file with save and load function
#' cache_data("iris_head.csv",
#'   fun_save = \(x, y) write.csv(x, y),
#'   fun_load = read.csv,
#'   {
#'     iris |>
#'       head(10)
#'   }
#' )
#' iris_head
#' }
#'
cache_data <- function(
    fpath, object, fun_load = readRDS,
    fun_save = \(x, y) saveRDS(object = x, file = y)) {
  if (!fs::file_exists(fpath)) {
    fun_save(object, fpath)
  }
  pos <- 1
  assign(
    fs::path_ext_remove(fs::path_file(fpath)),
    fun_load(fpath),
    envir = as.environment(pos)
  )
}
