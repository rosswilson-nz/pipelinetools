#' Write outputs to JSON file for Typst import
#'
#' @param x List of R objects
#' @param path JSON file to write `x` to
#' @param auto_unbox,... Passed to `jsonlite::write_json()`
#' @export
export_json <- function(x, path = "output/values.json", auto_unbox = TRUE, ...) {
  jsonlite::write_json(x, path, auto_unbox = auto_unbox, ...)
  path
}
