#' Save table to Typst source file
#'
#' Takes a text string specifying a Typst table, and writes it to a Typst file
#'     in `output/_tables/`.
#'
#' Returns the path to the produced Typst source, for use in
#'     `render_manuscript()`.
#'
#' @param table Character string
#' @param filename File name to create on disk (excluding path and extension)
#' @param create_dir Whether to create the output directory if it doesn't exist.
#'
#' @export
save_table <- function(
  table,
  filename,
  create_dir = TRUE
) {
  fs::dir_create("output", "_tables/")
  path <- paste0("output/_tables/", filename, ".typ")
  writeLines(table, path)
  path
}
