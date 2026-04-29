#' Render Typst manuscript to PDF
#'
#' Render a Typst source file to PDF, and return file path dependencies for
#'     `targets` pipeline
#'
#' @param path Path to Typst source file
#' @param deps List of dependencies (targets from the plan)
#' @param fig,tbl Lists of figures and tables referenced in the manuscript
#' @param template Typst template
#' @param bibliography Bibliography file
#'
#' @export
render_manuscript <- function(
  path,
  deps = list(),
  fig = list(),
  tbl = list(),
  template = "output/_templates/article.typ",
  bibliography = "output/references.yaml"
) {
  if (length(fig)) {
    # Extract preferred image format
    fig <- purrr::modify_tree(fig, leaf = extract_image)

    # Redefine relative paths
    fig <- purrr::modify_depth(fig, -1, \(x) fs::path_rel(x, "output"))

    # Write figure sources to JSON for Typst
    fig_json <- fs::path("output", "fig.json")
    jsonlite::write_json(fig, fig_json, auto_unbox = TRUE)
  } else {
    fig_json <- character(0)
  }
  if (length(tbl)) {
    # Redefine relative paths
    tbl <- purrr::modify_depth(tbl, -1, \(x) fs::path_rel(x, "output"))

    # Write table sources to JSON for Typst
    tbl_json <- fs::path("output", "tbl.json")
    jsonlite::write_json(tbl, tbl_json, auto_unbox = TRUE)
  } else {
    tbl_json <- character(0)
  }

  # Output file is the same as the Typst source, with .pdf extension instead of .typ
  output_path <- fs::path_ext_set(path, "pdf")

  # Compile using Typst
  stderr <- system2(
    "typst",
    c("compile", shQuote(path)),
    stderr = TRUE,
    stdout = TRUE
  )

  # Pass on any errors or warnings from the Typst compiler
  if (
    any(vapply(stderr, \(x) stringr::str_detect(x, "^error\\:"), logical(1)))
  ) {
    stop(
      "Error compiling Typst source at ",
      path,
      "\n",
      stringr::str_c(stderr, collapse = "\n")
    )
  }
  if (
    any(vapply(stderr, \(x) stringr::str_detect(x, "^warning\\:"), logical(1)))
  ) {
    warning(
      "Warning from the Typst compiler\n",
      stringr::str_c(stderr, collapse = "\n")
    )
  }

  # Path to file dependencies (output, input, template)
  c(output_path, path, template, bibliography)
}

extract_image <- function(x) {
  get_image_path(x, "svg") %||%
    get_image_path(x, "png") %||%
    get_image_path(x, "jpe?g") %||%
    get_image_path(x, "gif")
}

get_image_path <- function(x, ext) {
  pattern <- stringr::str_glue("\\.{ext}$")
  if (length(grepv(pattern, x, ignore.case = TRUE))) {
    grepv(pattern, x, ignore.case = TRUE)[[1]]
  } else {
    NULL
  }
}
