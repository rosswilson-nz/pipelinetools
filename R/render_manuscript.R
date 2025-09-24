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
render_manuscript <- function(path, deps, fig, tbl,
                              template = "reports/_templates/article.typ",
                              bibliography = "reports/references.yaml") {
  # Extract preferred image format
  fig <- purrr::modify_tree(fig, leaf = extract_image)

  # Redefine relative paths for tables and figures
  fig <- purrr::modify_depth(fig, -1, \(x) fs::path_rel(x, "output"))
  tbl <- purrr::modify_depth(tbl, -1, \(x) fs::path_rel(x, "output"))

  # Write table and figure sources to JSON for Typst
  jsonlite::write_json(fig, fs::path("output", "fig.json"), auto_unbox = TRUE)
  jsonlite::write_json(tbl, fs::path("output", "tbl.json"), auto_unbox = TRUE)

  # Temporarily copy Typst source to output directory
  newpath <- fs::path("output", fs::path_rel(path, "reports"))
  fs::file_copy(path, newpath, overwrite = TRUE)

  newtemplate <- fs::path("output", fs::path_rel(template, "reports"))
  fs::dir_create(fs::path_dir(newtemplate))
  fs::file_copy(template, newtemplate, overwrite = TRUE)

  newbibliography <- fs::path("output", fs::path_rel(bibliography, "reports"))
  fs::file_copy(bibliography, newbibliography, overwrite = TRUE)

  deps <- unlist(deps)
  newdeps <- character()
  purrr::walk(
    deps,
    \(d) {
      if (fs::path_has_parent(d, "reports")) {
        new_d <- fs::path("output", fs::path_rel(d, "reports"))
        newdeps <<- c(newdeps, new_d)
        fs::dir_create(fs::path_dir(new_d))
        fs::file_copy(d, new_d, overwrite = TRUE)
      }
    }
  )

  # Output file is the temporary Typst source, with .pdf extension
  output_path <- fs::path_ext_set(newpath, "pdf")

  # Compile using Typst
  stderr <- system2("typst", c("compile", shQuote(newpath)), stderr = TRUE)

  # Remove temporary files in output directory
  fs::file_delete(fs::path_ext_set(fs::path("output", c("fig", "tbl")), "json"))
  fs::file_delete(c(newpath, newtemplate, newdeps, newbibliography))
  fs::dir_delete(fs::path_dir(newtemplate))

  # Pass on any errors or warnings from the Typst compiler
  if (any(vapply(stderr, \(x) stringr::str_detect(x, "^error\\:"), logical(1)))) {
    stop("Error compiling Typst source at ", path, "\n", paste(stderr, collapse = "\n"))
  }
  if (any(vapply(stderr, \(x) stringr::str_detect(x, "^warning\\:"), logical(1)))) {
    warning("Warning from the Typst compiler\n", paste(stderr, collapse = "\n"))
  }

  # Path to file dependencies (output, input, template)
  c(output_path, path, template, bibliography)
}

extract_image <- function(x) {
  x <- as.list(x)
  x$svg %||% x$png %||% x$jpg %||% x$jpeg %||% x$gif
}
