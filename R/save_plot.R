#' Save plot to SVG (and optionally PDF) file
#'
#' A wrapper around `ggplot2::ggsave()` to save plots to SVG file for inclusion
#'     in Typst source documents.
#'
#' Returns the path to the produced SVG file, for use in
#'     `tarchetypes::tar_file()`.
#'
#' @param plot Plot to save
#' @param filename File name to create on disk
#' @param device (optional) Device to use for SVG output
#' @param device_pdf (optional) Device to use for PDF output
#' @param device_png (optional) Device to use for PNG output
#' @param height,width Plot size in inches (by default; use `units = ` to
#'     alternatively specify `"cm"`, `"mm`", or `"px"`).
#' @param ... Passed through to `ggplot2::ggsave()`
#' @param pdf Whether to produce PDF output (as well as SVG).
#' @param png Whether to produce PNG output (as well as SVG).
#' @param create_dir Whether to create the output directory if it doesn't exist.
#'
#' @export
save_plot <- function(
  plot,
  filename,
  device = NULL,
  device_pdf = NULL,
  device_png = NULL,
  height = NA,
  width = NA,
  ...,
  pdf = TRUE,
  png = TRUE,
  create_dir = TRUE
) {
  file_svg <- fs::path("output", "_figures", filename, ext = "svg")
  out <- list(svg = file_svg)
  if (is.null(device)) {
    if (rlang::is_installed("svglite")) {
      device <- svglite::svglite
    } else {
      device <- grDevices::svg
    }
  }
  ggplot2::ggsave(
    file_svg,
    plot,
    device,
    height = height,
    width = width,
    create.dir = create_dir,
    ...
  )

  if (pdf) {
    if (is.null(device_pdf)) {
      device_pdf <- grDevices::cairo_pdf
    }
    file_pdf <- fs::path("output", "_figures", filename, ext = "pdf")
    out <- append(out, list(pdf = file_pdf))
    ggplot2::ggsave(
      file_pdf,
      plot,
      device_pdf,
      height = height,
      width = width,
      create.dir = create_dir,
      ...
    )
  }

  if (png) {
    if (is.null(device_png)) {
      if (rlang::is_installed("ragg")) {
        device_png <- ragg::agg_png
      } else {
        device_png <- grDevices::png
      }
    }
    file_png <- fs::path("output", "_figures", filename, ext = "png")
    out <- append(out, list(png = file_png))
    ggplot2::ggsave(
      file_png,
      plot,
      device_png,
      height = height,
      width = width,
      create.dir = create_dir,
      ...
    )
  }

  unlist(out)
}
