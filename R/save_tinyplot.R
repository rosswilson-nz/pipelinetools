#' Save plot to SVG (and optionally PDF) file
#'
#' A wrapper around `ggplot2::ggsave()` to save plots to SVG file for inclusion
#'     in Typst source documents.
#'
#' Returns the path to the produced SVG file, for use in
#'     `tarchetypes::tar_file()`.
#'
#' @param fn Plot-generating function
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
  fn,
  filename,
  device = NULL,
  device_pdf = NULL,
  device_png = NULL,
  height = NULL,
  width = NULL,
  ...,
  pdf = TRUE,
  png = TRUE,
  create_dir = TRUE
) {
  height <- height %||% 8
  width <- width %||% 10
  fs::dir_create(fs::path("output", "_figures"))

  file_svg <- fs::path("output", "_figures", filename, ext = "svg")
  out <- list(svg = file_svg)
  if (is.null(device)) {
    if (rlang::is_installed("svglite")) {
      device <- svglite::svglite
    } else {
      device <- grDevices::svg
    }
  }
  device(file_svg, width = width, height = height, ...)
  fn()
  dev.off()

  if (pdf) {
    file_pdf <- fs::path("output", "_figures", filename, ext = "pdf")
    out <- append(out, list(pdf = file_pdf))
    if (is.null(device_pdf)) {
      device_pdf <- grDevices::cairo_pdf
    }
    device_pdf(file_pdf, width = width, height = height, ...)
    fn()
    dev.off()
  }

  if (png) {
    file_png <- fs::path("output", "_figures", filename, ext = "png")
    out <- append(out, list(png = file_png))
    if (is.null(device_png)) {
      if (rlang::is_installed("ragg")) {
        device_png <- ragg::agg_png
      } else {
        device_png <- grDevices::png
      }
    }
    dpi <- 300
    device_png(file_png, width = width, height = height, units = "in", ...)
    fn()
    dev.off()
  }

  unlist(out)
}
