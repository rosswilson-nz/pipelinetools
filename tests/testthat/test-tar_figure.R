targets::tar_test("tar_figure() works", {
  x <- tar_figure(x, y, "a", height = 4, width = 6)
  expect_equal(x$name, "fig_x")
  expect_equal(
    x$command$string,
    "expression(save_plot(y, \"a\", height = 4, width = 6))"
  )
  expect_equal(x$settings$description, character(0L))
})

targets::tar_test("tar_figure() description", {
  x <- tar_figure(x, y, "a", description = "info")
  expect_equal(x$settings$description, "info")
})

targets::tar_test("tar_figure() defines patterns correctly", {
  x <- tar_figure(x, y, "a", pattern = map(y))
  expect_equal(x$settings$pattern, expression(map(y)))
  expect_equal(x$settings$dimensions, "y")
})

targets::tar_test("tar_figure() sets deployment = 'main' by default", {
  x <- tar_figure(x, y, "a")
  expect_equal(x$settings$deployment, "main")
})

targets::tar_test("tidy eval works", {
  envir <- environment()
  targets::tar_option_set(envir = envir)
  envir$y <- 1
  x <- tar_figure(x, !!y, "a")
  expect_equal(x$command$string, "expression(save_plot(1, \"a\"))")
})

targets::tar_test("can disable tidy eval", {
  x <- tar_figure(x, !!y, "a", tidy_eval = FALSE)
  expect_equal(
    x$command$string,
    "expression(save_plot(expression(!!y), \"a\"))"
  )
})

targets::tar_test("no name", {
  expect_error(
    tar_figure(command = y, filename = "a"),
    class = "tar_condition_validate"
  )
})

targets::tar_test("no command", {
  expect_error(
    tar_figure(x, filename = "a"),
    class = "tar_condition_validate"
  )
})

targets::tar_test("no filename", {
  expect_error(
    tar_figure(x, y),
    class = "tar_condition_validate"
  )
})

targets::tar_test("filename expression", {
  name <- "b"
  x <- tar_figure(x, y, stringr::str_glue("a-", name), height = 4)
  expect_equal(
    x$command$string,
    "expression(save_plot(y, stringr::str_glue(\"a-\", name), height = 4))"
  )
})

targets::tar_test("declaring a target does not run its command", {
  x <- tar_figure(x, y, "a")
  path <- fs::path("output", "_figures", "x")
  expect_false(file.exists(fs::path(path, ext = "svg")))
  expect_false(file.exists(fs::path(path, ext = "pdf")))
  expect_false(file.exists(fs::path(path, ext = "png")))
})
