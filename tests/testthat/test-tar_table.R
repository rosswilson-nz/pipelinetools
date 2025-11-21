targets::tar_test("tar_table() works", {
  y <- 1
  x <- tar_table(x, y, "a")
  expect_equal(x$name, "tbl_x")
  expect_equal(
    x$command$string,
    "expression(save_table(y, \"a\"))"
  )
  expect_equal(x$settings$description, character(0L))
})

targets::tar_test("tar_table() description", {
  y <- 1
  x <- tar_table(x, y, "a", description = "info")
  expect_equal(x$settings$description, "info")
})

targets::tar_test("tar_table() defines patterns correctly", {
  y <- 1
  x <- tar_table(x, y, "a", pattern = map(y))
  expect_equal(x$settings$pattern, expression(map(y)))
  expect_equal(x$settings$dimensions, "y")
})

targets::tar_test("tar_table() sets deployment = 'main' by default", {
  y <- 1
  x <- tar_table(x, y, "a")
  expect_equal(x$settings$deployment, "main")
})

targets::tar_test("tidy eval works", {
  envir <- environment()
  targets::tar_option_set(envir = envir)
  envir$y <- 1
  x <- tar_table(x, !!y, "a")
  expect_equal(x$command$string, "expression(save_table(1, \"a\"))")
})

targets::tar_test("tidy eval works", {
  envir <- environment()
  targets::tar_option_set(envir = envir)
  envir$y <- "a"
  x <- tar_table(x, y, !!y)
  expect_equal(x$command$string, "expression(save_table(y, \"a\"))")
})

targets::tar_test("can disable tidy eval", {
  y <- 1
  x <- tar_table(x, !!y, "a", tidy_eval = FALSE)
  expect_equal(x$command$string, "expression(save_table(!!y, \"a\"))")
})

targets::tar_test("no name", {
  y <- 1
  expect_error(
    tar_table(command = y, filename = "a"),
    class = "tar_condition_validate"
  )
})

targets::tar_test("no command", {
  expect_error(
    tar_table(x, filename = "a"),
    class = "tar_condition_validate"
  )
})

targets::tar_test("no filename", {
  y <- 1
  expect_error(
    tar_table(x, y),
    class = "tar_condition_validate"
  )
})

targets::tar_test("declaring a target does not run its command", {
  y <- 1
  x <- tar_table(x, y, "a")
  path <- fs::path("output", "_tables", "x")
  expect_false(file.exists(fs::path(path, ext = "typ")))
})
