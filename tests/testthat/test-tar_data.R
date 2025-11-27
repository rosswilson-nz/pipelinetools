targets::tar_test("tar_data() works", {
  x <- tar_data(mydata, "mydata.csv", read.csv, skip = 1)
  expect_length(x, 2)
  expect_equal(x[[1]]$name, "file_mydata")
  expect_equal(x[[1]]$command$string, "expression(\"raw_data/mydata.csv\")")
  expect_equal(x[[1]]$settings$description, character(0L))
  expect_equal(x[[2]]$name, "raw_mydata")
  expect_equal(
    x[[2]]$command$string,
    "expression(read.csv(file_mydata, skip = 1))"
  )
  expect_equal(x[[2]]$settings$description, character(0L))
})

targets::tar_test("tar_data() receives options", {
  targets::tar_option_set(format = "qs")
  x <- tar_data(mydata, "mydata.csv", read.csv)
  expect_equal(x[[1]]$settings$format, "file")
  expect_equal(x[[2]]$settings$format, "qs")
})

targets::tar_test("tar_data() sets deployment = 'main' by default", {
  x <- tar_data(mydata, "mydata.csv", read.csv)
  expect_equal(x[[1]]$settings$deployment, "main")
  expect_equal(
    x[[2]]$settings$deployment,
    targets::tar_option_get("deployment")
  )
})

targets::tar_test("no name", {
  expect_error(
    tar_data(filename = "mydata.csv", fn = read.csv),
    class = "tar_condition_validate"
  )
})

targets::tar_test("no filename", {
  expect_error(
    tar_data(mydata, fn = read.csv),
    class = "tar_condition_validate"
  )
})

targets::tar_test("filename expression", {
  x <- tar_data(mydata, stringr::str_glue("mydata", "-1.csv"), read.csv)
  expect_equal(x[[1]]$command$string, "expression(\"raw_data/mydata-1.csv\")")
})

targets::tar_test("no fn", {
  expect_error(
    tar_data(mydata, "mydata.csv"),
    class = "tar_condition_validate"
  )
})
