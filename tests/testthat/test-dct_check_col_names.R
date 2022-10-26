test_that("properly formatted data passes", {
  expect_no_error(
    check_col_names_p(data.frame(taxonID = "a", namePublishedInID = "b"))
  )
})

test_that("check for 'all columns must have valid names' works", {
  expect_error(
    check_col_names_p(data.frame(a = 1)),
    paste0(
      "check_col_names failed.*",
      "Invalid column name\\(s\\) detected.*",
      "Bad column names\\: a"
    )
  )
  expect_equal(
    suppressWarnings(
      check_col_names_p(data.frame(a = 1), on_fail = "summary")
    ),
    tibble::tibble(
      error = "Invalid column names detected: a",
      check = "check_col_names"
    )
  )
})
