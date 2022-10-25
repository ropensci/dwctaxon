test_that("paste3() works", {
  expect_equal(
    paste3(1, 2, NA),
    "1 2"
  )
  expect_equal(
    paste3(1, 2, NA, sep = ", "),
    "1, 2"
  )
})

test_that("drop_first() works", {
  expect_equal(
    drop_first(c(1, 2, 3)),
    c(2, 3)
  )
})

test_that("assert_dat() returns expected error", {
  expect_error(
    assert_dat(
      data.frame(a = c(NA, 1)),
      assertr::not_na,
      a
    ),
    "Column 'a' violates assertion 'not_na' 1 time"
  )
})

test_that("assert_that_d() works", {
  # assert_that_d() only works inside a function, so first define
  # the function
  assert_that_d_wrapper <- function(...) {
    assert_that_d(...)
  }
  expect_warning(
    assert_that_d_wrapper(1 == 2, data = data.frame(error = "1 not equal to 2"))
  )
  expect_equal(
    suppressWarnings(
      assert_that_d_wrapper(
        1 == 2,
        data = data.frame(error = "1 not equal to 2"), msg = "1 not equal to 2"
      )
    ),
    data.frame(error = "1 not equal to 2")
  )
  expect_equal(
    assert_that_d_wrapper(1 != 2),
    TRUE
  )
})

test_that("assert_col() don't error on correct data", {
  expect_no_error(assert_col(data.frame(a = "a"), "a", "character"))
  expect_no_error(assert_col(data.frame(a = "a"), "a"))
  # - note that column only needs to inherit *one* of the classes
  expect_no_error(
    assert_col(data.frame(a = 1), "a", c("character", "numeric"))
  )
  expect_error(
    assert_col(data.frame(a = 1), "b", "numeric"),
    "Column b required in input data"
  )
})

test_that("assert_col() detects missing column", {
  # on_fail = "summary" returns df
  expect_equal(
    suppressWarnings(
      assert_col(
        data.frame(a = 1), "b", req_by = "some_func", on_fail = "summary"
      )
    ),
    tibble::tibble(
      check = "some_func",
      error = "some_func requires column b in input data"
    )
  )
  # `req_by` is optional
  expect_equal(
    suppressWarnings(
      assert_col(
        data.frame(a = 1), "b", on_fail = "summary"
      )
    ),
    tibble::tibble(
      error = "Column b required in input data"
    )
  )
  # on_fail = "summary" issues warning
  expect_warning(
    assert_col(
        data.frame(a = 1), "b", req_by = "some_func", on_fail = "summary"
      ),
    "some_func requires column b in input data"
  )
  # on_fail = "error" issues error
  expect_error(
    assert_col(data.frame(a = 1), "b"),
    "Column b required in input data"
  )
})

test_that("assert_col() detects column of wrong class", {
  # on_fail = "summary" returns df
  expect_equal(
    suppressWarnings(
      assert_col(
        data.frame(a = 1), "a", "character",
        req_by = "some_fun", on_fail = "summary")
    ),
    tibble::tibble(
      check = "some_fun",
      error = "Column a must be of class character"
    )
  )
  # `req_by` is optional
  expect_equal(
    suppressWarnings(
      assert_col(
        data.frame(a = 1), "a", "character", on_fail = "summary")
    ),
    tibble::tibble(
      error = "Column a must be of class character"
    )
  )
  # on_fail = "summary" issues warning
  expect_warning(
    assert_col(
      data.frame(a = 1), "a", "character",
      req_by = "some_fun", on_fail = "summary"
    ),
    "Column a must be of class character"
  )
  # on_fail = "error" issues error
  expect_error(
    assert_col(data.frame(a = 1), "a", "character"),
    "Column a must be of class character"
  )
  # Checks for multiple classes issue correctly formatted warnings
  expect_error(
    assert_col(
      data.frame(a = complex(1)),
      "a",
      c("character", "numeric")
    ),
    "Column a must be of class character or numeric"
  )
  expect_error(
    assert_col(
      data.frame(a = complex(1)),
      "a",
      c("character", "numeric", "integer")
    ),
    "Column a must be of class character, numeric, or integer"
  )
})

test_that("bind_rows_f() works", {
  expect_equal(
    bind_rows_f(
      list(data.frame(a = 1), TRUE, data.frame(b = 2), data.frame(a = 3))
    ),
    data.frame(
      a = c(1, NA, 3),
      b = c(NA, 2, NA)
    )
  )
})

test_that("any_not_true() works", {
  expect_equal(
    any_not_true(list(TRUE, TRUE)),
    FALSE
  )
  expect_equal(
    any_not_true(list(TRUE, "a")),
    TRUE
  )
  expect_equal(
    any_not_true(list(TRUE, data.frame(a = 1))),
    TRUE
  )
})

test_that("null_transformer() works", {
  expect_equal(
    glue::glue("hi {NULL}", .transformer = null_transformer("there")),
    "hi there"
  )
  expect_equal(
    glue::glue("hi {}", .transformer = null_transformer("there")),
    "hi there"
  )
})

test_that("make_msg() works", {
  expect_equal(
    make_msg("taxonID", c(1, 2), is_last = TRUE),
    "Bad taxonID: 1, 2"
  )
  expect_equal(
    make_msg("taxonID", c(1, 2)),
    "Bad taxonID: 1, 2\n"
  )
})
