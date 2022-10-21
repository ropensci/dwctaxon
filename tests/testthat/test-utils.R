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

test_that("assert_col() works", {
  expect_no_error(assert_col(data.frame(a = "a"), "a", "character"))
  expect_no_error(assert_col(data.frame(a = "a"), "a"))
  # note that column only needs to inherit *one* of the classes
  expect_no_error(
    assert_col(data.frame(a = 1), "a", c("character", "numeric"))
  )
  expect_error(
    assert_col(data.frame(a = 1), "b", "numeric"),
    "Column 'b' required in input data"
  )
  expect_error(
    assert_col(data.frame(a = 1), "a", "character"),
    "Column 'a' must be of class 'character'"
  )
  expect_error(
    assert_col(data.frame(a = 1), "b", "character"),
    "Column 'b' required in input data"
  )
  expect_no_error(
    assert_col(data.frame(a = 1), "b", "character", req_col = FALSE)
  )
  expect_error(
    assert_col(
      data.frame(a = complex(1)),
      "a",
      c("character", "numeric")
    ),
    "Column 'a' must be of class 'character' or 'numeric"
  )
  expect_error(
    assert_col(
      data.frame(a = complex(1)),
      "a",
      c("character", "numeric", "integer")
    ),
    "Column 'a' must be of class 'character', 'numeric', or 'integer'"
  )
})
