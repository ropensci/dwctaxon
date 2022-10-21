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
