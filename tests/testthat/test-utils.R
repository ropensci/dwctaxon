test_that("drop_first() works", {
  expect_equal(
    drop_first(c(1, 2, 3)),
    c(2, 3)
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
    assert_that_d_wrapper(
      1 == 2,
      data = data.frame(error = "1 not equal to 2"), msg = "1 not equal to 2",
      quiet = TRUE
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
    assert_col(
      data.frame(a = 1), "b",
      req_by = "some_func", on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      check = "some_func",
      error = "some_func requires column b in input data"
    )
  )
  # `req_by` is optional
  expect_equal(
    assert_col(
      data.frame(a = 1), "b",
      on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      error = "Column b required in input data"
    )
  )
  # on_fail = "summary" issues warning
  expect_warning(
    assert_col(
      data.frame(a = 1), "b",
      req_by = "some_func", on_fail = "summary"
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
    assert_col(
      data.frame(a = 1), "a", "character",
      req_by = "some_fun", on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      check = "some_fun",
      error = "Column a must be of class character"
    )
  )
  # `req_by` is optional
  expect_equal(
    assert_col(
      data.frame(a = 1), "a", "character",
      on_fail = "summary", quiet = TRUE
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

test_that("sort_cols_dwc() works", {
  expect_equal(
    sort_cols_dwc(
      data.frame(
        check = "a",
        taxonID = "b",
        scientificName = "c",
        acceptedNameUsageID = "d",
        error = "e"
      )
    ),
    data.frame(
      taxonID = "b",
      acceptedNameUsageID = "d",
      scientificName = "c",
      error = "e",
      check = "a"
    )
  )
})

test_that("val_if_in_dat() works", {
  expect_equal(
    val_if_in_dat(
      data.frame(a = 1),
      "a", 1
    ),
    1
  )
  expect_equal(
    val_if_in_dat(
      data.frame(a = 1),
      "b", 1
    ),
    NA
  )
})

test_that("make_taxon_id_from_sci_name() works", {
  expect_equal(
    make_taxon_id_from_sci_name(
      c(NA, NA),
      c("foo", "bar")
    ),
    c("bd40ef6d", "cbd21009")
  )
  expect_equal(
    make_taxon_id_from_sci_name(
      c(NA, "bat"),
      c("foo", "bar")
    ),
    c("bd40ef6d", "bat")
  )
  expect_error(
    make_taxon_id_from_sci_name(
      c(NA, "bat"),
      c(NA, "bar")
    ),
    "Cannot generate taxon_id from sci_name because sci_name is NA"
  )
})

test_that("is_unique() works", {
  expect_equal(
    is_unique(c(1, 2, NA, NA)),
    TRUE
  )
  expect_equal(
    is_unique(c(1, 2, NA, NA), allow_na = FALSE),
    FALSE
  )
  expect_equal(
    is_unique(c(1, 2, 2, NA)),
    FALSE
  )
  expect_equal(
    is_unique(c(1, 2, 2, NA), allow_na = FALSE),
    FALSE
  )
  expect_equal(
    is_unique(NA),
    TRUE
  )
  expect_equal(
    is_unique(NA, allow_na = FALSE),
    TRUE
  )
})

test_that("check_fill_usage_id_name() works", {
  expect_no_error(check_fill_usage_id_name())
  dct_options(clear_usage_name = FALSE)
  expect_error(
    check_fill_usage_id_name(),
    "clear_usage_name and clear_usage_id are both not TRUE"
  )
  dct_options(reset = TRUE)
})

test_that("Check for zip file ready to download works", {
  # Simulate being offline
  expect_equal(
    safe_to_download(vascan_url, online = FALSE),
    FALSE
  )
  # Rest of tests require an internet connection
  skip_if_offline(host = "r-project.org")
  # URL used in vignette should work
  # - load URL
  source(system.file("extdata", "vascan_url.R", package = "dwctaxon"))
  expect_equal(
    safe_to_download(vascan_url),
    TRUE
  )
  # Check for 404
  expect_equal(
    safe_to_download(
      "https://github.com/joelnitta/i_will_never_make_this_repo"),
    FALSE
  )
  # Check for something that is not a zip file
  expect_equal(
    safe_to_download("https://data.canadensys.net/"),
    FALSE
  )
  # Check for something that is not a zip file
  expect_equal(
    safe_to_download(
      "https://data.canadensys.net/ipt/archive.do?r=vascan&v=WRONGNUMBER"),
    FALSE
  )
})

dct_options(reset = TRUE)
