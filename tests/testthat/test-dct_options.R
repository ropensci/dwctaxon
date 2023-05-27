test_that("get_dct_opt() works", {
  expect_equal(
    get_dct_opt("check_taxon_id"),
    TRUE
  )
  expect_error(
    get_dct_opt("whazzat"),
    "'opt' is not the name of an option"
  )
})

test_that("custom checks on options work", {
  expect_error(
    dct_options(valid_tax_status = 1),
    "Option value must be a string"
  )
  expect_error(
    dct_options(valid_tax_status = c("a", "b")),
    "Option value must be a string"
  )
  expect_error(
    dct_options(extra_cols = c(1, 2)),
    "Option value must be a character vector"
  )
  expect_error(
    dct_options(valid_tax_status = NULL),
    "Option value must be a string"
  )
  expect_no_error(
    dct_options(extra_cols = NULL)
  )
})

test_that("dct_options() can be set and unset", {
  expect_equal(
    inherits(dct_options(), "list"),
    TRUE
  )
  dct_options(check_taxon_id = FALSE)
  expect_equal(
    dct_options()$check_taxon_id,
    FALSE
  )
  dct_options(check_taxon_id = TRUE)
  expect_equal(
    dct_options()$check_taxon_id,
    TRUE
  )
  dct_options(reset = TRUE)
  expect_equal(
    dct_options()$check_taxon_id,
    TRUE
  )
})

test_that("do.call works on a list of options", {
  dct_options(reset = TRUE)
  old_settings <- dct_options()
  # check_taxon_id and stamp_modified are both TRUE by default
  dct_options(check_taxon_id = FALSE)
  dct_options(stamp_modified = FALSE)
  expect_equal(
    dct_options()$check_taxon_id,
    FALSE
  )
  expect_equal(
    dct_options()$stamp_modified,
    FALSE
  )
  do.call(dct_options, old_settings)
  expect_equal(
    dct_options()$check_taxon_id,
    TRUE
  )
  expect_equal(
    dct_options()$stamp_modified,
    TRUE
  )
})

dct_options(reset = TRUE)
