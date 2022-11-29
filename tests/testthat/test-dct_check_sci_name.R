test_that("Checks on input data format work", {
  expect_error(
    dct_check_sci_name(data.frame(a = 1), on_fail = c(1, 2)),
    "on_fail is not a string"
  )
  expect_error(
    dct_check_sci_name(data.frame(a = 1), on_success = c(1, 2)),
    "on_success is not a string"
  )
  expect_error(
    dct_check_sci_name(data.frame(a = 1), on_fail = "symmury"),
    "on_fail must be one of 'error' or 'summary'"
  )
  expect_error(
    dct_check_sci_name(data.frame(a = 1), on_success = "summary"),
    "on_success must be one of 'data' or 'logical'"
  )
  expect_error(
    dct_check_sci_name("a"),
    "'tax_dat' must be of class 'data.frame'"
  )
})

test_that("Correctly formatted data does not error", {
  good_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", NA, "accepted", "Species bat"
  )
  expect_no_error(dct_check_sci_name(good_dat))
  expect_equal(dct_check_sci_name(good_dat), good_dat)
  expect_equal(dct_check_sci_name(good_dat, on_success = "logical"), TRUE)
})

test_that("check for correctly formatted columns works", {
  expect_error(
    dct_check_sci_name(data.frame(taxonID = "foo bar")),
    "check_sci_name requires column scientificName in input data"
  )
  expect_error(
    dct_check_sci_name(
      data.frame(scientificName = 1)
    ),
    "Column scientificName must be of class character"
  )
  expect_equal(
    dct_check_sci_name(
      data.frame(taxonID = "foo bar"),
      on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      check = "check_sci_name",
      error = "check_sci_name requires column scientificName in input data"
    )
  )
  expect_equal(
    dct_check_sci_name(
      data.frame(scientificName = 1),
      on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      check = "check_sci_name",
      error = "Column scientificName must be of class character"
    )
  )
})

test_that("check for 'sci name cannot have missing values' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "accepted", NA,
    "3", NA, "accepted", "Species bat"
  )
  expect_error(
    check_sci_name_not_na(bad_dat, on_fail = "error"),
    paste(
      "check_sci_name failed.*",
      "scientificName detected with missing value.*",
      "Bad scientificName\\: NA"
    )
  )
  expect_equal(
    check_sci_name_not_na(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      scientificName = NA_character_,
      check = "check_sci_name",
      error = "scientificName detected with missing value"
    )
  )
  expect_error(
    dct_check_sci_name(bad_dat),
    paste(
      "check_sci_name failed.*",
      "scientificName detected with missing value.*",
      "Bad scientificName\\: NA"
    )
  )
  expect_equal(
    dct_check_sci_name(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      scientificName = NA_character_,
      check = "check_sci_name",
      error = "scientificName detected with missing value"
    )
  )
})

test_that("check for 'sci name cannot be duplicated' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "3", "1", "accepted", "Species bar",
    "3", NA, "accepted", "Species bar"
  )
  expect_error(
    check_sci_name_is_uniq(bad_dat, on_fail = "error"),
    paste(
      "check_sci_name failed.*",
      "scientificName detected with duplicated value.*",
      "Bad scientificName\\: Species bar"
    )
  )
  expect_error(
    dct_check_sci_name(bad_dat),
    paste(
      "check_sci_name failed.*",
      "scientificName detected with duplicated value.*",
      "Bad scientificName\\: Species bar"
    )
  )
  expect_equal(
    dct_check_sci_name(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      scientificName = "Species bar",
      check = "check_sci_name",
      error = "scientificName detected with duplicated value"
    )
  )
})
