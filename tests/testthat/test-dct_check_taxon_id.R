test_that("Checks on input data format work", {
  expect_error(
    dct_check_taxon_id(data.frame(a = 1), on_fail = c(1, 2)),
    "on_fail is not a string"
  )
  expect_error(
    dct_check_taxon_id(data.frame(a = 1), on_success = c(1, 2)),
    "on_success is not a string"
  )
  expect_error(
    dct_check_taxon_id(data.frame(a = 1), on_fail = "symmury"),
    "on_fail must be one of 'error' or 'summary'"
  )
  expect_error(
    dct_check_taxon_id(data.frame(a = 1), on_success = "summary"),
    "on_success must be one of 'data' or 'logical'"
  )
  expect_error(
    dct_check_taxon_id("a"),
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
  expect_no_error(dct_check_taxon_id(good_dat))
  expect_equal(dct_check_taxon_id(good_dat), good_dat)
  expect_equal(dct_check_taxon_id(good_dat, on_success = "logical"), TRUE)
})

test_that("check for correctly formatted columns works", {
  # taxonID column missing
  expect_error(
    dct_check_taxon_id(data.frame(scientificName = "foo bar")),
    "check_taxon_id requires column taxonID in input data"
  )
  expect_equal(
    dct_check_taxon_id(
      data.frame(scientificName = "foo bar"),
      on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      error = "check_taxon_id requires column taxonID in input data",
      check = "check_taxon_id"
    )
  )
  # taxonID of wrong class
  expect_error(
    dct_check_taxon_id(
      data.frame(taxonID = complex(1))
    ),
    "Column taxonID must be of class character, numeric, or integer"
  )
  expect_equal(
    dct_check_taxon_id(
      data.frame(taxonID = complex(1)),
      on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      error = "Column taxonID must be of class character, numeric, or integer",
      check = "check_taxon_id"
    )
  )
})

test_that("check for 'taxonID cannot be missing' works", {
  # taxonID with missing values
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    NA, "1", "accepted", "Species bar",
    "3", NA, "accepted", "Species bat"
  )
  expect_error(
    check_taxon_id_not_na(bad_dat, on_fail = "error"),
    paste(
      "check_taxon_id failed.*",
      "taxonID detected with missing value.*",
      "Bad taxonID\\: NA"
    )
  )
  expect_equal(
    check_taxon_id_not_na(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      taxonID = NA_character_,
      check = "check_taxon_id",
      error = "taxonID detected with missing value"
    )
  )
  expect_error(
    dct_check_taxon_id(bad_dat),
    paste(
      "check_taxon_id failed.*",
      "taxonID detected with missing value.*",
      "Bad taxonID\\: NA"
    )
  )
  expect_equal(
    dct_check_taxon_id(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      taxonID = NA_character_,
      error = "taxonID detected with missing value",
      check = "check_taxon_id"
    )
  )
})

test_that("check for 'taxonID cannot be duplicated' works", {
  # Duplicated taxonID
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "3", NA, "accepted", "Species bar",
    "3", NA, "accepted", "Species bat",
    "3", "1", "synonym", "Species blah"
  )
  expect_error(
    check_taxon_id_is_uniq(bad_dat, on_fail = "error"),
    paste(
      "check_taxon_id failed.*",
      "taxonID detected with duplicated value.*",
      "Bad taxonID\\: 3"
    )
  )
  expect_equal(
    check_taxon_id_is_uniq(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      taxonID = as.character(rep(3, 2)),
      check = rep("check_taxon_id", 2),
      error = rep("taxonID detected with duplicated value", 2)
    )
  )
  expect_error(
    dct_check_taxon_id(bad_dat),
    paste(
      "check_taxon_id failed.*",
      "taxonID detected with duplicated value.*",
      "Bad taxonID\\: 3"
    )
  )
  expect_equal(
    dct_check_taxon_id(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      taxonID = as.character(rep(3, 2)),
      error = rep("taxonID detected with duplicated value", 2),
      check = rep("check_taxon_id", 2)
    )
  )
  expect_warning(
    dct_check_taxon_id(bad_dat, on_fail = "summary", quiet = FALSE),
    "taxonID detected with duplicated value"
  )
})
