test_that("correctly formatted data does not error", {
  good_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar"
  )
  expect_equal(
    dct_check_tax_status(good_dat),
    good_dat
  )
})

test_that("Check for 'taxonomicStatus in valid values' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", NA, "foo", "Species bat"
  )
  expect_error(
    dct_check_tax_status(bad_dat),
    paste0(
      "check_tax_status failed.*",
      "taxonID detected whose taxonomicStatus is not in valid_tax_status.*",
      "Bad taxonID\\: 3.*",
      "Bad taxonomicStatus\\: foo"
    )
  )
  expect_equal(
    suppressWarnings(
      dct_check_tax_status(bad_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      scientificName = "Species bat",
      taxonomicStatus = "foo",
      error = paste(
        "taxonID detected whose taxonomicStatus is not in",
        "valid_tax_status (accepted, synonym, variant, NA)"
      ),
      check = "check_tax_status"
    )
  )
})
