test_that("Correctly formatted data does not error", {
 good_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", "1", "synonym", "Species bat"
  )
  expect_equal(
    check_mapping_exists(good_dat), good_dat
  )
  expect_equal(
    check_mapping_exists(good_dat, on_success = "logical"), TRUE
  )
  expect_equal(
    dct_check_mapping(good_dat), good_dat
  )
  expect_equal(
    dct_check_mapping(good_dat, on_success = "logical"), TRUE
  )
})

test_that("check for correctly formatted columns works", {
  expect_equal(
    suppressWarnings(
      dct_check_mapping(
        data.frame(scientificName = "a", taxonID = 1),
        on_fail = "summary"
      )
    ),
    tibble::tibble(
      error = "check_mapping requires column acceptedNameUsageID in input data",
      check = "check_mapping"
    )
  )
  expect_equal(
    suppressWarnings(
      dct_check_mapping(
        data.frame(b = "a"),
        on_fail = "summary"
      )
    ),
    tibble::tibble(
      error = c(
        "check_mapping requires column taxonID in input data",
        "check_mapping requires column acceptedNameUsageID in input data"
      ),
      check = rep("check_mapping", 2)
    )
  )
})

test_that("check for 'no mapping to self' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", "3", "synonym", "Species bat"
  )
  expect_error(
    check_mapping_to_self(bad_dat),
    paste0(
      "check_mapping failed.*",
      "taxonID detected with identical acceptedNameUsageID.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat.*",
      "Bad acceptedNameUsageID\\: 3"
    )
  )
  expect_equal(
    suppressWarnings(
      check_mapping_to_self(bad_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      scientificName = "Species bat",
      acceptedNameUsageID = "3",
      error = "taxonID detected with identical acceptedNameUsageID",
      check = "check_mapping"
    )
  )
  expect_error(
    dct_check_mapping(bad_dat),
    paste0(
      "check_mapping failed.*",
      "taxonID detected with identical acceptedNameUsageID.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat.*",
      "Bad acceptedNameUsageID\\: 3"
    )
  )
  expect_equal(
    suppressWarnings(
      dct_check_mapping(bad_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      acceptedNameUsageID = "3",
      scientificName = "Species bat",
      error = "taxonID detected with identical acceptedNameUsageID",
      check = "check_mapping"
    )
  )
})

test_that("check for 'target taxonID exists' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", "4", "synonym", "Species bat"
  )
  expect_error(
    check_mapping_exists(bad_dat),
    paste0(
      "check_mapping failed.*",
      "taxonID detected whose acceptedNameUsageID value does not map to.*",
      "taxonID of an existing name.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat.*",
      "Bad acceptedNameUsageID\\: 4"
    )
  )
  expect_equal(
    suppressWarnings(
      check_mapping_exists(bad_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      scientificName = "Species bat",
      acceptedNameUsageID = "4",
      error = paste(
        "taxonID detected whose acceptedNameUsageID value does not map to",
        "taxonID of an existing name."
      ),
      check = "check_mapping"
    )
  )
  expect_error(
    dct_check_mapping(bad_dat),
    paste0(
      "check_mapping failed.*",
      "taxonID detected whose acceptedNameUsageID value does not map to.*",
      "taxonID of an existing name.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat.*",
      "Bad acceptedNameUsageID\\: 4"
    )
  )
  expect_equal(
    suppressWarnings(
      dct_check_mapping(bad_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      acceptedNameUsageID = "4",
      scientificName = "Species bat",
      error = paste(
        "taxonID detected whose acceptedNameUsageID value does not map to",
        "taxonID of an existing name."
      ),
      check = "check_mapping"
    )
  )
})

test_that("bad taxonID also causes failure", {
  # Duplicated taxonID
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "3", "1", "accepted", "Species bar",
    "3", NA, "accepted", "Species bat"
  )
  expect_error(
    dct_check_mapping(bad_dat),
    paste(
      "check_taxon_id failed.*",
      "taxonID detected with duplicated value.*",
      "Bad taxonID\\: 3"
    )
  )
})