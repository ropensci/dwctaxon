test_that("Checks on required columns work", {
  expect_equal(
    suppressWarnings(
      dct_check_mapping(
        data.frame(scientificName = "a", taxonID = 1),
        on_fail = "summary"
      )
    ),
    tibble::tibble(
      check = "check_mapping",
      error = "check_mapping requires column acceptedNameUsageID in input data"
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
      check = rep("check_mapping", 2),
      error = c(
        "check_mapping requires column taxonID in input data",
        "check_mapping requires column acceptedNameUsageID in input data"
      )
    )
  )
})

test_that("Correctly formatted data does not error", {
 good_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", "1", "synonym", "Species bat"
  )
  expect_equal(
    check_mapping(good_dat), good_dat
  )
  expect_equal(
    check_mapping(good_dat, on_success = "logical"), TRUE
  )
  expect_equal(
    dct_check_mapping(good_dat), good_dat
  )
  expect_equal(
    dct_check_mapping(good_dat, on_success = "logical"), TRUE
  )
})

# Set up some shared data for tests
bad_dat_1 <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  "1", NA, "accepted", "Species foo",
  "2", "1", "synonym", "Species bar",
  "3", "3", "synonym", "Species bat"
)
bad_dat_2 <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  "1", NA, "accepted", "Species foo",
  "2", "1", "synonym", "Species bar",
  "3", "4", "synonym", "Species bat"
)

test_that("Bad data results in error with on_fail = 'error'", {
  expect_error(
    check_mapping_to_self(bad_dat_1),
    paste0(
      "check_mapping failed.*",
      "taxonID detected with identical acceptedNameUsageID.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat.*",
      "Bad acceptedNameUsageID\\: 3"
    )
  )
  expect_error(
    check_mapping(bad_dat_2),
    paste0(
      "check_mapping failed.*",
      "taxonID detected whose acceptedNameUsageID value does not map to.*",
      "taxonID of an existing name.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat.*",
      "Bad acceptedNameUsageID\\: 4"
    )
  )
  expect_error(
    dct_check_mapping(bad_dat_1),
    paste0(
      "check_mapping failed.*",
      "taxonID detected with identical acceptedNameUsageID.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat.*",
      "Bad acceptedNameUsageID\\: 3"
    )
  )
  expect_error(
    dct_check_mapping(bad_dat_2),
    paste0(
      "check_mapping failed.*",
      "taxonID detected whose acceptedNameUsageID value does not map to.*",
      "taxonID of an existing name.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat.*",
      "Bad acceptedNameUsageID\\: 4"
    )
  )
})

test_that("Bad data results in summary with on_fail = 'summary'", {
  expect_equal(
    suppressWarnings(
      check_mapping_to_self(bad_dat_1, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      scientificName = "Species bat",
      acceptedNameUsageID = "3",
      error = "taxonID detected with identical acceptedNameUsageID",
      check = "check_mapping"
    )
  )
  expect_equal(
    suppressWarnings(
      dct_check_mapping(bad_dat_1, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      scientificName = "Species bat",
      acceptedNameUsageID = "3",
      error = "taxonID detected with identical acceptedNameUsageID",
      check = "check_mapping"
    )
  )
  expect_equal(
    suppressWarnings(
      check_mapping(bad_dat_2, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      scientificName = "Species bat",
      acceptedNameUsageID = "4",
      error = "taxonID detected whose acceptedNameUsageID value does not map to taxonID of an existing name.", # nolint
      check = "check_mapping",
    )
  )
  expect_equal(
    suppressWarnings(
      dct_check_mapping(bad_dat_2, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      scientificName = "Species bat",
      acceptedNameUsageID = "4",
      error = "taxonID detected whose acceptedNameUsageID value does not map to taxonID of an existing name.", # nolint
      check = "check_mapping",
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

rm(bad_dat_1)
rm(bad_dat_2)
