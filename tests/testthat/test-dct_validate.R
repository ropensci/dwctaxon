test_that("validation works", {
  # No errrors on correctly formatted data
  good_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", NA, "accepted", "Species bat"
  )
  expect_no_error(dct_validate(good_dat))
  expect_no_error(
    dct_validate(
      data.frame(taxonID = 1),
      check_mapping = FALSE,
      check_taxonomic_status = FALSE,
      check_acc_syn_diff = FALSE
    )
  )
  # input not data frame
  expect_error(
    dct_validate(1),
    "'tax_dat' must be of class 'data.frame'"
  )
  # taxonID column missing (check_taxon_id)
  expect_error(
    dct_validate(data.frame(scientificName = "foo bar")),
    "Column 'taxonID' required in input data"
  )
  expect_error(
    dct_validate(
      data.frame(taxonID = complex(1))
    ),
    "Column 'taxonID' must be of class 'character', 'numeric', or 'integer'"
  )
  # taxonID with missing values (check_taxon_id)
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    NA, "1", "accepted", "Species bar",
    "3", NA, "accepted", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat),
    "Column 'taxonID' violates assertion 'not_na' 1 time"
  )
  # Duplicated taxonID (check_taxon_id)
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "3", "1", "accepted", "Species bar",
    "3", NA, "accepted", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat),
    "Column 'taxonID' violates assertion 'is_uniq' 2 times"
  )
  # Bad mapping of synonyms
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", "4", "synonym", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat),
    "`check_mapping` failed"
  )
  # Bad taxonomicStatus
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synoWHAT", "Species bar",
    "3", "1", "synoWHO", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat),
    "`check_taxonomic_status` failed"
  )
  # Names appear in both accepted names and synonyms
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", NA, "accepted", "Species bar",
    "3", "2", "synonym", "Species foo"
  )
  expect_error(
    dct_validate(bad_dat),
    "`check_acc_syn_diff` failed"
  )
  # Bad columns: taxonomicstatus instead of taxonomicStatus
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicstatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", NA, "accepted", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat),
    "Column 'taxonomicStatus' required in input data"
  )
})
