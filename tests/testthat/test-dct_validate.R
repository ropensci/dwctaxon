test_that("validation of mapping works", {
  # taxonID missing (check_taxon_id)
  expect_error(
    dct_validate(data.frame(scientificName = "foo bar")),
    "Column 'taxonID' missing from 'tax_dat'"
  )
  # taxonID (check_taxon_id)
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
  # duplicated taxonID (check_taxon_id)
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
  # bad mapping of synonyms
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", "2", "synonym", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat),
    "`check_mapping` failed"
  )
  # bad taxonomicStatus
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
  # names appear in both accepted names and synonyms
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
  # bad columns: taxonomicstatus instead of taxonomicStatus
  bad_dat_4 <- tibble::tribble(
    ~taxonID, ~taxonomicstatus, ~scientificName,
    "1", "accepted", "Species foo",
    "2", "accepted", "Species bar",
    "3", "accepted", "Species shoofoo"
  )
  expect_error(
    dct_validate(bad_dat_4),
    "`check_col_names` failed"
  )
})
