test_that("validation of mapping works", {
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
  bad_dat_2 <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synoWHAT", "Species bar",
    "3", "1", "synoWHO", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat_2),
    "`check_taxonomic_status` failed"
  )
  # names appear in both accepted names and synonyms
  bad_dat_3 <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", NA, "accepted", "Species bar",
    "3", "2", "synonym", "Species foo"
  )
  expect_error(
    dct_validate(bad_dat_3),
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
