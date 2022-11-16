# Clear default value for VALID_TAX_STATUS
Sys.unsetenv("VALID_TAX_STATUS")

test_that("check for 'valid_tax_status must include required values' works", {
  expect_error(
    check_mapping_strict_status(
      data.frame(a = 1),
      valid_tax_status = "foo"
    ),
    paste0(
      "valid_tax_status missing required value or values.*",
      "Missing values\\: synonym, accepted, variant.*",
      "Current valid_tax_status\\: 'foo'"
    )
  )
  expect_equal(
    suppressWarnings(
      check_mapping_strict_status(
          data.frame(a = 1),
          valid_tax_status = "foo",
          on_fail = "summary"
        )
    ),
    tibble::tibble(
      error = paste(
        "valid_tax_status missing required value or values:",
        "synonym, accepted, variant"
      ),
      check = "check_mapping_strict"
    )
  )
})

test_that("check for 'synonyms must map to accepted names' works", {
  bad_dat <- rbind(
    data.frame(
      taxonID = "1",
      acceptedNameUsageID = "3",
      taxonomicStatus = "synonym",
      scientificName = "foo"),
    data.frame(
      taxonID = "2",
      acceptedNameUsageID = NA_character_,
      taxonomicStatus = "accepted",
      scientificName = "bar"
      )
  )
  expect_error(
    check_syn_map_to_acc(bad_dat
    ),
    paste0(
      "check_mapping_strict failed.*",
      "synonym detected whose acceptedNameUsageID value.*",
      "does not map to taxonID of an accepted name.*",
      "Bad taxonID\\: 1.*",
      "Bad scientificName\\: foo.*",
      "Bad acceptedNameUsageID\\: 3"
    )
  )
  expect_equal(
    suppressWarnings(
      check_syn_map_to_acc(bad_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "1",
      scientificName = "foo",
      acceptedNameUsageID = "3",
      error = paste(
        "synonym detected whose acceptedNameUsageID value",
        "does not map to taxonID of an accepted name"
      ),
      check = "check_mapping_strict"
    )
  )
})

test_that(
  "check for 'acceptedNameUsageID must have non-NA taxonomicStatus' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", "1", "accepted", "Species bar",
    "2", "1", NA_character_, "foo",
    "3", "1", "synonym", "Species bat"
  )
  expect_error(
    check_acc_id_has_tax_status(bad_dat),
    paste0(
      "check_mapping_strict failed.*",
      "rows detected whose acceptedNameUsageID value.*",
      "is not missing, but have missing taxonomicStatus.*",
      "Bad taxonID\\: 2.*",
      "Bad scientificName\\: foo.*",
      "Bad acceptedNameUsageID\\: 1"
    )
  )
  expect_equal(
    suppressWarnings(
      check_acc_id_has_tax_status(bad_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "2",
      scientificName = "foo",
      acceptedNameUsageID = "1",
      error = paste(
        "rows detected whose acceptedNameUsageID value",
        "is not missing, but have missing taxonomicStatus"
      ),
      check = "check_mapping_strict"
    )
  )
})

test_that(
  "check for 'acceptedNameUsageID must have valid taxonomicStatus' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", "1", "accepted", "Species bar",
    "2", "1", "meh", "foo",
    "3", "1", "synonym", "Species bat"
  )
  expect_error(
    check_acc_id_valid_tax_status(bad_dat),
    paste0(
      "check_mapping_strict failed.*",
      "rows detected whose acceptedNameUsageID value is not missing.*",
      "but with taxonomicStatus that is not.*",
      "Bad taxonID\\: 2.*",
      "Bad scientificName\\: foo.*",
      "Bad taxonomicStatus\\: meh"
    )
  )
  expect_equal(
    suppressWarnings(
      check_acc_id_valid_tax_status(bad_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "2",
      acceptedNameUsageID = "1",
      scientificName = "foo",
      taxonomicStatus = "meh",
      error = paste(
        "rows detected whose acceptedNameUsageID value is not missing,",
        "but with taxonomicStatus that is not 'accepted', 'synonym', or",
        "'variant'"
      ),
      check = "check_mapping_strict"
    )
  )
})

test_that(
  "check for 'variant must map to non-variant' works", {
  bad_var_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", "3", "variant", "Species bar",
    "3", "2", "variant", "Species bat"
  )
  expect_error(
    check_variant_map_to_nonvar(bad_var_dat),
    paste0(
      "check_mapping_strict failed.*",
      "variant\\(s\\) detected whose acceptedNameUsageID value maps to.*",
      "taxonID of a variant.*",
      "Bad taxonID\\: 1.*",
      "Bad scientificName\\: Species bar.*",
      "Bad acceptedNameUsageID\\: 3"
    )
  )
  expect_equal(
    suppressWarnings(
      check_variant_map_to_nonvar(bad_var_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "1",
      scientificName = "Species bar",
      acceptedNameUsageID = "3",
      error = paste(
        "variant(s) detected whose acceptedNameUsageID value maps to",
        "taxonID of a variant"
      ),
      check = "check_mapping_strict"
    )
  )
})

test_that(
  "check for 'variant must map to something' works", {
  bad_var_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", "3", "variant", "Species bar",
    "3", NA, "variant", "Species bat"
  )
  expect_error(
    check_variant_map_to_something(bad_var_dat),
    paste0(
      "check_mapping_strict failed.*",
      "variant\\(s\\) detected who lack an acceptedNameUsageID.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat"
    )
  )
  expect_equal(
    suppressWarnings(
      check_variant_map_to_something(bad_var_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "3",
      scientificName = "Species bat",
      error = "variant(s) detected who lack an acceptedNameUsageID",
      check = "check_mapping_strict"
    )
  )
})

test_that(
  "check for 'accepted names cannot map to anything' works", {
  bad_acc_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", "3", "accepted", "Species bar",
    "3", NA, "accepted", "Species bat"
  )
  expect_error(
    check_accepted_map_to_nothing(bad_acc_dat),
    paste0(
      "check_mapping_strict failed.*",
      "accepted name\\(s\\) detected with a non\\-missing value for ",
      "acceptedNameUsageID.*",
      "Bad taxonID\\: 1.*",
      "Bad scientificName\\: Species bar"
    )
  )
  expect_equal(
    suppressWarnings(
      check_accepted_map_to_nothing(bad_acc_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = "1",
      scientificName = "Species bar",
      error = paste(
        "accepted name(s) detected with a non-missing value for",
        "acceptedNameUsageID"
      ),
      check = "check_mapping_strict"
    )
  )
})

# Clear default value for VALID_TAX_STATUS
Sys.unsetenv("VALID_TAX_STATUS")