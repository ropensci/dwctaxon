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
    check_mapping_strict_status(
      data.frame(a = 1),
      valid_tax_status = "foo",
      on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      error = paste(
        "valid_tax_status missing required value or values:",
        "synonym, accepted, variant"
      ),
      check = "check_mapping_accepted_status"
    )
  )
  expect_warning(
    check_mapping_strict_status(
      data.frame(a = 1),
      valid_tax_status = "foo",
      on_fail = "summary"
    ),
    paste(
      "valid_tax_status missing required value or values:",
      "synonym, accepted, variant"
    )
  )
  expect_error(
    check_mapping_strict_status(
      data.frame(a = 1),
      valid_tax_status = "synonym, accepted",
    ),
    paste0(
      "valid_tax_status missing required value or values.*",
      "Missing values\\: variant.*",
      "Current valid_tax_status\\: 'synonym, accepted'"
    )
  )
  expect_error(
    check_mapping_strict_status(
      data.frame(a = 1),
      valid_tax_status = "variant, accepted",
    ),
    paste0(
      "valid_tax_status missing required value or values.*",
      "Missing values\\: synonym.*",
      "Current valid_tax_status\\: 'variant, accepted'"
    )
  )
  expect_error(
    check_mapping_strict_status(
      data.frame(a = 1),
      valid_tax_status = "variant, synonym",
    ),
    paste0(
      "valid_tax_status missing required value or values.*",
      "Missing values\\: accepted.*",
      "Current valid_tax_status\\: 'variant, synonym'"
    )
  )
  expect_error(
    check_mapping_strict_status(
      data.frame(a = 1),
      valid_tax_status = "variant, synonym",
    ),
    paste0(
      "valid_tax_status missing required value or values.*",
      "Missing values\\: accepted.*",
      "Current valid_tax_status\\: 'variant, synonym'"
    )
  )
  expect_error(
    check_mapping_strict_status(
      data.frame(a = 1),
      valid_tax_status = paste(
        sample(c("variant", "synonym", "accepted"), 2),
        collapse = ", "
      )
    ),
    "valid_tax_status missing required value or values.*"
  )
})

test_that("check for 'synonyms must map to accepted names' works", {
  bad_dat <- rbind(
    data.frame(
      taxonID = "1",
      acceptedNameUsageID = "3",
      taxonomicStatus = "synonym",
      scientificName = "foo"
    ),
    data.frame(
      taxonID = "2",
      acceptedNameUsageID = NA_character_,
      taxonomicStatus = "accepted",
      scientificName = "bar"
    )
  )
  bad_dat_warning <- paste(
    "synonym detected whose acceptedNameUsageID value",
    "does not map to taxonID of an accepted name"
  )
  expect_error(
    check_syn_map_to_acc(bad_dat),
    paste0(
      "check_mapping_accepted_status failed.*",
      "synonym detected whose acceptedNameUsageID value.*",
      "does not map to taxonID of an accepted name.*",
      "Bad taxonID\\: 1.*",
      "Bad scientificName\\: foo.*",
      "Bad acceptedNameUsageID\\: 3"
    )
  )
  expect_equal(
    check_syn_map_to_acc(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      taxonID = "1",
      scientificName = "foo",
      acceptedNameUsageID = "3",
      error = bad_dat_warning,
      check = "check_mapping_accepted_status"
    )
  )
  expect_warning(
    check_syn_map_to_acc(bad_dat, on_fail = "summary"),
    bad_dat_warning
  )
})

test_that(
  "check for 'acceptedNameUsageID must have non-NA taxonomicStatus' works",
  {
    bad_dat <- tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", "1", "accepted", "Species bar",
      "2", "1", NA_character_, "foo",
      "3", "1", "synonym", "Species bat"
    )
    bad_dat_warning <- paste(
      "rows detected whose acceptedNameUsageID value",
      "is not missing, but have missing taxonomicStatus"
    )
    expect_error(
      check_acc_id_has_tax_status(bad_dat),
      paste0(
        "check_mapping_accepted_status failed.*",
        "rows detected whose acceptedNameUsageID value.*",
        "is not missing, but have missing taxonomicStatus.*",
        "Bad taxonID\\: 2.*",
        "Bad scientificName\\: foo.*",
        "Bad acceptedNameUsageID\\: 1"
      )
    )
    expect_equal(
      check_acc_id_has_tax_status(bad_dat, on_fail = "summary", quiet = TRUE),
      tibble::tibble(
        taxonID = "2",
        scientificName = "foo",
        acceptedNameUsageID = "1",
        error = bad_dat_warning,
        check = "check_mapping_accepted_status"
      )
    )
    expect_warning(
      check_acc_id_has_tax_status(bad_dat, on_fail = "summary"),
      bad_dat_warning
    )
  }
)

test_that(
  "check for 'acceptedNameUsageID must have valid taxonomicStatus' works",
  {
    bad_dat <- tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", "1", "accepted", "Species bar",
      "2", "1", "meh", "foo",
      "3", "1", "synonym", "Species bat"
    )
    bad_dat_error <- paste(
      "rows detected whose acceptedNameUsageID value is not missing,",
      "but with taxonomicStatus that is not 'accepted', 'synonym', or",
      "'variant'"
    )
    expect_error(
      check_acc_id_valid_tax_status(bad_dat),
      paste0(
        "check_mapping_accepted_status failed.*",
        "rows detected whose acceptedNameUsageID value is not missing.*",
        "but with taxonomicStatus that is not.*",
        "Bad taxonID\\: 2.*",
        "Bad scientificName\\: foo.*",
        "Bad taxonomicStatus\\: meh"
      )
    )
    expect_equal(
      check_acc_id_valid_tax_status(bad_dat, on_fail = "summary", quiet = TRUE),
      tibble::tibble(
        taxonID = "2",
        acceptedNameUsageID = "1",
        scientificName = "foo",
        taxonomicStatus = "meh",
        error = bad_dat_error,
        check = "check_mapping_accepted_status"
      )
    )
    expect_warning(
      check_acc_id_valid_tax_status(bad_dat, on_fail = "summary"),
      bad_dat_error
    )
  }
)

test_that(
  "check for 'variant must map to non-variant' works",
  {
    bad_var_dat <- tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", "3", "variant", "Species bar",
      "3", "2", "variant", "Species bat"
    )
    expect_error(
      check_variant_map_to_nonvar(bad_var_dat),
      paste0(
        "check_mapping_accepted_status failed.*",
        "variant\\(s\\) detected whose acceptedNameUsageID value maps to.*",
        "taxonID of a variant.*",
        "Bad taxonID\\: 1.*",
        "Bad scientificName\\: Species bar.*",
        "Bad acceptedNameUsageID\\: 3"
      )
    )
    expect_equal(
      check_variant_map_to_nonvar(bad_var_dat,
        on_fail = "summary",
        quiet = TRUE
      ),
      tibble::tibble(
        taxonID = "1",
        scientificName = "Species bar",
        acceptedNameUsageID = "3",
        error = paste(
          "variant(s) detected whose acceptedNameUsageID value maps to",
          "taxonID of a variant"
        ),
        check = "check_mapping_accepted_status"
      )
    )
    expect_warning(
      check_variant_map_to_nonvar(bad_var_dat, on_fail = "summary"),
      paste(
        "variant\\(s\\) detected whose acceptedNameUsageID value maps to",
        "taxonID of a variant"
      )
    )
  }
)

test_that(
  "check for 'variant must map to something' works",
  {
    bad_var_dat <- tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", "3", "variant", "Species bar",
      "3", NA, "variant", "Species bat"
    )
    expect_error(
      check_variant_map_to_something(bad_var_dat),
      paste0(
        "check_mapping_accepted_status failed.*",
        "variant\\(s\\) detected who lack an acceptedNameUsageID.*",
        "Bad taxonID\\: 3.*",
        "Bad scientificName\\: Species bat"
      )
    )
    expect_equal(
      check_variant_map_to_something(bad_var_dat,
        on_fail = "summary",
        quiet = TRUE
      ),
      tibble::tibble(
        taxonID = "3",
        scientificName = "Species bat",
        error = "variant(s) detected who lack an acceptedNameUsageID",
        check = "check_mapping_accepted_status"
      )
    )
    expect_warning(
      check_variant_map_to_something(bad_var_dat, on_fail = "summary"),
      "variant\\(s\\) detected who lack an acceptedNameUsageID"
    )
  }
)

test_that(
  "check for 'accepted names cannot map to anything' works",
  {
    bad_acc_dat <- tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", "3", "accepted", "Species bar",
      "3", NA, "accepted", "Species bat"
    )
    expect_error(
      check_accepted_map_to_nothing(bad_acc_dat),
      paste0(
        "check_mapping_accepted_status failed.*",
        "accepted name\\(s\\) detected with a non\\-missing value for ",
        "acceptedNameUsageID.*",
        "Bad taxonID\\: 1.*",
        "Bad scientificName\\: Species bar"
      )
    )
    expect_equal(
      check_accepted_map_to_nothing(bad_acc_dat,
        on_fail = "summary",
        quiet = TRUE
      ),
      tibble::tibble(
        taxonID = "1",
        scientificName = "Species bar",
        error = paste(
          "accepted name(s) detected with a non-missing value for",
          "acceptedNameUsageID"
        ),
        check = "check_mapping_accepted_status"
      )
    )
    expect_warning(
      check_accepted_map_to_nothing(bad_acc_dat, on_fail = "summary"),
      paste(
        "accepted name\\(s\\) detected with a non-missing value for",
        "acceptedNameUsageID"
      )
    )
  }
)
