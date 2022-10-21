# Clear default value for VALID_TAX_STATUS
Sys.unsetenv("VALID_TAX_STATUS")

test_that("checks on input work", {
  expect_error(
    dct_validate(1),
    "'tax_dat' must be of class 'data.frame'"
  )
  expect_error(
    dct_validate(data.frame(), check_taxon_id = NULL),
    "check_taxon_id is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_mapping = NULL),
    "check_mapping is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_taxonomic_status = NULL),
    "check_taxonomic_status is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_acc_syn_diff = NULL),
    "check_acc_syn_diff is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_col_names = NULL),
    "check_col_names is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), strict_mapping = NULL),
    "strict_mapping is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), valid_tax_status = c(1, 2)),
    "valid_tax_status is not a string \\(a length one character vector\\)"
  )
})

test_that("correctly formatted data does not error", {
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
      strict_mapping = FALSE,
      check_taxonomic_status = FALSE,
      check_acc_syn_diff = FALSE
    )
  )
})

test_that("Setting valid taxonomic status via env var works", {
  Sys.setenv(VALID_TAX_STATUS = "accepted")
  expect_error(
    dct_validate(
      data.frame(taxonID = 1, taxonomicStatus = "synonym"),
      check_mapping = FALSE,
      strict_mapping = FALSE,
      check_acc_syn_diff = FALSE,
      check_col_names = FALSE
      ),
      "`check_taxonomic_status` failed"
  )
  # reset
  Sys.unsetenv("VALID_TAX_STATUS")
})

test_that("check_taxon_id works", {
  # taxonID column missing
  expect_error(
    dct_validate(data.frame(scientificName = "foo bar")),
    "`check_taxon_id` requires column 'taxonID' in input data"
  )
  expect_error(
    dct_validate(
      data.frame(taxonID = complex(1))
    ),
    "Column 'taxonID' must be of class 'character', 'numeric', or 'integer'"
  )
  # taxonID with missing values
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
  # Duplicated taxonID
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
})

test_that("check_mapping works", {
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
})

test_that("check_taxonomic_status works", {
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
  # Bad columns: taxonomicstatus instead of taxonomicStatus
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicstatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", NA, "accepted", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat),
    "`check_taxonomic_status` requires column 'taxonomicStatus' in input data"
  )
})

test_that("strict_mapping works", {
  # bad data: synonym doesn't map to accepted name
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "3", "synonym", "Species bar",
    "3", "2", "variant", "Species foobar"
  )
  expect_error(
    dct_validate(bad_dat),
    "`strict_mapping` failed\\.\nsynonym\\(s\\) detected whose `acceptedNameUsageID` value does not map to `taxonID` of an accepted name" # nolint
  )
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", NA, "synonym", "Species bar",
    "3", "2", "variant", "Species foobar"
  )
  expect_error(
    dct_validate(bad_dat),
    "`strict_mapping` failed\\.\nsynonym\\(s\\) detected whose `acceptedNameUsageID` value does not map to `taxonID` of an accepted name" # nolint
  )
  # bad data: variant maps to variant
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "3", "variant", "Species bar",
    "3", "1", "variant", "Species foobar"
  )
  expect_error(
    dct_validate(bad_dat),
    "`strict_mapping` failed\\.\nvariants\\(s\\) detected whose `acceptedNameUsageID` value maps to `taxonID` of a variant" # nolint
  )
  # bad data: variant has no acceptedNameUsageID
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", NA, "variant", "Species bar",
    "3", "1", "variant", "Species foobar"
  )
  expect_error(
    dct_validate(bad_dat),
    "`strict_mapping` failed\\.\nvariants\\(s\\) detected who lack an `acceptedNameUsageID`" # nolint
  )
  # bad data: accepted name has an acceptedNameUsageID
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", "2", "accepted", "Species foo",
    "2", NA, "accepted", "Species bar"
  )
  expect_error(
    dct_validate(bad_dat),
    "`strict_mapping` failed\\.\nAccepted names\\(s\\) detected with a non-missing value for `acceptedNameUsageID`.*Bad `taxonID`\\: 1\nBad `scientificName`\\: Species foo" # nolint
  )
  # bad data: any row with acceptedNameUsageID must have a non-missing
  # taxonomicStatus
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "3", "variant", "Species bar",
    "3", "1", "synonym", "Species foobar",
    "4", "1", NA, "Species foobar"
  )
  expect_error(
    dct_validate(bad_dat),
    "`strict_mapping` failed.*`acceptedNameUsageID` value is not missing, but have missing `taxonomicStatus`.*Bad `taxonID`\\: 4\nBad `scientificName`\\: Species foobar" # nolint
  )
  # bad data: any row with acceptedNameUsageID must have taxonomicStatus
  # of 'accepted', 'synonym', or 'variant'
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "3", "variant", "Species bar",
    "3", "1", "synonym", "Species foobar",
    "4", "1", "fooblah", "Species foobar"
  )
  expect_error(
    dct_validate(
      bad_dat,
      valid_tax_status = "accepted synonym variant fooblah"),
      paste0(
        "`strict_mapping` failed.*",
        "`taxonomicStatus`.* is not 'accepted', 'synonym', or 'variant'.*",
        "Bad `taxonID`\\: 4.*",
        "Bad `scientificName`\\: Species foobar.*",
        "Bad `taxonomicStatus`\\: fooblah.*"
      )
  )
  # NA values that don't intefere with mapping are OK
  good_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "3", "variant", "Species bar",
    "3", "1", "synonym", "Species foobar",
    "4", NA, NA, "Species foobar"
  )
  expect_no_error(
    dct_validate(good_dat)
  )
})

test_that("check_acc_syn_diff works", {
  # Names appear in both accepted names and synonyms
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", NA, "accepted", "Species bar",
    "3", "2", "synonym", "Species foo"
  )
  expect_error(
    dct_validate(bad_dat),
    "`check_acc_syn_diff` failed\\.\n`taxonID`\\(s\\) detected whose scientific names appear in both accepted names and synonyms" # nolint
  )
})