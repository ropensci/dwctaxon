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
    dct_validate(data.frame(), check_mapping_accepted = NULL),
    "check_mapping_accepted is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_mapping_parent = NULL),
    "check_mapping_parent is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_mapping_original = NULL),
    "check_mapping_original is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_mapping_accepted_status = NULL),
    "check_mapping_accepted_status is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_tax_status = NULL),
    "check_tax_status is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_status_diff = NULL),
    "check_status_diff is not a flag"
  )
  expect_error(
    dct_validate(data.frame(), check_col_names = NULL),
    "check_col_names is not a flag"
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
})

test_that("Missing columns cause error by default", {
  expect_error(
    dct_validate(dplyr::select(dct_filmies, -taxonID)),
    "check_taxon_id requires column taxonID in input data"
  )
  expect_error(
    dct_validate(
      dplyr::select(dct_filmies, -acceptedNameUsageID),
      check_mapping_accepted_status = TRUE
    ),
    "check_mapping_accepted_status requires column acceptedNameUsageID"
  )
  expect_error(
    dct_validate(
      dplyr::select(dct_filmies, -taxonomicStatus),
      check_mapping_accepted_status = TRUE
    ),
    "check_tax_status requires column taxonomicStatus in input data"
  )
  expect_error(
    dct_validate(
      dplyr::select(dct_filmies, -scientificName)
    ),
    "check_sci_name requires column scientificName in input data"
  )
})

# skip_missing_cols ----
test_that("skip_missing_cols works", {
  dct_options(skip_missing_cols = TRUE)
  expect_no_error(
    dct_validate(data.frame(taxonID = 1))
  )
  expect_no_error(
    dct_validate(dplyr::select(dct_filmies, -taxonID))
  )
  expect_no_error(
    dct_validate(
      dplyr::select(dct_filmies, -acceptedNameUsageID),
      check_mapping_accepted_status = TRUE
    )
  )
  expect_no_error(
    dct_validate(
      dplyr::select(dct_filmies, -taxonomicStatus),
      check_mapping_accepted_status = TRUE
    )
  )
  expect_no_error(
    dct_validate(dplyr::select(dct_filmies, -scientificName))
  )
  dct_options(reset = TRUE)
})

# check_taxon_id ----

test_that("check for 'taxonID cannot be missing' works", {
  # taxonID with missing values
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    NA, "1", "accepted", "Species bar",
    "3", NA, "accepted", "Species bat"
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
    dct_validate(bad_dat),
    paste(
      "check_taxon_id failed.*",
      "taxonID detected with duplicated value.*",
      "Bad taxonID\\: 3"
    )
  )
  expect_equal(
    dct_validate(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      taxonID = as.character(rep(3, 2)),
      error = rep("taxonID detected with duplicated value", 2),
      check = rep("check_taxon_id", 2)
    )
  )
})

# check_tax_status -----

test_that("Check for 'taxonomicStatus in valid values' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", NA, "foo", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat),
    paste0(
      "check_tax_status failed.*",
      "taxonID detected whose taxonomicStatus is not in valid_tax_status.*",
      "Bad taxonID\\: 3.*",
      "Bad taxonomicStatus\\: foo"
    )
  )
  expect_equal(
    dct_validate(bad_dat, on_fail = "summary", quiet = TRUE),
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

test_that("Setting valid taxonomic status via dct_options() works", {
  dct_options(valid_tax_status = "accepted")
  expect_error(
    dct_validate(
      data.frame(taxonID = 1, taxonomicStatus = "synonym"),
      check_mapping_accepted = FALSE,
      check_mapping_accepted_status = FALSE,
      check_status_diff = FALSE,
      check_col_names = FALSE,
      check_sci_name = FALSE,
      check_tax_status = TRUE
    ),
    paste0(
      "check_tax_status failed.*",
      "taxonID detected whose taxonomicStatus is not in valid_tax_status.*",
      "\\(accepted\\).*",
      "Bad taxonID\\: 1.*",
      "Bad taxonomicStatus\\: synonym"
    )
  )
  dct_options(reset = TRUE)
})

# check_mapping -----

test_that("check for 'no mapping to self' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "synonym", "Species bar",
    "3", "3", "synonym", "Species bat"
  )
  expect_error(
    dct_validate(bad_dat, check_mapping_accepted_status = FALSE),
    paste0(
      "check_mapping failed.*",
      "taxonID detected with identical acceptedNameUsageID.*",
      "Bad taxonID\\: 3.*",
      "Bad scientificName\\: Species bat.*",
      "Bad acceptedNameUsageID\\: 3"
    )
  )
  expect_equal(
    dct_validate(
      bad_dat,
      check_mapping_accepted_status = FALSE, on_fail = "summary", quiet = TRUE
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
    dct_validate(bad_dat, check_mapping_accepted_status = FALSE),
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
    dct_validate(
      bad_dat,
      check_mapping_accepted_status = FALSE, on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      taxonID = "3",
      acceptedNameUsageID = "4",
      scientificName = "Species bat",
      error = paste(
        "taxonID detected whose acceptedNameUsageID value does not map to",
        "taxonID of an existing name."
      ),
      check = "check_mapping",
    )
  )
})

# check_mapping_accepted_status ----

dct_options(check_tax_status = FALSE)
with_parameters_test_that("check_mapping_* works for dup taxid",
  {
    expect_snapshot({
      (expect_error(dct_validate(bad_dat)))
    })
    expect_snapshot({
      (expect_warning(dct_validate(bad_dat, on_fail = "summary")))
    })
    expect_snapshot({
      dct_validate(bad_dat, on_fail = "summary", quiet = TRUE)
    })
  },
  bad_dat = bad_dat_dup_taxid
)

with_parameters_test_that("check_mapping_* works for missing taxid",
  {
    expect_snapshot({
      (expect_error(dct_validate(bad_dat)))
    })
    expect_snapshot({
      (expect_warning(dct_validate(bad_dat, on_fail = "summary")))
    })
    expect_snapshot({
      dct_validate(bad_dat, on_fail = "summary", quiet = TRUE)
    })
  },
  bad_dat = bad_dat_missing_taxid
)
dct_options(reset = TRUE)

# check_mapping_accepted_status tests
dct_options(check_mapping_accepted_status = TRUE)

test_that("check for 'synonyms must map to accepted names' works", {
  bad_dat <- rbind(
    data.frame(
      taxonID = "1",
      acceptedNameUsageID = "2",
      taxonomicStatus = "synonym",
      scientificName = "foo"
    ),
    data.frame(
      taxonID = "2",
      acceptedNameUsageID = NA_character_,
      taxonomicStatus = NA_character_,
      scientificName = "bar"
    )
  )
  expect_error(
    dct_validate(bad_dat),
    paste0(
      "check_mapping_accepted_status failed.*",
      "synonym detected whose acceptedNameUsageID value.*",
      "does not map to taxonID of an accepted name.*",
      "Bad taxonID\\: 1.*",
      "Bad scientificName\\: foo.*",
      "Bad acceptedNameUsageID\\: 2"
    )
  )
  expect_equal(
    dct_validate(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      taxonID = "1",
      acceptedNameUsageID = "2",
      scientificName = "foo",
      error = paste(
        "synonym detected whose acceptedNameUsageID value",
        "does not map to taxonID of an accepted name"
      ),
      check = "check_mapping_accepted_status"
    )
  )
})

test_that(
  "check for 'acceptedNameUsageID must have non-NA taxonomicStatus' works",
  {
    bad_dat <- tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "Species bar",
      "2", "1", NA, "foo",
      "3", "1", "synonym", "Species bat"
    )
    expect_error(
      dct_validate(bad_dat),
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
      dct_validate(bad_dat, on_fail = "summary", quiet = TRUE),
      tibble::tibble(
        taxonID = "2",
        acceptedNameUsageID = "1",
        scientificName = "foo",
        error = paste(
          "rows detected whose acceptedNameUsageID value",
          "is not missing, but have missing taxonomicStatus"
        ),
        check = "check_mapping_accepted_status"
      )
    )
  }
)

test_that(
  "check for 'variant must map to non-variant' works",
  {
    bad_var_dat <- tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", "3", "variant", "Species bar",
      "2", NA, "accepted", "foo",
      "3", "2", "variant", "Species bat"
    )
    expect_error(
      dct_validate(bad_var_dat),
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
      dct_validate(bad_var_dat, on_fail = "summary", quiet = TRUE),
      tibble::tibble(
        taxonID = "1",
        acceptedNameUsageID = "3",
        scientificName = "Species bar",
        error = paste(
          "variant(s) detected whose acceptedNameUsageID value maps to",
          "taxonID of a variant"
        ),
        check = "check_mapping_accepted_status"
      )
    )
  }
)

test_that(
  "check for 'variant must map to something' works",
  {
    bad_var_dat <- tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "Species bar",
      "2", "1", "synonym", "boo",
      "3", NA, "variant", "Species bat"
    )
    expect_error(
      dct_validate(bad_var_dat),
      paste0(
        "check_mapping_accepted_status failed.*",
        "variant\\(s\\) detected who lack an acceptedNameUsageID.*",
        "Bad taxonID\\: 3.*",
        "Bad scientificName\\: Species bat"
      )
    )
    expect_equal(
      dct_validate(bad_var_dat, on_fail = "summary", quiet = TRUE),
      tibble::tibble(
        taxonID = "3",
        scientificName = "Species bat",
        error = "variant(s) detected who lack an acceptedNameUsageID",
        check = "check_mapping_accepted_status"
      )
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
      dct_validate(bad_acc_dat, check_mapping_accepted_status = TRUE),
      paste0(
        "check_mapping_accepted_status failed.*",
        "accepted name\\(s\\) detected with a non\\-missing value for ",
        "acceptedNameUsageID.*",
        "Bad taxonID\\: 1.*",
        "Bad scientificName\\: Species bar"
      )
    )
    expect_equal(
      dct_validate(
        bad_acc_dat,
        check_mapping_accepted_status = TRUE,
        on_fail = "summary", quiet = TRUE
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
  }
)

test_that("Values like 'ambiguous synonym' work", {
  good_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "3", "variant", "Species bar",
    "3", "1", "ambiguous synonym", "Species foobar",
    "4", NA, NA, "Species moobar"
  )
  expect_no_error(
    dct_validate(
      good_dat,
      valid_tax_status = "accepted, ambiguous synonym, variant, NA"
    )
  )
})

dct_options(reset = TRUE)

# check_sci_name ----

test_that("check for 'each sci name has single status' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "variant", "Species foo",
    "3", "7", "synonym", "Species bar",
    "4", "7", "synonym", "Species bar",
    "5", "1", "synonym", "Species foobar",
    "6", NA, NA, "Species foobar",
    "7", NA, "accepted", "Species blah"
  )
  expect_error(
    dct_validate(bad_dat, check_status_diff = TRUE, check_sci_name = FALSE),
    paste0(
      "scientificName detected with multiple different values for ",
      "taxonomicStatus.*",
      "Bad scientificName\\: Species foo, Species foobar"
    )
  )
  expect_equal(
    dct_validate(bad_dat,
      check_status_diff = TRUE, check_sci_name = FALSE,
      on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      taxonID = c("1", "2", "5", "6"),
      scientificName = c(
        rep("Species foo", 2),
        rep("Species foobar", 2)
      ),
      taxonomicStatus = c("accepted", "variant", "synonym", NA),
      error = rep(
        paste(
          "scientificName detected with multiple different values for",
          "taxonomicStatus"
        ), 4
      ),
      check = rep("check_status_diff", 4)
    )
  )
})

# check_status_diff ----

test_that("check for 'each sci name has single status' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "Species foo",
    "2", "1", "variant", "Species foo",
    "3", "7", "synonym", "Species bar",
    "4", "7", "synonym", "Species bar",
    "5", "7", "synonym", "Species foobar",
    "6", NA, NA, "Species foobar",
    "7", NA, "accepted", "Species blah"
  )
  expect_error(
    dct_validate(bad_dat, check_sci_name = FALSE, check_status_diff = TRUE),
    paste0(
      "scientificName detected with multiple different values for ",
      "taxonomicStatus.*",
      "Bad scientificName\\: Species foo, Species foobar"
    )
  )
  expect_equal(
    dct_validate(
      bad_dat,
      check_sci_name = FALSE, check_status_diff = TRUE,
      on_fail = "summary", quiet = TRUE
    ),
    tibble::tibble(
      taxonID = c("1", "2", "5", "6"),
      scientificName = c(
        rep("Species foo", 2),
        rep("Species foobar", 2)
      ),
      taxonomicStatus = c("accepted", "variant", "synonym", NA),
      error = rep(
        paste(
          "scientificName detected with multiple different values for",
          "taxonomicStatus"
        ), 4
      ),
      check = rep("check_status_diff", 4)
    )
  )
})

# check_col_names -----

test_that("check for 'all columns must have valid names' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName, ~a,
    "1", NA, "accepted", "Species foo", 1,
    "2", "1", "synonym", "Species bar", 2,
    "3", NA, "accepted", "Species bat", 3
  )
  expect_error(
    dct_validate(bad_dat),
    paste0(
      "check_col_names failed.*",
      "Invalid column name\\(s\\) detected.*",
      "Bad column names\\: a"
    )
  )
  expect_equal(
    dct_validate(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      error = "Invalid column names detected: a",
      check = "check_col_names"
    )
  )
})

# Other tests -----

dct_options(check_mapping_accepted_status = TRUE)

test_that("combinations of failures get reported", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", "2", "accepted", "foo",
    "2", NA, "synonym", "bar",
    "3", NA, "accept", "bat",
    "4", NA, "accept", "bat"
  )
  expect_equal(
    dct_validate(bad_dat, on_fail = "summary", quiet = TRUE),
    tibble::tibble(
      taxonID = c("1", "2", "3", "4", "3", "4"),
      acceptedNameUsageID = rep(NA_character_, 6),
      scientificName = c("foo", "bar", "bat", "bat", "bat", "bat"),
      taxonomicStatus = c(NA, NA, NA, NA, "accept", "accept"),
      error = c(
        "accepted name(s) detected with a non-missing value for acceptedNameUsageID", # nolint
        "synonym detected whose acceptedNameUsageID value does not map to taxonID of an accepted name", # nolint
        "scientificName detected with duplicated value",
        "scientificName detected with duplicated value",
        "taxonID detected whose taxonomicStatus is not in valid_tax_status (accepted, synonym, variant, NA)", # nolint
        "taxonID detected whose taxonomicStatus is not in valid_tax_status (accepted, synonym, variant, NA)" # nolint
      ),
      check = c(
        "check_mapping_accepted_status", "check_mapping_accepted_status",
        "check_sci_name", "check_sci_name",
        "check_tax_status", "check_tax_status"
      )
    )
  )
})

dct_options(reset = TRUE)
