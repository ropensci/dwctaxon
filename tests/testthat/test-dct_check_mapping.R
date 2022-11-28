library(patrick)

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

test_that("missing columns will pass", {
  expect_no_error(
    dct_check_mapping(
      data.frame(scientificName = "a", taxonID = 1),
      on_fail = "summary"
    )
  )
  expect_no_error
  dct_check_mapping(
    data.frame(b = "a"),
    on_fail = "summary"
  )
})

# Set up data for testing check_mapping_to_self()
bad_dat_dup_taxid <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~scientificName,
  "1", NA, "Species foo",
  "2", "1", "Species bar",
  "3", "3", "Species bat"
)
bad_cols <- c("acceptedNameUsageID", "parentNameUsageID", "originalNameUsageID")
bad_dat_dup_taxid <- rep(list(bad_dat_dup_taxid), 3)
bad_dat_dup_taxid[[2]] <- dplyr::rename(
  bad_dat_dup_taxid[[2]], !!bad_cols[2] := acceptedNameUsageID
)
bad_dat_dup_taxid[[3]] <- dplyr::rename(
  bad_dat_dup_taxid[[3]], !!bad_cols[3] := acceptedNameUsageID
)

# Set up data for testing check_mapping_exists()
bad_dat_missing_taxid <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~scientificName,
  "1", NA, "Species foo",
  "2", "1", "Species bar",
  "3", "4", "Species bat"
)
bad_dat_missing_taxid <- rep(list(bad_dat_missing_taxid), 3)
bad_dat_missing_taxid[[2]] <- dplyr::rename(
  bad_dat_missing_taxid[[2]], !!bad_cols[2] := acceptedNameUsageID
)
bad_dat_missing_taxid[[3]] <- dplyr::rename(
  bad_dat_missing_taxid[[3]], !!bad_cols[3] := acceptedNameUsageID
)

with_parameters_test_that("check_mapping_to_self works",
  {
    # subcheck: check_mapping_to_self
    expect_snapshot({
      (expect_error(
        check_mapping_to_self(bad_dat_dup_taxid, col_select = bad_cols)
      )
      )
    })
    expect_snapshot({
      suppressWarnings(
        check_mapping_to_self(
          bad_dat_dup_taxid,
          col_select = bad_cols, on_fail = "summary"
        )
      )
    })
    # main check: dct_check_mapping
    expect_snapshot({
      (expect_error(
        dct_check_mapping(bad_dat_dup_taxid, col_select = bad_cols)
      )
      )
    })
    expect_snapshot({
      suppressWarnings(
        dct_check_mapping(
          bad_dat_dup_taxid,
          col_select = bad_cols, on_fail = "summary"
        )
      )
    })
  },
  bad_dat_dup_taxid = bad_dat_dup_taxid,
  bad_cols = bad_cols
)

with_parameters_test_that("check_mapping_exists works",
  {
    # subcheck: check_mapping_exists
    expect_snapshot({
      (
        expect_error(
          check_mapping_exists(bad_dat_missing_taxid, col_select = bad_cols)
        )
      )
    })
    expect_snapshot({
      suppressWarnings(
        check_mapping_exists(
          bad_dat_missing_taxid,
          col_select = bad_cols, on_fail = "summary"
        )
      )
    })
    # main: dct_check_mapping
    expect_snapshot({
      (
        expect_error(
          dct_check_mapping(bad_dat_missing_taxid, col_select = bad_cols)
        )
      )
    })
    expect_snapshot({
      suppressWarnings(
        dct_check_mapping(
          bad_dat_missing_taxid,
          col_select = bad_cols, on_fail = "summary"
        )
      )
    })
  },
  bad_dat_missing_taxid = bad_dat_missing_taxid,
  bad_cols = bad_cols
)

test_that("check for 'no mapping to self' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~scientificName,
    "1", NA, "Species foo",
    "2", "1", "Species bar",
    "3", "3", "Species bat"
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
