# Tests for major arguments ----

library(mockery)
library(tibble)

# - taxon_id
# - sci_name
test_that("scientificName works to identify or modify row", {
  tax_dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo"
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      scientificName = "foo", taxonomicStatus = "maybe accepted",
      stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "maybe accepted", "foo"
    )
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "1", scientificName = "foobar",
      stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "foobar"
    )
  )
})

# - ...
test_that("non-DwC terms fail", {
  expect_error(
    dct_modify_row(
      data.frame(taxonID = "a"),
      taxon_id = "a", scientificName = "foobar"
    ),
    "All terms to modify must be valid DwC taxon terms"
  )
})

# - taxonomicStatus
# - acceptedNameUsageID
# - remap_names
test_that(
  "Name remapping works",
  {
    tax_dat <- tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "foo",
      "2", NA, "accepted", "bar",
      "3", "2", "synonym", "bat"
    )
    expect_equal(
      dct_modify_row(
        tax_dat,
        taxonID = "2", taxonomicStatus = "synonym", acceptedNameUsageID = "1",
        stamp_modified = FALSE
      ),
      tribble(
        ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
        "1", NA, "accepted", "foo",
        "2", "1", "synonym", "bar",
        "3", "1", "synonym", "bat"
      )
    )
    expect_equal(
      dct_modify_row(
        tax_dat,
        taxonID = "2", taxonomicStatus = "synonym", acceptedNameUsageID = "1",
        stamp_modified = FALSE, remap_names = FALSE
      ),
      tribble(
        ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
        "1", NA, "accepted", "foo",
        "2", "1", "synonym", "bar",
        "3", "2", "synonym", "bat"
      )
    )
  }
)

# - clear_usage_id
test_that("clear_usage_id argument works", {
  tax_dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo",
    "2", "1", "synonym", "bar"
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "2", taxonomicStatus = "accepted",
      stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "foo",
      "2", NA_character_, "accepted", "bar"
    )
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "2", taxonomicStatus = "accepted",
      clear_usage_id = FALSE, stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "foo",
      "2", "1", "accepted", "bar"
    )
  )
})

# - fill_usage_name
test_that("fill_usage_name argument works", {
  tax_dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    ~acceptedNameUsage,
    "1", NA, "accepted", "foo", NA_character_,
    "2", NA, "accepted", "bar", NA
  )
  # fills if acceptedNameUsage col in tax_dat
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "2", taxonomicStatus = "synonym", acceptedNameUsageID = "1",
      fill_usage_name = TRUE, stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      ~acceptedNameUsage,
      "1", NA, "accepted", "foo", NA,
      "2", "1", "synonym", "bar", "foo"
    )
  )
  # doesn't fill if not
  expect_equal(
    dct_modify_row(
      tax_dat[, colnames(tax_dat) != "acceptedNameUsage"],
      taxonID = "2", taxonomicStatus = "synonym", acceptedNameUsageID = "1",
      fill_usage_name = TRUE, stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "foo",
      "2", "1", "synonym", "bar"
    )
  )
})

# - remap_names
test_that("fill_usage_name works with remap_names", {
  tax_dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    ~acceptedNameUsage,
    "1", NA, "accepted", "foo", NA_character_,
    "2", NA, "accepted", "bar", NA,
    "3", "2", "synonym", "bat", NA
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "2", taxonomicStatus = "synonym", acceptedNameUsageID = "1",
      fill_usage_name = TRUE, stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      ~acceptedNameUsage,
      "1", NA, "accepted", "foo", NA,
      "2", "1", "synonym", "bar", "foo",
      "3", "1", "synonym", "bat", "foo"
    )
  )
})

# - remap_variant
test_that("remap_variant argument works", {
  tax_dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA_character_, "accepted", "foo",
    "2", NA_character_, "accepted", "bar",
    "3", "2", "variety", "bat"
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "2",
      taxonomicStatus = "synonym",
      acceptedNameUsage = "foo",
      stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA_character_, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "2", "variety", "bat"
    )
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "2",
      taxonomicStatus = "synonym",
      acceptedNameUsage = "foo",
      remap_variant = TRUE,
      stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA_character_, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "1", "variety", "bat"
    )
  )
  tax_dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA_character_, "accepted", "foo",
    "2", NA_character_, "accepted", "bar",
    "3", "2", "putative variety", "bat"
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "2",
      taxonomicStatus = "synonym",
      acceptedNameUsage = "foo",
      stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA_character_, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "2", "putative variety", "bat"
    )
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "2",
      taxonomicStatus = "synonym",
      acceptedNameUsage = "foo",
      remap_variant = TRUE,
      stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA_character_, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "1", "putative variety", "bat"
    )
  )
})

# - stamp_modified
test_that("stamp_modified argument works", {
  tax_dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA_character_, "accepted", "foo"
  )
  # Replace time stamp with date stamp for testing
  stub(dct_modify_row, "Sys.time", Sys.Date(), depth = 2)
  expect_equal(
    dct_modify_row(
      tax_dat, "1",
      taxonomicStatus = "bar", stamp_modified = TRUE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      ~modified,
      "1", NA_character_, "bar", "foo", as.character(Sys.Date())
    )
  )
})

# TODO - strict
# TODO - quiet

# - args_tbl
test_that("args_tbl can be used to update data", {
  dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    ~nameAccordingTo,
    1, NA, "accepted", "foo", "her",
    2, NA, "accepted", "bar", "her",
    3, NA, "accepted", "bat", "her"
  )
  expect_equal(
    dct_modify_row(
      dat,
      args_tbl = data.frame(
        taxonID = c(1, 2),
        taxonomicStatus = "synonym",
        acceptedNameUsageID = 3,
        stamp_modified = FALSE
      )
    ),
    tibble(
      taxonID = c(1, 2, 3),
      acceptedNameUsageID = c(3, 3, NA),
      taxonomicStatus = c("synonym", "synonym", "accepted"),
      scientificName = c("foo", "bar", "bat"),
      nameAccordingTo = rep("her", 3)
    )
  )
  expect_equal(
    dct_modify_row(
      dat,
      args_tbl = data.frame(
        scientificName = c("foo", "bar"),
        taxonomicStatus = "synonym",
        acceptedNameUsageID = 3,
        stamp_modified = FALSE
      )
    ),
    tibble(
      taxonID = c(1, 2, 3),
      acceptedNameUsageID = c(3, 3, NA),
      taxonomicStatus = c("synonym", "synonym", "accepted"),
      scientificName = c("foo", "bar", "bat"),
      nameAccordingTo = rep("her", 3)
    )
  )
  expect_equal(
    dct_modify_row(
      dat,
      args_tbl = tibble(
        taxonID = c(1, 2),
        taxonomicStatus = "synonym",
        acceptedNameUsageID = 3,
        stamp_modified = FALSE,
        nameAccordingTo = "me"
      )
    ),
    tibble(
      taxonID = c(1, 2, 3),
      acceptedNameUsageID = c(3, 3, NA),
      taxonomicStatus = c("synonym", "synonym", "accepted"),
      scientificName = c("foo", "bar", "bat"),
      nameAccordingTo = c("me", "me", "her")
    )
  )
})

# Helper functions ----
# These are mostly already covered by tests of dct_modify_row(),
# so tests in this section are not exhaustive

test_that("isolate_row() works", {
  expect_equal(
    isolate_row(tax_dat = data.frame(taxonID = c(1, 2)), taxon_id = 1),
    data.frame(taxonID = 1)
  )
  expect_equal(
    isolate_row(
      tax_dat = data.frame(scientificName = c("a", "b")), sci_name = "a"
    ),
    data.frame(scientificName = "a")
  )
  expect_error(
    isolate_row(
      tax_dat = data.frame(scientificName = c("a", "a")), sci_name = "a"
    ),
    "Not exactly one scientificName in data matches input scientificName 'a'"
  )
})

test_that("lookup_usage_id() works", {
  expect_equal(
    lookup_usage_id(
      tax_dat = data.frame(taxonID = c(1, 2), scientificName = c("a", "b")),
      usage_name = "b"
    ),
    2
  )
  expect_error(
    lookup_usage_id(
      tax_dat = data.frame(taxonID = c(1, 2), scientificName = c("b", "b")),
      usage_name = "b"
    ),
    "Not exactly one scientificName in data matches input acceptedNameUsage 'b'"
  )
  expect_error(
    lookup_usage_id(
      tax_dat = data.frame(taxonID = c(1, 2), scientificName = c("a", "b")),
      usage_name = "b",
      usage_id = 1
    ),
    "Input acceptedNameUsageID and acceptedNameUsage do not agree"
  )
})

test_that("create_new_row_by_modification() works", {
  expect_equal(
    create_new_row_by_modification(
      tax_dat = data.frame(taxonID = "a"),
      tax_dat_row = data.frame(taxonID = "a"),
      sci_name = "foo",
      taxon_id = "a"
    ),
    data.frame(taxonID = "a", scientificName = "foo")
  )
  expect_equal(
    create_new_row_by_modification(
      tax_dat = data.frame(taxonID = "a", scientificName = "bar"),
      tax_dat_row = data.frame(taxonID = "a", scientificName = "bar"),
      sci_name = "foo",
      taxon_id = "a"
    ),
    data.frame(taxonID = "a", scientificName = "foo")
  )
  expect_equal(
    create_new_row_by_modification(
      tax_dat = data.frame(taxonID = "a", scientificName = "bar"),
      tax_dat_row = data.frame(taxonID = "a", scientificName = "bar"),
      sci_name = "foo",
      taxon_id = "a"
    ),
    data.frame(taxonID = "a", scientificName = "foo")
  )
  expect_equal(
    create_new_row_by_modification(
      tax_dat = data.frame(taxonID = "a"),
      tax_dat_row = data.frame(taxonID = "a"),
      sci_name = "foo",
      tax_status = "bar",
      taxon_id = "a"
    ),
    data.frame(taxonID = "a", taxonomicStatus = "bar", scientificName = "foo")
  )
  # Clearing acceptedNameUsageID works
  expect_equal(
    create_new_row_by_modification(
      tax_dat = data.frame(
        taxonID = "a", taxonomicStatus = "synonym", acceptedNameUsageID = "b"
      ),
      tax_dat_row = data.frame(
        taxonID = "a", taxonomicStatus = "synonym", acceptedNameUsageID = "b"
      ),
      taxon_id = "a",
      tax_status = "accepted",
      clear_usage_id = TRUE
    ),
    data.frame(
      taxonID = "a",
      taxonomicStatus = "accepted", acceptedNameUsageID = NA_character_
    )
  )
})

test_that("change_other_rows() finds other rows affected by change", {
  expect_equal(
    change_other_rows(
      tax_dat = data.frame(
        taxonID = c(1, 2, 3),
        scientificName = c("a", "b", "c"),
        acceptedNameUsageID = c(NA, NA, 1),
        taxonomicStatus = c("accepted", "accepted", "synonym")
      ),
      tax_dat_row = data.frame(taxonID = 1),
      remap_variant = TRUE,
      remap_names = TRUE,
      usage_id = 2
    ),
    data.frame(
      taxonID = 3,
      scientificName = "c",
      acceptedNameUsageID = 2,
      taxonomicStatus = "synonym"
    )
  )
})

test_that("format_modified_row_output() works", {
  tax_dat_test <- data.frame(
    taxonID = c(1, 2, 3),
    scientificName = c("a", "b", "c"),
    acceptedNameUsageID = c(NA, NA, 1),
    taxonomicStatus = c("accepted", "accepted", "synonym")
  )
  expect_equal(
    format_modified_row_output(
      tax_dat = tax_dat_test,
      tax_dat_row = tax_dat_test[1, ],
      new_row = data.frame(
        taxonID = 1, scientificName = "a",
        acceptedNameUsageID = 2, taxonomicStatus = "synonym"
      ),
      new_row_other = data.frame(
        taxonID = 3, scientificName = "c",
        acceptedNameUsageID = 2, taxonomicStatus = "synonym"
      ),
      quiet = FALSE,
      strict = TRUE
    ),
    data.frame(
      taxonID = c(1, 2, 3),
      scientificName = c("a", "b", "c"),
      acceptedNameUsageID = c(2, NA, 2),
      taxonomicStatus = c("synonym", "accepted", "synonym")
    )
  )
})

test_that("na_to_null() works", {
  expect_equal(
    na_to_null(NA_character_),
    NULL
  )
  expect_equal(
    na_to_null("a"),
    "a"
  )
  expect_equal(
    na_to_null(NULL),
    NULL
  )
})

# Other tests ----

test_that("Catching missing taxonID works", {
  expect_error(
    dct_modify_row(
      data.frame(scientificName = "a")
    ),
    paste(
      "tax_dat must include column taxonID, the values of which must be",
      "unique and non-missing"
    )
  )
  expect_error(
    dct_modify_row(
      data.frame(taxonID = c("a", "a"))
    ),
    paste(
      "tax_dat must include column taxonID, the values of which must be",
      "unique and non-missing"
    )
  )
  expect_error(
    dct_modify_row(
      data.frame(taxonID = c("a", NA))
    ),
    paste(
      "tax_dat must include column taxonID, the values of which must be",
      "unique and non-missing"
    )
  )
})

test_that("Catching incorrectly specified usage names or IDs works", {
  expect_error(
    dct_modify_row(
      tibble(taxonID = "a", scientificName = "b"),
      acceptedNameUsage = "c"
    ),
    "Input acceptedNameUsage not detected in tax_dat\\$scientificName"
  )
  expect_error(
    dct_modify_row(
      tibble(taxonID = "a", scientificName = "b"),
      acceptedNameUsageID = "c"
    ),
    "Input acceptedNameUsageID not detected in tax_dat\\$taxonID"
  )
  expect_error(
    dct_modify_row(
      tibble(taxonID = "a"),
      acceptedNameUsage = "c"
    ),
    paste(
      "tax_dat must include column 'scientificName' to look up rows by",
      "acceptedNameUsage"
    )
  )
})

test_that("can modify row without changing status", {
  tax_dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo"
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "1", scientificName = "bar", stamp_modified = FALSE
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "bar"
    )
  )
})

test_that("other terms can be added", {
  tax_dat <- tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo",
    "2", NA, "accepted", "bar",
    "3", "2", "variety", "bat"
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxonID = "2",
      taxonomicStatus = "synonym",
      acceptedNameUsage = "foo",
      stamp_modified = FALSE,
      nameAccordingTo = "Me",
      nameAccordingToID = 1
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      ~nameAccordingTo, ~nameAccordingToID,
      "1", NA, "accepted", "foo", NA, NA,
      "2", "1", "synonym", "bar", "Me", 1,
      "3", "2", "variety", "bat", NA, NA
    )
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      args_tbl = data.frame(
        taxonID = c("2", "3"),
        taxonomicStatus = c("synonym", "accepted"),
        acceptedNameUsage = c("foo", NA),
        clear_usage_id = c(FALSE, TRUE),
        stamp_modified = rep(FALSE, 2),
        nameAccordingTo = rep("Me", 2),
        nameAccordingToID = rep(1, 2)
      )
    ),
    tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      ~nameAccordingTo, ~nameAccordingToID,
      "1", NA, "accepted", "foo", NA, NA,
      "2", "1", "synonym", "bar", "Me", 1,
      "3", NA, "accepted", "bat", "Me", 1
    )
  )
})

test_that("attempt to update without changes returns original data", {
  expect_equal(
    dct_modify_row(
      dct_filmies,
      scientificName = "Cephalomanes atrovirens Presl",
      taxonomicStatus = "accepted",
      stamp_modified = FALSE, quiet = TRUE
    ),
    dct_filmies
  )
  expect_warning(
    dct_modify_row(
      dct_filmies,
      scientificName = "Cephalomanes atrovirens Presl",
      taxonomicStatus = "accepted",
      stamp_modified = FALSE
    ),
    paste0(
      "No change to taxonomicStatus or acceptedNameUsageID.*",
      "for selected row \\(taxonID 54115096\\)"
    )
  )
  expect_warning(
    dct_modify_row(
      dct_filmies,
      scientificName = "Trichomanes crassum Copel.",
      acceptedNameUsageID = "54115097",
      taxonomicStatus = "synonym",
      stamp_modified = FALSE
    ),
    paste0(
      "No change to taxonomicStatus or acceptedNameUsageID.*",
      "for selected row \\(taxonID 54133783\\)"
    )
  )
})
