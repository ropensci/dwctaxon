# Tests for major arguments ----

library(mockery)

# - taxon_id
# - sci_name
test_that("sci_name works to identify or modify row", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo")
  expect_equal(
    dct_modify_row(
      tax_dat, sci_name = "foo", tax_status = "maybe accepted",
      stamp_modified = FALSE),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName, 
      "1", NA, "maybe accepted", "foo")
  )
  expect_equal(
    dct_modify_row(
      tax_dat, taxon_id = "1", sci_name = "foobar",
      stamp_modified = FALSE),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName, 
      "1", NA, "accepted", "foobar")
  )
})

# - tax_status
# - usage_id
# - remap_names
test_that("tax_status, usage_id, and remap_names arguments work", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo",
    "2", NA, "accepted", "bar",
    "3", "2", "synonym", "bat"
  )
  expect_equal(
    dct_modify_row(
      tax_dat, taxon_id = "2", tax_status = "synonym", usage_id = "1",
      stamp_modified = FALSE
    ),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "1", "synonym", "bat"
    )
  )
  expect_equal(
    dct_modify_row(
      tax_dat, taxon_id = "2", tax_status = "synonym", usage_id = "1",
      stamp_modified = FALSE, remap_names = FALSE
    ),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "2", "synonym", "bat"
    )
  )
})

# - clear_usage_id
test_that("clear_usage_id argument works", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo",
    "2", "1", "synonym", "bar"
    )
  expect_equal(
    dct_modify_row(
      tax_dat, taxon_id = "2", tax_status = "accepted",
      stamp_modified = FALSE
    ),
    tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "foo",
      "2", NA_character_, "accepted", "bar"
    )
  )
  expect_equal(
    dct_modify_row(
      tax_dat, taxon_id = "2", tax_status = "accepted",
      clear_usage_id = FALSE, stamp_modified = FALSE
    ),
    tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "foo",
      "2", "1", "accepted", "bar"
    )
  )
})

# - fill_usage_name
test_that("fill_usage_name argument works", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo",
    "2", NA, "accepted", "bar"
  )
  expect_equal(
    dct_modify_row(
      tax_dat, taxon_id = "2", tax_status = "synonym", usage_id = "1",
      fill_usage_name = TRUE, stamp_modified = FALSE
    ),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      ~acceptedNameUsage,
      "1", NA, "accepted", "foo", NA,
      "2", "1", "synonym", "bar", "foo"
    )
  )
})

# - remap_names
test_that("fill_usage_name works with remap_names", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo",
    "2", NA, "accepted", "bar",
    "3", "2", "synonym", "bat"
  )
  expect_equal(
    dct_modify_row(
      tax_dat, taxon_id = "2", tax_status = "synonym", usage_id = "1",
      fill_usage_name = TRUE, stamp_modified = FALSE
    ),
    tibble::tribble(
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
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA_character_, "accepted", "foo",
    "2", NA_character_, "accepted", "bar",
    "3", "2", "variety", "bat"
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxon_id = "2",
      tax_status = "synonym",
      usage_name = "foo",
      stamp_modified = FALSE
    ),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA_character_, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "2", "variety", "bat"
    )
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxon_id = "2",
      tax_status = "synonym",
      usage_name = "foo",
      remap_variant = TRUE,
      stamp_modified = FALSE
    ),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA_character_, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "1", "variety", "bat"
    )
  )
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA_character_, "accepted", "foo",
    "2", NA_character_, "accepted", "bar",
    "3", "2", "putative variety", "bat"
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxon_id = "2",
      tax_status = "synonym",
      usage_name = "foo",
      stamp_modified = FALSE
    ),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA_character_, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "2", "putative variety", "bat"
    )
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxon_id = "2",
      tax_status = "synonym",
      usage_name = "foo",
      remap_variant = TRUE,
      stamp_modified = FALSE
    ),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA_character_, "accepted", "foo",
      "2", "1", "synonym", "bar",
      "3", "1", "putative variety", "bat"
    )
  )
})

# - stamp_modified
test_that("stamp_modified argument works", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA_character_, "accepted", "foo"
  )
  # Replace time stamp with date stamp for testing
  stub(dct_modify_row, "Sys.time", Sys.Date(), depth = 2)
  expect_equal(
    dct_modify_row(tax_dat, "1", tax_status = "bar"),
    tibble::tribble(
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
  dat <- tibble::tribble(
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
        taxon_id = c(1, 2),
        tax_status = "synonym",
        usage_id = 3,
        stamp_modified = FALSE
      )
    ),
    tibble::tibble(
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
        sci_name = c("foo", "bar"),
        tax_status = "synonym",
        usage_id = 3,
        stamp_modified = FALSE
      )
    ),
    tibble::tibble(
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
      args_tbl = tibble::tibble(
        taxon_id = c(1, 2),
        tax_status = "synonym",
        usage_id = 3,
        stamp_modified = FALSE,
        nameAccordingTo = "me"
        )
    ),
    tibble::tibble(
      taxonID = c(1, 2, 3),
      acceptedNameUsageID = c(3, 3, NA),
      taxonomicStatus = c("synonym", "synonym", "accepted"),
      scientificName = c("foo", "bar", "bat"),
      nameAccordingTo = c("me", "me", "her")
    )
  )
})

# Other tests ----

test_that("can modify row without changing status", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo"
  )
  expect_equal(
    dct_modify_row(
      tax_dat, taxon_id = "1", sci_name = "bar", stamp_modified = FALSE),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "bar"
    )
  )
})

test_that("other terms can be added", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo",
    "2", NA, "accepted", "bar",
    "3", "2", "variety", "bat"
  )
  expect_equal(
    dct_modify_row(
      tax_dat,
      taxon_id = "2",
      tax_status = "synonym",
      usage_name = "foo",
      stamp_modified = FALSE,
      nameAccordingTo = "Me",
      nameAccordingToID = 1
    ),
    tibble::tribble(
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
        taxon_id = c("2", "3"),
        tax_status = c("synonym", "accepted"),
        usage_name = c("foo", NA),
        clear_usage_id = c(FALSE, TRUE),
        stamp_modified = rep(FALSE, 2),
        nameAccordingTo = rep("Me", 2),
        nameAccordingToID = rep(1, 2)
      )
    ),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      ~nameAccordingTo, ~nameAccordingToID,
      "1", NA, "accepted", "foo", NA, NA,
      "2", "1", "synonym", "bar", "Me", 1,
      "3", NA, "accepted", "bat", "Me", 1
    )
  )
})

test_that("argument aliases work", {
  dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo",
    "2", "1", "synonym", "bar",
    "3", NA, "accepted", "bat"
    )
  expect_equal(
    dct_modify_row(dat, taxon_id = "2", tax_status = "accepted"),
    dct_modify_row(dat, taxonID = "2", tax_status = "accepted")
  )
  expect_equal(
    dct_modify_row(dat, sci_name = "bar", tax_status = "accepted"),
    dct_modify_row(dat, scientificName = "bar", tax_status = "accepted")
  )
  expect_equal(
    dct_modify_row(
      dat, taxon_id = "2", usage_id = "3", tax_status = "synonym"),
    dct_modify_row(
      dat, taxon_id = "2", acceptedNameUsageID = "3", tax_status = "synonym")
  )
  expect_equal(
    dct_modify_row(
      dat, taxon_id = "2", usage_name = "bat", tax_status = "synonym"),
    dct_modify_row(
      dat, taxon_id = "2", acceptedNameUsage = "bat", tax_status = "synonym")
  )
  expect_equal(
    dct_modify_row(
      dat, taxon_id = "2", usage_name = "bat", tax_status = "synonym"),
    dct_modify_row(
      dat, taxon_id = "2", acceptedNameUsage = "bat",
      taxonomicStatus = "synonym", clear_usage_id = FALSE
    )
  )
})

test_that("attempt to update without changes returns original data", {
  expect_equal(
    suppressWarnings(
      dct_modify_row(
        dct_filmies,
        sci_name = "Cephalomanes atrovirens Presl",
        tax_status = "accepted name",
        stamp_modified = FALSE
      )
    ),
    dct_filmies
  )
  expect_warning(
    dct_modify_row(
      dct_filmies,
      sci_name = "Cephalomanes atrovirens Presl",
      tax_status = "accepted name",
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
      sci_name = "Trichomanes crassum Copel.",
      usage_id = "54115097",
      tax_status = "synonym",
      stamp_modified = FALSE
    ),
    paste0(
      "No change to taxonomicStatus or acceptedNameUsageID.*",
      "for selected row \\(taxonID 54133783\\)"
    )
  )
})
