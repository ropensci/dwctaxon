library(mockery)

# make data for sharing across tests
base_dat <- tibble::tribble(
  ~taxonID, ~scientificName,
  "1", "foo"
)

# checks ----
test_that("check for duplicated taxonID works", {
  expect_error(
    dct_add_row(base_dat, taxon_id = "1", sci_name = "bar"),
    "taxonID in new data must be different from that in existing data"
  )
})

test_that("check for bad col names works", {
  expect_error(
    dct_add_row(
      base_dat,
      taxon_id = "2", sci_namea = "hi", stamp_modified = FALSE
    ),
    "Invalid column name"
  )
})

# warnings ---
test_that("warning about changing taxonID class works", {
  expect_warning(
    dct_add_row(
      tibble::tribble(~taxonID, ~scientificName, 1, "foo"),
      sci_name = "bar"
    ),
    paste(
      "Class of taxonID column changed in either new_dat or tax_dat",
      "so new data could be added"
    )
  )
})

# arguments ----
test_that("basic adding works", {
  expect_equal(
    dct_add_row(base_dat, taxon_id = "2", stamp_modified = FALSE),
    tibble::tribble(
      ~taxonID, ~scientificName,
      "1", "foo",
      "2", NA
    )
  )
})

test_that("new_dat argument works", {
  expect_equal(
    dct_add_row(
      base_dat,
      new_dat = tibble::tibble(
        taxon_id = c("2", "3"),
        sci_name = c("bar", "bat"),
        usage_id = c("1", NA),
        tax_status = c("synonym", NA)
      ),
      stamp_modified = FALSE
    ),
    tibble::tribble(
      ~taxonID, ~scientificName, ~acceptedNameUsageID, ~taxonomicStatus,
      "1", "foo", NA, NA,
      "2", "bar", "1", "synonym",
      "3", "bat", NA, NA,
    )
  )
})

test_that("argument aliases work", {
  expect_equal(
    dct_add_row(
      base_dat,
      taxon_id = "2", sci_name = "bar", usage_id = "1",
      tax_status = "synonym",
      stamp_modified = FALSE
    ),
    tibble::tribble(
      ~taxonID, ~scientificName, ~acceptedNameUsageID, ~taxonomicStatus,
      "1", "foo", NA, NA,
      "2", "bar", "1", "synonym"
    )
  )
  expect_equal(
    dct_add_row(
      base_dat,
      taxon_id = "2", sci_name = "bar", usage_id = "1",
      tax_status = "synonym",
      stamp_modified = FALSE
    ),
    dct_add_row(
      base_dat,
      taxonID = "2", scientificName = "bar",
      acceptedNameUsageID = "1",
      taxonomicStatus = "synonym",
      stamp_modified = FALSE
    )
  )
})

test_that("fill_taxon_id argument works", {
  expect_equal(
    dct_add_row(base_dat,
      sci_name = c("bar", "bat"),
      stamp_modified = FALSE
    ),
    tibble::tibble(
      taxonID = c(
        "1",
        digest::digest("bar") |> substr(1, 8),
        digest::digest("bat") |> substr(1, 8)
      ),
      scientificName = c("foo", "bar", "bat")
    )
  )
})

test_that("fill_taxon_id warning works", {
  expect_warning(
    dct_add_row(base_dat, sci_name = "bar", taxon_id = 2),
    paste(
      "Class of taxonID column changed in either new_dat or tax_dat so",
      "new data could be added"
    )
  )
})

# fill_usage_id is TRUE by default
test_that("fill_usage_id works", {
  expect_snapshot(
    tibble::tibble(
      taxonID = "123",
      scientificName = "Foogenus barspecies",
      taxonomicStatus = "accepted"
    ) |>
      dct_add_row(
        sci_name = "Bargenus foosp", tax_status = "accepted", usage_id = NA,
        stamp_modified = FALSE
      ) |>
      dct_add_row(
        sci_name = c("Foogenus boospecies", "Bargenus bkaspecies"),
        usage_name = c("Foogenus barspecies", "Bargenus foosp"),
        tax_status = "synonym",
        stamp_modified = FALSE
      )
  )
})

test_that("fill_usage_id doesn't create acceptedUsageID column", {
  expect_snapshot(
    tibble::tibble(
      taxonID = "123",
      scientificName = "Foogenus barspecies",
      taxonomicStatus = "accepted"
    ) |>
      dct_add_row(
        sci_name = "Bargenus foosp", tax_status = "accepted",
        stamp_modified = FALSE
      ) |>
      dct_add_row(
        sci_name = c("Foogenus boospecies", "Bargenus bkaspecies"),
        usage_name = c("Foogenus barspecies", "Bargenus foosp"),
        tax_status = "synonym",
        stamp_modified = FALSE
      )
  )
})

test_that("stamp_modified argument works", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA_character_, "accepted", "foo"
  )
  # Replace time stamp with date stamp for testing
  stub(dct_add_row, "Sys.time", Sys.Date(), depth = 2)
  expect_equal(
    dct_add_row(tax_dat, taxon_id = "2", stamp_modified = TRUE),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      ~modified,
      "1", NA_character_, "accepted", "foo", NA,
      "2", NA_character_, NA, NA, as.character(Sys.Date())
    )
  )
})

# options ----

test_that("setting validation args via options works", {
  add_dat <- data.frame(
    sci_name = "foo",
    usage_id = NA_character_,
    usage_name = NA_character_,
    tax_status = NA_character_
  )
  expect_error(
    dct_add_row(base_dat, new_dat = add_dat, strict = TRUE),
    "scientificName detected with duplicated value"
  )
  dct_options(check_sci_name = FALSE, stamp_modified = FALSE)
  expect_snapshot({
    (expect_no_error(dct_add_row(base_dat, new_dat = add_dat, strict = TRUE)))
  })
  dct_options(reset = TRUE)
})

rm(base_dat)
