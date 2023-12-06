library(mockery)

# make data for sharing across tests
base_dat <- tibble::tribble(
  ~taxonID, ~scientificName,
  "1", "foo"
)

# checks ----
test_that("check for duplicated taxonID works", {
  expect_error(
    dct_add_row(base_dat, taxonID = "1", scientificName = "bar"),
    "taxonID in new data must be different from that in existing data"
  )
})

test_that("check for bad col names works", {
  expect_error(
    dct_add_row(
      base_dat,
      taxonID = "2", sci_namea = "hi", stamp_modified = FALSE
    ),
    "Invalid column name"
  )
})

test_that("check agreement between taxonID in data and fill_usage_id works", {
  expect_error(
    dct_add_row(
      data.frame(scientificName = "a"),
      scientificName = "b",
      acceptedNameUsage = "a",
      fill_taxon_id = FALSE,
      stamp_modified = FALSE,
      fill_usage_id = TRUE
    ),
    "tax_dat must include column taxonID if fill_usage_id is TRUE"
  )
})

# warnings ---
test_that("warning about changing taxonID class works", {
  expect_warning(
    dct_add_row(
      tibble::tribble(~taxonID, ~scientificName, 1, "foo"),
      scientificName = "bar"
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
    dct_add_row(base_dat, taxonID = "2", stamp_modified = FALSE),
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
        taxonID = c("2", "3"),
        scientificName = c("bar", "bat"),
        acceptedNameUsageID = c("1", NA),
        taxonomicStatus = c("synonym", NA)
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

test_that("fill_taxon_id argument works", {
  expect_equal(
    dct_add_row(base_dat,
      scientificName = c("bar", "bat"),
      stamp_modified = FALSE
    ),
    tibble::tibble(
      taxonID = c(
        "1",
        digest::digest("bar"),
        digest::digest("bat")
      ),
      scientificName = c("foo", "bar", "bat")
    )
  )
  expect_equal(
    dct_add_row(base_dat,
      scientificName = c("bar", "bat"),
      stamp_modified = FALSE,
      taxon_id_length = 8
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
  expect_error(
    dct_add_row(base_dat,
      scientificName = c("bar", "bat"),
      stamp_modified = FALSE,
      taxon_id_length = "a"
    ),
    "taxon_id_length is not a number"
  )
  expect_error(
    dct_add_row(base_dat,
      scientificName = c("bar", "bat"),
      stamp_modified = FALSE,
      taxon_id_length = c(2, 3)
    ),
    "taxon_id_length is not a number"
  )
  expect_error(
    dct_add_row(base_dat,
      scientificName = c("bar", "bat"),
      stamp_modified = FALSE,
      taxon_id_length = 40
    ),
    "taxon_id_length must be <= 32"
  )
  expect_error(
    dct_add_row(base_dat,
      scientificName = c("bar", "bat"),
      stamp_modified = FALSE,
      taxon_id_length = -1
    ),
    "taxon_id_length must be >= 1"
  )
  expect_error(
    dct_options(taxon_id_length = 50),
    "Option value out of range"
  )
})

test_that("fill_taxon_id warning works", {
  expect_warning(
    dct_add_row(base_dat, scientificName = "bar", taxonID = 2),
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
        scientificName = "Bargenus foosp", taxonomicStatus = "accepted",
        acceptedNameUsageID = NA,
        stamp_modified = FALSE
      ) |>
      dct_add_row(
        scientificName = c("Foogenus boospecies", "Bargenus bkaspecies"),
        acceptedNameUsage = c("Foogenus barspecies", "Bargenus foosp"),
        taxonomicStatus = "synonym",
        stamp_modified = FALSE
      )
  )
})

test_that("fill_usage_id only cares about uniqueness of matching names", {
  # Original data has repeated sci name
  base_dat <- tibble::tibble(
    taxonID = c("1", "2"),
    scientificName = rep("Foogenus barspecies", 2),
    taxonomicStatus = rep("accepted", 2)
  ) |>
    dct_add_row(
      taxonID = "3",
      scientificName = "Bargenus bkaspecies",
      taxonomicStatus = "accepted",
      stamp_modified = FALSE
    )
  # Here the added data is not a match to the duplicated name, so its OK
  expect_snapshot(
    base_dat |>
      dct_add_row(
        scientificName = "Bargenus foosp",
        acceptedNameUsage = "Bargenus bkaspecies",
        taxonomicStatus = "synonym",
        stamp_modified = FALSE
      )
  )
  # Here the added data does hit a duplicated name, should fail
  expect_error(
    base_dat |>
      dct_add_row(
        scientificName = "Foogenus boospecies",
        acceptedNameUsage = "Foogenus barspecies",
        taxonomicStatus = "synonym",
        stamp_modified = FALSE
      ),
    paste(
      "fill_usage_id requires unique match between",
      "acceptedNameUsage and scientificName"
    )
  )
})

test_that("fill_usage_id doesn't create acceptedUsageID column when FALSE", {
  expect_snapshot(
    tibble::tibble(
      taxonID = "123",
      scientificName = "Foogenus barspecies",
      taxonomicStatus = "accepted"
    ) |>
      dct_add_row(
        scientificName = "Bargenus foosp", taxonomicStatus = "accepted",
        stamp_modified = FALSE
      ) |>
      dct_add_row(
        scientificName = c("Foogenus boospecies", "Bargenus bkaspecies"),
        acceptedNameUsage = c("Foogenus barspecies", "Bargenus foosp"),
        taxonomicStatus = "synonym",
        stamp_modified = FALSE,
        fill_usage_id = FALSE
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
    dct_add_row(tax_dat, taxonID = "2", stamp_modified = TRUE),
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
    scientificName = "foo",
    acceptedNameUsageID = NA_character_,
    acceptedNameUsage = NA_character_,
    taxonomicStatus = NA_character_
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
  dct_options(taxon_id_length = 8)
  expect_equal(
    dct_add_row(base_dat,
      scientificName = c("bar", "bat"),
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
  dct_options(reset = TRUE)
})

dct_options(reset = TRUE)

rm(base_dat)
