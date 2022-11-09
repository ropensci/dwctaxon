# make data for sharing across tests
base_dat <- tibble::tribble(
  ~taxonID, ~scientificName,
  "1", "foo"
)

test_that("basic adding works", {
  expect_equal(
    dct_add_row(base_dat, taxon_id = "2"),
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
      )
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
      tax_status = "synonym"
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
      tax_status = "synonym"
    ),
    dct_add_row(
      base_dat,
      taxonID = "2", scientificName = "bar",
      acceptedNameUsageID = "1",
      taxonomicStatus = "synonym"
    )
  )
})

test_that("fill_taxon_id argument works", {
  expect_equal(
    dct_add_row(base_dat, sci_name = c("bar", "bat")),
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
    dct_add_row(base_dat, sci_name = "bar", taxon_id = 1),
    paste(
      "Class of taxonID column changed in either new_dat or tax_dat so",
      "new data could be added"
    )
  )
})
