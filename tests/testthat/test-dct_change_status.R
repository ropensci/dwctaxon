test_that("fill_usage_name arg works", {
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

test_that("can modify row without changing status", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo"
  )
  expect_equal(
    dct_modify_row(tax_dat, taxon_id = "1", sci_name = "bar"),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      "1", NA, "accepted", "bar"
    )
  )
})

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
  # TODO make tax_status non-required
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

test_that("varieties don't get remapped by default", {
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

test_that("attempt to update without changes returns original data", {
  expect_equal(
    suppressWarnings(
      dct_modify_row(
        dct_filmies,
        sci_name = "Cephalomanes atrovirens Presl",
        tax_status = "accepted name"
      )
    ),
    dct_filmies
  )
  expect_warning(
    dct_modify_row(
      dct_filmies,
      sci_name = "Cephalomanes atrovirens Presl",
      tax_status = "accepted name"
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
      tax_status = "synonym"
    ),
    paste0(
      "No change to taxonomicStatus or acceptedNameUsageID.*",
      "for selected row \\(taxonID 54133783\\)"
    )
  )
})

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
