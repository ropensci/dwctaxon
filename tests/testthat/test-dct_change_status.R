test_that("other terms can be added", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA, "accepted", "foo",
    "2", NA, "accepted", "bar",
    "3", "2", "variety", "bat"
  )
  expect_equal(
    dct_change_status(
      tax_dat,
      taxon_id = "2",
      new_status = "synonym",
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
    dct_change_status(
      tax_dat,
      args_tbl = data.frame(
        taxon_id = c("2", "3"),
        new_status = c("synonym", "accepted"),
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

test_that("varieties don't get remapped by default", {
  tax_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    "1", NA_character_, "accepted", "foo",
    "2", NA_character_, "accepted", "bar",
    "3", "2", "variety", "bat"
  )
  expect_equal(
    dct_change_status(
      tax_dat,
      taxon_id = "2",
      new_status = "synonym",
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
    dct_change_status(
      tax_dat,
      taxon_id = "2",
      new_status = "synonym",
      usage_name = "foo",
      remap_variety = TRUE,
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
    dct_change_status(
      tax_dat,
      taxon_id = "2",
      new_status = "synonym",
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
    dct_change_status(
      tax_dat,
      taxon_id = "2",
      new_status = "synonym",
      usage_name = "foo",
      remap_variety = TRUE,
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
      dct_change_status(
        dct_filmies,
        sci_name = "Cephalomanes atrovirens Presl",
        new_status = "accepted name"
      )
    ),
    dct_filmies
  )
  expect_warning(
    dct_change_status(
      dct_filmies,
      sci_name = "Cephalomanes atrovirens Presl",
      new_status = "accepted name"
    ),
    paste0(
      "No change to taxonomicStatus or acceptedNameUsageID.*",
      "for selected row \\(taxonID 54115096\\)"
    )
  )
  expect_warning(
    dct_change_status(
      dct_filmies,
      sci_name = "Trichomanes crassum Copel.",
      usage_id = "54115097",
      new_status = "synonym"
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
    dct_change_status(
      dat,
      args_tbl = data.frame(
        taxon_id = c(1, 2),
        new_status = "synonym",
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
    dct_change_status(
      dat,
      args_tbl = data.frame(
        sci_name = c("foo", "bar"),
        new_status = "synonym",
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
    dct_change_status(
      dat,
      args_tbl = tibble::tibble(
        taxon_id = c(1, 2),
        new_status = "synonym",
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
