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
    "No change to taxonomicStatus or acceptedNameUsageID; returning original input"
  )
  expect_warning(
    dct_change_status(
      dct_filmies,
      sci_name = "Trichomanes crassum Copel.",
      usage_id = "54115097",
      new_status = "synonym"
    ),
    "No change to taxonomicStatus or acceptedNameUsageID; returning original input"
  )
})

test_that("args_tbl can be used to update data", {
  dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    1, NA, "accepted", "foo",
    2, NA, "accepted", "bar",
    3, NA, "accepted", "bat"
  )
  expect_equal(
    dct_change_status(
      dat,
      args_tbl = data.frame(
        taxon_id = c(1, 2),
        new_status = "synonym",
        usage_id = 3
      )
    ) |>
    dplyr::select(-modified),
    tibble::tibble(
      taxonID = c(1, 2, 3),
      acceptedNameUsageID = c(3, 3, NA),
      taxonomicStatus = c("synonym", "synonym", "accepted"),
      scientificName = c("foo", "bar", "bat")
    )
  )
  expect_equal(
    dct_change_status(
      dat,
      args_tbl = data.frame(
        sci_name = c("foo", "bar"),
        new_status = "synonym",
        usage_id = 3
      )
    ) |>
    dplyr::select(-modified),
    tibble::tibble(
      taxonID = c(1, 2, 3),
      acceptedNameUsageID = c(3, 3, NA),
      taxonomicStatus = c("synonym", "synonym", "accepted"),
      scientificName = c("foo", "bar", "bat")
    )
  )
})
