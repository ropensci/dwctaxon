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
