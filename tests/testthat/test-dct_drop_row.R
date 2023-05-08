test_dat <- data.frame(
  taxonID = 1:5,
  scientificName = letters[1:5]
)

test_that("Drop by taxonID works", {
  expect_equal(
    dct_drop_row(test_dat, taxonID = c(1, 2)),
    data.frame(
      taxonID = 1:5,
      scientificName = letters[1:5]
    )[3:5, ]
  )
  expect_equal(
    drop_row_by_taxon_id(test_dat, taxon_id = c(1, 2)),
    data.frame(
      taxonID = 1:5,
      scientificName = letters[1:5]
    )[3:5, ]
  )
})

test_that("Drop by scientificName works", {
  expect_equal(
    dct_drop_row(test_dat, scientificName = c("a", "b")),
    data.frame(
      taxonID = 1:5,
      scientificName = letters[1:5]
    )[3:5, ]
  )
  expect_equal(
    drop_row_by_sci_name(test_dat, sci_name = c("a", "b")),
    data.frame(
      taxonID = 1:5,
      scientificName = letters[1:5]
    )[3:5, ]
  )
})

test_that("Checks work", {
  expect_error(
    dct_drop_row(test_dat, scientificName = "a", taxonID = 1),
    "Either taxonID or scientificName must be provided, but not both"
  )
  expect_error(
    dct_drop_row(test_dat),
    "Either taxonID or scientificName must be provided, but not both"
  )
  expect_error(
    dct_drop_row(data.frame(taxonID = "a"), scientificName = "b"),
    paste(
      "tax_dat must include column scientificName,",
      "which must be a character vector with unique, non-missing values"
    )
  )
  expect_error(
    dct_drop_row(
      data.frame(taxonID = 1:3, scientificName = c("a", "b", "b")),
      scientificName = "b"
    ),
    paste(
      "tax_dat must include column scientificName,",
      "which must be a character vector with unique, non-missing values"
    )
  )
  expect_error(
    dct_drop_row(
      data.frame(taxonID = 1:3, scientificName = c("a", "b", NA)),
      scientificName = "b"
    ),
    paste(
      "tax_dat must include column scientificName,",
      "which must be a character vector with unique, non-missing values"
    )
  )
  expect_error(
    dct_drop_row(data.frame(scientificName = "a"), taxonID = "b"),
    paste(
      "tax_dat must include column taxonID, which must be a character or",
      "numeric vector with unique, non-missing values"
    )
  )
  expect_error(
    dct_drop_row(
      data.frame(taxonID = c("a", "b", "b"), scientificName = c("a", "b", "c")),
      taxonID = "b"
    ),
    paste(
      "tax_dat must include column taxonID, which must be a character or",
      "numeric vector with unique, non-missing values"
    )
  )
  expect_error(
    dct_drop_row(
      data.frame(taxonID = c("a", "b", NA), scientificName = c("a", "b", "c")),
      taxonID = "b"
    ),
    paste(
      "tax_dat must include column taxonID, which must be a character or",
      "numeric vector with unique, non-missing values"
    )
  )
})
