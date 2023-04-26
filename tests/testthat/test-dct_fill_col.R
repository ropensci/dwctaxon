# input format ----

# make data for sharing across tests
test_dat <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  "1", NA, "accepted", "foo",
  "2", "1", "synonym", "foobar"
)

test_that("input format checks work", {
  rand_nonstring_input <- function() {
    sample(
      list(1, 1L, Inf, NA_complex_, c("a", "b")),
      1
    )[[1]]
  }
  expect_error(
    dct_fill_col(
      test_dat,
      fill_to = rand_nonstring_input(),
      fill_from = "scientificName",
      match_to = "taxonID",
      match_from = "acceptedNameUsageID"
    ),
    "fill_to is not a string"
  )
  expect_error(
    dct_fill_col(
      test_dat,
      fill_to = "acceptedNameUsage",
      fill_from = rand_nonstring_input(),
      match_to = "taxonID",
      match_from = "acceptedNameUsageID"
    ),
    "fill_from is not a string"
  )
  expect_error(
    dct_fill_col(
      test_dat,
      fill_to = "acceptedNameUsage",
      fill_from = "scientificName",
      match_to = rand_nonstring_input(),
      match_from = "acceptedNameUsageID"
    ),
    "match_to is not a string"
  )
  expect_error(
    dct_fill_col(
      test_dat,
      fill_to = "acceptedNameUsage",
      fill_from = "scientificName",
      match_to = "taxonID",
      match_from = rand_nonstring_input()
    ),
    "match_from is not a string"
  )
  expect_error(
    dct_fill_col(
      rand_nonstring_input(),
      fill_to = "acceptedNameUsage",
      fill_from = "scientificName",
      match_to = "taxonID",
      match_from = "acceptedNameUsageID"
    ),
    "tax_dat must be of class 'data.frame'"
  )
  expect_error(
    dct_fill_col(
      test_dat,
      fill_to = "acceptedNameUsage",
      fill_from = "parentNameUsageID", # valid DwC name but not in data
      match_to = "taxonID",
      match_from = "acceptedNameUsageID",
      "fill_from must be an existing column in tax_dat"
    )
  )
  expect_error(
    dct_fill_col(
      test_dat,
      fill_to = "acceptedNameUsage",
      fill_from = "scientificName",
      match_to = "taxonID",
      match_from = "parentNameUsageID", # valid DwC name but not in data
      "match_from must be an existing column in tax_dat"
    )
  )
  expect_error(
    dct_fill_col(
      test_dat,
      fill_to = "acceptedNameUsage",
      fill_from = "scientificName",
      match_to = "parentNameUsageID", # valid DwC name but not in data
      match_from = "acceptedNameUsageID", 
      "match_to must be an existing column in tax_dat"
    )
  )
})

test_that("dct_fill_col() input checks give meaningful errors", {
  expect_snapshot({
    (expect_error(dct_fill_col(test_dat, fill_to = "a")))
    (expect_error(dct_fill_col(test_dat, fill_from = "a")))
    (expect_error(dct_fill_col(test_dat, match_to = "a")))
    (expect_error(dct_fill_col(test_dat, match_from = "a")))
  })
})

# functionality ----

test_that("sequential filling works", {
  start_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    ~acceptedNameUsage, ~parentNameUsage, ~parentNameUsageID,
    "1", NA, "accepted", "foo df", NA, NA, "3",
    "2", NA, "synonym", "foo dfa", "foo df", NA, "3",
    "3", NA, "accepted", "foo", NA, NA, NA,
    "4", NA, "accepted", "bar ba", NA, NA, "6",
    "5", "4", "synonym", "bar baf", NA, NA, "6",
    "6", NA, "accepted", "bar", NA, NA, NA
  )
  filled_dat <- tibble::tribble(
    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
    ~acceptedNameUsage, ~parentNameUsage, ~parentNameUsageID,
    "1", NA, "accepted", "foo df", NA, "foo", "3",
    "2", "1", "synonym", "foo dfa", "foo df", "foo", "3",
    "3", NA, "accepted", "foo", NA, NA, NA,
    "4", NA, "accepted", "bar ba", NA, "bar", "6",
    "5", "4", "synonym", "bar baf", "bar ba", "bar", "6",
    "6", NA, "accepted", "bar", NA, NA, NA
  )
  expect_equal(
    dct_fill_col(
      start_dat,
      fill_to = "acceptedNameUsageID",
      fill_from = "taxonID",
      match_from = "acceptedNameUsage",
      match_to = "scientificName"
    ) |>
      dct_fill_col(
        fill_to = "parentNameUsage",
        fill_from = "scientificName",
        match_from = "parentNameUsageID",
        match_to = "taxonID"
      ) |>
      dct_fill_col(
        fill_to = "acceptedNameUsage",
        fill_from = "scientificName",
        match_from = "acceptedNameUsageID",
        match_to = "taxonID"
      ),
    filled_dat
  )
})

test_that("filling adds a new column if needed", {
  expect_equal(
    dct_fill_col(
      tibble::tribble(
        ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
        "1", NA, "accepted", "foo bla",
        "2", "1", "synonym", "foo bar"
      ),
      fill_to = "acceptedNameUsage",
      fill_from = "scientificName",
      match_from = "acceptedNameUsageID",
      match_to = "taxonID"
    ),
    tibble::tribble(
      ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
      ~acceptedNameUsage,
      "1", NA, "accepted", "foo bla", NA,
      "2", "1", "synonym", "foo bar", "foo bla"
    )
  )
})

rm(test_dat)
