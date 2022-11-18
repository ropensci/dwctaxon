test_that("get_dct_opt() works", {
  expect_equal(
    get_dct_opt("check_taxon_id"),
    TRUE
  )
  expect_error(
    get_dct_opt("whazzat"),
    "'opt' is not the name of an option"
  )
})

test_that("dct_options() can be set and unset", {
  expect_equal(
    inherits(dct_options(), "list"),
    TRUE
  )
  dct_options(check_taxon_id = FALSE)
  expect_equal(
    dct_options()$check_taxon_id,
    FALSE
  )
  dct_options(check_taxon_id = TRUE)
  expect_equal(
    dct_options()$check_taxon_id,
    TRUE
  )
  dct_options(reset = TRUE)
  expect_equal(
    dct_options()$check_taxon_id,
    TRUE
  )
})
