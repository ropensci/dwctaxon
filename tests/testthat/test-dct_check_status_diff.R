test_that("check for 'each sci name has single status' works", {
  bad_dat <- tibble::tribble(
    ~taxonID, ~taxonomicStatus, ~scientificName,
    "1", "accepted", "Species foo",
    "2", "variant", "Species foo",
    "3", "synonym", "Species bar",
    "4", "synonym", "Species bar",
    "5", "synonym", "Species foobar",
    "6", NA, "Species foobar",
    "7", "accepted", "Species blah"
  )
  expect_error(
    check_status_diff_p(bad_dat),
    paste0(
      "scientificName detected with multiple different values for ",
      "taxonomicStatus.*",
      "Bad scientificName\\: Species foo, Species foobar"
    )
  )
  expect_equal(
    suppressWarnings(
      check_status_diff_p(bad_dat, on_fail = "summary")
    ),
    tibble::tibble(
      taxonID = c("1", "2", "5", "6"),
      scientificName = c(
        rep("Species foo", 2),
        rep("Species foobar", 2)
      ),
      taxonomicStatus = c("accepted", "variant", "synonym", NA),
      error = rep(
        paste(
          "scientificName detected with multiple different values for",
          "taxonomicStatus"
        ), 4
      ),
      check = rep("check_status_diff", 4)
    )
  )
})
