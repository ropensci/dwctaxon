# Prepare test data
map_to_accepted_dat_good <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~scientificName, ~taxonomicStatus,
  ~taxonRank, ~parentNameUsageID,
  "1", NA, "Goobar foo", "accepted", "species", "2",
  "2", NA, "Goobar", "accepted", "genus", NA
)

map_to_accepted_dat_bad <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~scientificName, ~taxonomicStatus,
  ~taxonRank, ~parentNameUsageID,
  "1", NA, "Goobar foo", "accepted", "species", "3",
  "2", NA, "Goobar", "accepted", "genus", NA,
  "3", "2", "Moobar", "synonym", "genus", NA
)

map_to_accepted_dat_bad_mult <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~scientificName, ~taxonomicStatus,
  ~taxonRank, ~parentNameUsageID,
  "1", NA, "Goobar foo", "accepted", "species", "3",
  "2", NA, "Goobar", "accepted", "genus", NA,
  "3", "2", "Moobar", "synonym", "genus", NA,
  "4", NA, "Goobar fla", "accepted", "species", "3"
)

test_that("No error is thrown for good data", {
  expect_no_error(
    check_map_to_parent_accepted(map_to_accepted_dat_good)
  )
  expect_no_error(
    dct_validate(map_to_accepted_dat_good, check_mapping_parent_accepted = TRUE)
  )
})

test_that("Error is thrown for bad data", {
  bad_dat_msg <- paste0(
    "check_map_to_parent_accepted failed.*",
    "name\\(s\\) detected with parentNameUsageID that corresponds to a ",
    "synonym.*",
    "Bad taxonID\\: 1.*",
    "Bad scientificName\\: Goobar foo.*",
    "Bad parentNameUsageID\\: 3"
  )
  expect_error(
    check_map_to_parent_accepted(map_to_accepted_dat_bad),
    bad_dat_msg
  )
  expect_error(
    dct_validate(map_to_accepted_dat_bad, check_mapping_parent_accepted = TRUE),
    bad_dat_msg
  )
  bad_dat_msg_mult <- paste0(
    "check_map_to_parent_accepted failed.*",
    "name\\(s\\) detected with parentNameUsageID that corresponds to a ",
    "synonym.*",
    "Bad taxonID\\: 1, 4.*",
    "Bad scientificName\\: Goobar foo, Goobar fla.*",
    "Bad parentNameUsageID\\: 3, 3"
  )
  expect_error(
    check_map_to_parent_accepted(map_to_accepted_dat_bad_mult),
    bad_dat_msg_mult
  )
  expect_error(
    dct_validate(
      map_to_accepted_dat_bad_mult,
      check_mapping_parent_accepted = TRUE
    ),
    bad_dat_msg_mult
  )
})

test_that("Error is thrown if required columns are missing", {
  expect_error(
    dct_validate(
      dplyr::select(map_to_accepted_dat_good, -parentNameUsageID),
      check_mapping_parent_accepted = TRUE
    ),
    paste(
      "check_map_to_parent_accepted requires column parentNameUsageID",
      "in input data"
    )
  )
})

test_that("Error is thrown if required checks are missing", {
  expect_error(
    dct_validate(
      map_to_accepted_dat_good,
      check_mapping_parent_accepted = TRUE,
      check_taxon_id = FALSE
    ),
    "check_mapping_parent_accepted requires check_taxon_id to be TRUE"
  )
  expect_error(
    dct_validate(
      map_to_accepted_dat_good,
      check_mapping_parent_accepted = TRUE,
      check_mapping_parent = FALSE
    ),
    "check_mapping_parent_accepted requires check_mapping_parent to be TRUE"
  )
  expect_error(
    dct_validate(
      map_to_accepted_dat_good,
      check_mapping_parent_accepted = TRUE,
      check_tax_status = FALSE
    ),
    "check_mapping_parent_accepted requires check_tax_status to be TRUE"
  )
})

rm(map_to_accepted_dat_good)
rm(map_to_accepted_dat_bad)
rm(map_to_accepted_dat_bad_mult)
