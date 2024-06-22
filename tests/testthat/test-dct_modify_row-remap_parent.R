library(tibble)

dct_options(reset = TRUE)
dct_options(stamp_modified = FALSE)
dct_options(remap_parent = TRUE)

# Prepare test data ----
# Two species: one is a synonym of the other, in different genera
test_dat <- tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  ~parentNameUsageID, ~parentNameUsage, ~taxonRank,
  1, NA, "accepted", "Goobar foo", 3, "Goobar", "species",
  2, 1, "synonym", "Foobar foo", 4, "Foobar", "species",
  3, NA, "accepted", "Goobar", NA, NA, "genus",
  4, 3, "synonym", "Foobar", NA, NA, "genus",
  5, 1, "synonym", "Foobar blah", 3, "Foobar", "species"
) |>
  # Swap the synonym/accepted status of the genera
  dct_modify_row(
    scientificName = "Foobar", taxonomicStatus = "accepted"
  ) |>
  dct_modify_row(
    scientificName = "Goobar",
    acceptedNameUsageID = 4,
    taxonomicStatus = "synonym"
  )

# Change parent of target row ----

new_dat_1 <-
  test_dat |>
  # Update the species to accepted
  # This step should change parentNameUsageID of Foobar foo to 4
  dct_modify_row(
    scientificName = "Foobar foo",
    taxonomicStatus = "accepted"
  )

expected_dat_1 <- tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  ~parentNameUsageID, ~parentNameUsage, ~taxonRank,
  1, NA, "accepted", "Goobar foo", 3, "Goobar", "species",
  2, NA, "accepted", "Foobar foo", 4, "Foobar", "species",
  3, 4, "synonym", "Goobar", NA, NA, "genus",
  4, NA, "accepted", "Foobar", NA, NA, "genus",
  5, 1, "synonym", "Foobar blah", 3, "Foobar", "species"
)

test_that(
  "remap_parent changes the parentNameUsageID to that of the accepted name", {
    expect_equal(new_dat_1, expected_dat_1)
  }
)

# Change parent of synonyms of target row ----

# this step should change parentNameUsageID of Goobar foo 4
# AND Foobar blah to 4
new_dat_2 <-
  new_dat_1 |>
  dct_modify_row(
    scientificName = "Goobar foo",
    acceptedNameUsageID = 2,
    taxonomicStatus = "synonym"
  )

expected_dat_2 <- tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  ~parentNameUsageID, ~parentNameUsage, ~taxonRank,
  1, 2, "synonym", "Goobar foo", 4, "Foobar", "species",
  2, NA, "accepted", "Foobar foo", 4, "Foobar", "species",
  3, 4, "synonym", "Goobar", NA, NA, "genus",
  4, NA, "accepted", "Foobar", NA, NA, "genus",
  5, 2, "synonym", "Foobar blah", 4, "Foobar", "species"
)

test_that(
  "remap_parent changes the parentNameUsageID of remapped synonyms", {
    expect_equal(new_dat_2, expected_dat_2)
  }
)

# Effect of remap_parent ----

new_dat_3 <-
  test_dat |>
  # Update the species to accepted
  # This step should NOT change parentNameUsageID of Foobar foo to 4
  dct_modify_row(
    scientificName = "Foobar foo",
    taxonomicStatus = "accepted",
    remap_parent = FALSE
  )

expected_dat_3  <- tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  ~parentNameUsageID, ~parentNameUsage, ~taxonRank,
  1, NA, "accepted", "Goobar foo", 3, "Goobar", "species",
  2, NA, "accepted", "Foobar foo", 4, "Foobar", "species",
  3, 4, "synonym", "Goobar", NA, NA, "genus",
  4, NA, "accepted", "Foobar", NA, NA, "genus",
  5, 1, "synonym", "Foobar blah", 3, "Foobar", "species"
)

test_that(
  "remap_parent has no effect when FALSE", {
    expect_equal(new_dat_3, expected_dat_3)
  }
)

dct_options(reset = TRUE)
