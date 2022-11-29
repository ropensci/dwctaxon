# The bad data has an acceptedNameUsageID (third row, "4") that lacks a
# corresponding taxonID
bad_dat <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  "1", NA, "accepted", "Species foo",
  "2", "1", "synonym", "Species bar",
  "3", "4", "synonym", "Species bat"
)

dct_check_mapping(bad_dat, on_fail = "summary", quiet = TRUE)
