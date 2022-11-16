# The bad data has an taxonomicStatus (third row, "foo") that is not
# a valid value
bad_dat <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  "1", NA, "accepted", "Species foo",
  "2", "1", "synonym", "Species bar",
  "3", NA, "foo", "Species bat"
)
suppressWarnings(
  dct_check_tax_status(bad_dat, on_fail = "summary")
)
# Example of setting valid values of taxonomicStatus via an environmental
# variable
Sys.setenv(VALID_TAX_STATUS = "provisionally accepted, synonym, NA")
tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  "1", NA, "provisionally accepted", "Species foo",
  "2", "1", "synonym", "Species bar",
  "3", NA, NA, "Strange name"
) |>
  dct_check_tax_status()
Sys.unsetenv("VALID_TAX_STATUS")
