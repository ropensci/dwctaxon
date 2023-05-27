# The bad data has an taxonomicStatus (third row, "foo") that is not
# a valid value
bad_dat <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  "1", NA, "accepted", "Species foo",
  "2", "1", "synonym", "Species bar",
  "3", NA, "foo", "Species bat"
)

dct_check_tax_status(bad_dat, on_fail = "summary", quiet = TRUE)

# Example of setting valid values of taxonomicStatus via dct_options()

# First store existing settings, including any changes made by the user
old_settings <- dct_options()

# Change options for valid_tax_status
dct_options(valid_tax_status = "provisionally accepted, synonym, NA")
tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
  "1", NA, "provisionally accepted", "Species foo",
  "2", "1", "synonym", "Species bar",
  "3", NA, NA, "Strange name"
) |>
  dct_check_tax_status()

# Reset options to those before this example was run
do.call(dct_options, old_settings)
