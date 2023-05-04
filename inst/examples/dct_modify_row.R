# Swap the accepted / synonym status of
# Cephalomanes crassum (Copel.) M. G. Price
# and Trichomanes crassum Copel.
dct_filmies |>
  dct_modify_row(
    scientificName = "Cephalomanes crassum (Copel.) M. G. Price",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Trichomanes crassum Copel."
  ) |>
  dct_modify_row(
    scientificName = "Trichomanes crassum Copel.",
    taxonomicStatus = "accepted"
  ) |>
  dct_validate(
    check_tax_status = FALSE,
    check_mapping_accepted_status = FALSE,
    check_sci_name = FALSE
  )
# Sometimes changing one name will affect others, if they map
# to the new synonym
dct_modify_row(
  tax_dat = dct_filmies |> head(),
  scientificName = "Cephalomanes crassum (Copel.) M. G. Price",
  taxonomicStatus = "synonym",
  acceptedNameUsage = "Cephalomanes densinervium (Copel.) Copel."
)
# Apply a set of changes
library(tibble)
updates <- tibble(
  scientificName = c(
    "Cephalomanes atrovirens Presl",
    "Cephalomanes crassum (Copel.) M. G. Price"
  ),
  taxonomicStatus = "synonym",
  acceptedNameUsage = "Trichomanes crassum Copel."
)
dct_filmies |>
  dct_modify_row(args_tbl = updates) |>
  dct_modify_row(
    scientificName = "Trichomanes crassum Copel.",
    taxonomicStatus = "accepted"
  )
