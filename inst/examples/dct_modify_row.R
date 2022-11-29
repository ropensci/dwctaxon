# Swap the accepted / synonym status of
# Cephalomanes crassum (Copel.) M. G. Price
# and Trichomanes crassum Copel.
dct_filmies |>
  dct_modify_row(
    sci_name = "Cephalomanes crassum (Copel.) M. G. Price",
    tax_status = "synonym",
    usage_name = "Trichomanes crassum Copel."
  ) |>
  dct_modify_row(
    sci_name = "Trichomanes crassum Copel.",
    tax_status = "accepted"
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
  sci_name = "Cephalomanes crassum (Copel.) M. G. Price",
  tax_status = "synonym",
  usage_name = "Cephalomanes densinervium (Copel.) Copel."
)
# Apply a set of changes
library(tibble)
updates <- tibble(
  sci_name = c(
    "Cephalomanes atrovirens Presl",
    "Cephalomanes crassum (Copel.) M. G. Price"
  ),
  tax_status = "synonym",
  usage_name = "Trichomanes crassum Copel."
)
dct_filmies |>
  dct_modify_row(args_tbl = updates) |>
  dct_modify_row(
    sci_name = "Trichomanes crassum Copel.",
    tax_status = "accepted"
  )
