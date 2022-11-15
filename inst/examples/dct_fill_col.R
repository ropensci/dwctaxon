# Fill acceptedNameUsage with values from scientificName by
# matching acceptedNameUsageID to taxonID
(head(dct_filmies, 5)) |>
  dct_fill_col(
    fill_to = "acceptedNameUsage",
    fill_from = "scientificName",
    match_to = "taxonID",
    match_from = "acceptedNameUsageID"
  )
