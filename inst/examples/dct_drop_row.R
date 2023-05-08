# Can drop rows by scientificName or taxonID
dct_filmies |>
  dct_drop_row(scientificName = "Cephalomanes atrovirens Presl")

dct_filmies |>
  dct_drop_row(taxonID = "54133783")

# Can drop multiple rows at once by providing multiple values for
# scientificName or taxonID
dct_filmies |>
  dct_drop_row(
    scientificName = c(
      "Cephalomanes atrovirens Presl",
      "Trichomanes crassum Copel."
    )
  )

dct_filmies |>
  dct_drop_row(
    taxonID = c(
      "54133783", "54133783"
    )
  )
