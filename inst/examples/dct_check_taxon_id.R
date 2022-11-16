suppressWarnings(
  dct_check_taxon_id(
    data.frame(taxonID = NA_character_),
    on_fail = "summary"
  )
)
dct_check_taxon_id(data.frame(taxonID = 1))
