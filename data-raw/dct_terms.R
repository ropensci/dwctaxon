## code to prepare `dct_terms` dataset goes here

# Specify current URL with Darwin Core taxon terms
dwc_taxon_url <- "https://raw.githubusercontent.com/tdwg/dwc/master/build/taxon_core_list.csv"

dct_terms <-
	# Fetch the taxon terms from the official Darwin Core repo
  readr::read_csv(dwc_taxon_url) |>
	# Filter to only terms in Taxon class
	dplyr::filter(group == "Taxon") |>
	# Strip out the term from the URL
	dplyr::transmute(term = stringr::str_split(iri, "\\/") |> purrr::map_chr(dplyr::last)) |>
	pull(term)

# Add two attributes: date-time of retrieval, URL where terms originate
attributes(dct_terms) <- list(
	retrieved = Sys.time(),
	url = dwc_taxon_url
)

usethis::use_data(dct_terms, overwrite = TRUE)
