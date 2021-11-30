#' Extract Ferns of the World taxonomic data from Catalog of Life
#'
#' Also do some quality checks, and filter to only data at species level or below
#'
#' @param col_data Catalog of Life data. Output of of load_col()
#'
#' @return Dataframe (tibble)
#' @export
pcg_extract_fow <- function(col_data) {
	# Filter Catalog of Life data to only Ferns of the World
	col_data |>
		dplyr::filter(`dwc:datasetID` == "1140") |>
		dplyr::select(
			taxonID = `dwc:taxonID`,
			acceptedNameUsageID = `dwc:acceptedNameUsageID`,
			taxonomicStatus = `dwc:taxonomicStatus`,
			rank = `dwc:taxonRank`,
			scientificName = `dwc:scientificName`,
			namePublishedIn = `dwc:namePublishedIn`,
			taxonRemarks = `dwc:taxonRemarks`) |>
		# Keep only species level and below
		dplyr::filter(rank %in% c("form", "infraspecific name", "species", "subform", "subspecies", "subvariety", "variety")) |>
		# Filter some names that were incorrectly labeled species level
		dplyr::filter(stringr::str_detect(scientificName, "Polypodiaceae tribe Thelypterideae|Asplenium grex Triblemma|Pteridaceae tribus Platyzomateae|Filicaceae tribus Taenitideae", negate = TRUE)) |>
		dplyr::select(-rank) |>
		# Note: taxonID is unique, but scientificName may not be (esp in case of ambiguous synonyms)
		assertr::assert(assertr::not_na, taxonID, scientificName) |>
		assertr::assert(assertr::is_uniq, taxonID) |>
		# Replace empty strings with NA
		dplyr::mutate(
			dplyr::across(dplyr::everything(), ~dplyr::na_if(., ""))
		) |>
		# Fix scientific names that are both synonyms and accepted names
		dplyr::filter(
			!taxonID %in% c(
				"392HN", # Elaphoglossum rigidum (Aubl.) Urb. -> exclude ambiguous synonym
				"392HM", # Elaphoglossum rigidum (Aubl.) Urb. -> exclude ambiguous synonym
				"3926G", # Elaphoglossum killipii Mickel -> exclude ambiguous synonym (per Tropicos)
				"392K8", # Elaphoglossum serpentinum A. Rojas & W. D. Rodr. -> exclude ambiguous synonym
				"HK2Q", # Asplenium centrifugale Baker -> exclude ambiguous synonym
				"36J4J", # Diplazium wangii Ching -> exclude ambiguous synonym
				"36HP3", # Diplazium lonchophyllum Kunze -> exclude ambiguous synonym
				"3GXD3" # Goniopteris hastata Fée -> exclude ambiguous synonym
			)
		) |>
		# Run taxonomic database checks
		pcg_assert_tax_dat()
}
