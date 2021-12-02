#' Assert that a taxonomic database is correctly formatted
#'
#' Assertions include:
#' - all synonyms must map to an accepted name
#' - all synonyms and accepted names must be distinct
#' - all names must include a unique taxonID
#'
#' Stops with an error if any check fails.
#'
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format
#'
#' @return Dataframe; taxonomic database in Darwin Core format. Will
#' be the same as the input.
#' @autoglobal
#' @export
dct_assert_tax_dat <- function(tax_dat) {
	# Make sure all synonyms map correctly
	tax_dat_accepted <-
		tax_dat |>
		dplyr::filter(stringr::str_detect(taxonomicStatus, "accepted"))

	tax_dat_synonyms <-
		tax_dat |>
		dplyr::filter(stringr::str_detect(taxonomicStatus, "synonym"))

	tax_dat_synonyms_check <-
		dplyr::anti_join(tax_dat_synonyms, tax_dat_accepted, by = c(acceptedNameUsageID = "taxonID"))

	assertthat::assert_that(
		nrow(tax_dat_synonyms_check) == 0,
		msg = "Some synonyms not accounted for")

	# Make sure all accepted names and synonyms are accounted for
	tax_dat_accepted_check <-
		dplyr::bind_rows(tax_dat_accepted, tax_dat_synonyms) |>
		assertr::assert(assertr::is_uniq, taxonID) |>
		dplyr::anti_join(tax_dat, by = "taxonID")

	assertthat::assert_that(
		nrow(tax_dat_accepted_check) == 0,
		msg = "Some accepted names not accounted for")

	# Make sure accepted names and synonyms are distinct
	tax_dat_no_overlap_check <-
		tax_dat_accepted |>
		dplyr::inner_join(tax_dat_synonyms, by = "scientificName")

	assertthat::assert_that(
		nrow(tax_dat_no_overlap_check) == 0,
		msg = "Some scientific names appear in both accepted names and synonyms")

	tax_dat
}
