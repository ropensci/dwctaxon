#' Check that a taxonomic database is correctly formatted
#'
#' Stops with an error if any check fails. Most checks are geared towards being
#' able to use the taxonomic database for taxonomic name resolution at the
#' species level.
#'
#' For `check_taxonomic_status` and `check_acc_syn_diff`, "accepted" and
#' "synonym" are determined by string matching of `taxonomicStatus`; so
#' "provisionally accepted" is counted as "accepted", "ambiguous synonym" is
#' counted as "synonym", etc (not case-sensitive).
#'
#' For `check_mapping`, the `acceptedNameUsageID` must map to the `taxonID` of
#' an existing name which itself does not have an `acceptedNameUsageID` (i.e.,
#' no multiple levels of mapping allowed).
#'
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format
#' @param check_taxon_id Logical; should all instances of `taxonID` be required
#' to be non-missing and unique?
#' @param check_mapping Logical; should all values of `acceptedNameUsageID` be
#' required to map to the `taxonID` of an existing name?
#' @param check_taxonomic_status Logical; should all taxonomic names be required
#' to include the status of either "accepted" or "synonym"?
#' @param check_acc_syn_diff Logical; should accepted names and synonyms be
#' required to be different?
#' @param check_col_names Logical; should all column names be required to
#' be a valid Darwin Core term?
#'
#' @return Dataframe; taxonomic database in Darwin Core format. Will
#' be the same as the input.
#' @autoglobal
#' @export
#' @examples
#' dct_validate(dct_filmies)
#'
dct_validate <- function(
	tax_dat,
	check_taxon_id = TRUE,
	check_mapping = TRUE,
	check_taxonomic_status = TRUE,
	check_acc_syn_diff = TRUE,
	check_col_names = TRUE) {

	# Check for unique, non-missing taxon ID
	if (isTRUE(check_taxon_id)) {
		assertr::assert(tax_dat, assertr::is_uniq, taxonID, success_fun = assertr::success_logical)
		assertr::assert(tax_dat, assertr::not_na, taxonID, success_fun = assertr::success_logical)
	}

	# Check for name mapping
	if (isTRUE(check_mapping)) {

		assertthat::assert_that(
			isTRUE(check_taxon_id),
			msg = "`check_mapping` requires `check_taxon_id` to be TRUE")

		if ("acceptedNameUsageID" %in% colnames(tax_dat)) {
			# Split dataset into "target" (no acceptedNameUsageID) and "query"
			# (acceptedNameUsageID is present)
			tax_dat_target <-
				tax_dat |>
				dplyr::filter(is.na(acceptedNameUsageID) | acceptedNameUsageID == "")

			tax_dat_query <-
				dplyr::anti_join(tax_dat, tax_dat_target, by = "taxonID")

			# All names should map
			tax_dat_mapping_check <-
				dplyr::anti_join(tax_dat_query, tax_dat_target, by = c(acceptedNameUsageID = "taxonID"))

			assertthat::assert_that(
				nrow(tax_dat_mapping_check) == 0,
				msg = "`check_mapping` failed. At least one `acceptedNameUsageID` value does not map to `taxonID` of an existing name")
		}
	}

	# Check that all names have either accepted or synonym
	if (isTRUE(check_taxonomic_status)) {

		assertthat::assert_that(
			isTRUE(check_taxon_id),
			msg = "`check_taxonomic_status` requires `check_taxon_id` to be TRUE")

		if ("taxonomicStatus" %in% colnames(tax_dat)) {
			# Separate accepted names and synonyms
			tax_dat_accepted <-
				tax_dat |>
				dplyr::filter(stringr::str_detect(taxonomicStatus, stringr::fixed("accepted", ignore_case = TRUE)))

			tax_dat_synonyms <-
				tax_dat |>
				dplyr::filter(stringr::str_detect(taxonomicStatus, stringr::fixed("synonym", ignore_case = TRUE)))

			# Make sure all accepted names and synonyms are accounted for
			tax_dat_accepted_check <-
				dplyr::bind_rows(tax_dat_accepted, tax_dat_synonyms) |>
				dplyr::anti_join(tax_dat, by = "taxonID")

			assertthat::assert_that(
				nrow(tax_dat_accepted_check) == 0,
				msg = "`check_taxonomic_status` failed. At least one name does not have the `taxonomicStatus` as either an accepted name or a synonym")
		}
	}

	# Check that accepted names and synonyms are distinct
	if (isTRUE(check_acc_syn_diff)) {
		if ("taxonomicStatus" %in% colnames(tax_dat)) {
		assertthat::assert_that(
			isTRUE(check_taxonomic_status),
			msg = "`check_acc_syn_diff` requires `check_taxonomic_status` to be TRUE")

		tax_dat_no_overlap_check <-
			tax_dat_accepted |>
			dplyr::inner_join(tax_dat_synonyms, by = "scientificName")

		assertthat::assert_that(
			nrow(tax_dat_no_overlap_check) == 0,
			msg = "`check_acc_syn_diff` failed. Some scientific names appear in both accepted names and synonyms")
		}
	}

	# Check that column names are valid
	if (isTRUE(check_col_names)) {
		bad_col_names <- setdiff(colnames(tax_dat), dct_terms$term)
		assertthat::assert_that(
			length(bad_col_names) == 0,
			msg = glue::glue("`check_col_names` failed. Invalid column names present: {paste(bad_col_names, collapse = ', ')}. See dct_terms for valid column names."))
	}

	tax_dat
}
