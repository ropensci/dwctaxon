#' Add one entry to a taxonomic database
#'
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format
#' @param taxon_id taxonID to use for new entry; optional, will be
#' assigned automatically if not provided.
#' @param sci_name scientificName to use for new entry
#' @param tax_status taxonomicStatus to use for new entry
#' @param usage_id acceptedNameUsageID to use for new entry; optional
#' @param strict Logical; should taxonomic checks be run on the updated
#' taxonomic database?
#'
#' @return Dataframe; taxonomic database in Darwin Core format
#' @autoglobal
#' @export
dct_add_row <- function(
	tax_dat, taxon_id = NULL,
	sci_name, tax_status,
	usage_id = NULL,
	strict = FALSE
) {

	# Check input
	assertthat::assert_that(assertthat::is.string(sci_name))
	assertthat::assert_that(assertthat::is.string(tax_status))
	# Assign default taxon_id as hash of sci name
	if (is.null(taxon_id)) taxon_id <- digest::digest(sci_name)
	if (is.null(usage_id)) usage_id <- NA_character_
	# Create new row to add
	new_row <- tibble::tibble(
		taxonID = taxon_id,
		acceptedNameUsageID = usage_id,
		taxonomicStatus = tax_status,
		scientificName = sci_name,
		modified = as.character(Sys.time())
	)
	# Check for duplicated ID
	overlap_id <- dplyr::inner_join(tax_dat, new_row, by = "taxonID")
	assertthat::assert_that(nrow(overlap_id) == 0,
													msg = "`tax_dat` already contains that `taxonID`"
	)
	# Check for duplicated data
	overlap_rows <- dplyr::inner_join(tax_dat, new_row, by = c(
		"taxonID", "acceptedNameUsageID", "taxonomicStatus", "scientificName"
	))
	assertthat::assert_that(nrow(overlap_rows) == 0,
													msg = "`tax_dat` already contains data to add"
	)
	# Check that usage ID exists, if applicable
	if (!is.na(usage_id)) {
		overlap_usage_id <- dplyr::inner_join(
			tax_dat, new_row, by = c(taxonID = "acceptedNameUsageID"))
		assertthat::assert_that(
			nrow(overlap_usage_id) == 1,
			msg = "`usage_id` does not match exactly one existing `taxonID`"
		)
	}
	# Add new row
	res <- tax_dat |>
		tibble::add_row(new_row)

	# Optionally run taxonomic database checks
	if (isTRUE(strict)) res <- dct_validate(res)

	res
}
