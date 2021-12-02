
#' Change the taxonomic status of one entry in a taxonomic database
#'
#' Only one of `taxon_id` or `sci_name` needs to be entered. Either will
#' work if as long as it makes a partial or full match to one row in the data.
#'
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format
#' @param taxon_id taxonID for the entry to be changed
#' @param sci_name scientificName for the entry to be changed
#' @param new_status Updated taxonomicStatus to use for the entry
#' @param strict Logical; should taxonomic checks be run on the updated
#' taxonomic database?
#'
#' @return Dataframe; taxonomic database in Darwin Core format
#' @autoglobal
#' @export
dct_change_status <- function(
	tax_dat, taxon_id = NULL,
	sci_name = NULL, new_status,
	strict = FALSE) {

	assertthat::assert_that(assertthat::is.string(new_status))
	assertthat::assert_that(
		sum(is.null(taxon_id), is.null(sci_name)) == 1,
		msg = "Only one of `sci_name` or `taxon_id` should be NULL, not both"
	)

	# Isolate row to change
	if (!is.null(sci_name) && is.null(taxon_id)) {
		row_hits <- stringr::str_detect(
			tax_dat$scientificName, stringr::fixed(sci_name)) |> which()
		if (length(row_hits) == 1) {
			tax_dat_row <- dplyr::slice(tax_dat, row_hits)
		}
	}

	if (!is.null(taxon_id) && is.null(sci_name)) {
		row_hits <- stringr::str_detect(
			tax_dat$taxonID, stringr::fixed(taxon_id)) |> which()
		if (length(row_hits) == 1) {
			tax_dat_row <- dplyr::slice(tax_dat, row_hits)
		}
	}

	assertthat::assert_that(length(row_hits) == 1,
													msg = "Not exactly one row selected")

	new_row <- dplyr::mutate(
		tax_dat_row,
		taxonomicStatus = new_status,
		taxonRemarks = paste3(taxonRemarks, glue::glue("taxonomicStatus updated {Sys.time()}"))
		)

	assertthat::assert_that(
		!isTRUE(all.equal(tax_dat_row, new_row)),
		msg = "Must choose a new taxonomic status"
	)

	# Remove selected row, add back in with modified taxonomic status
	res <- tax_dat |>
		dplyr::slice(-row_hits) |>
		tibble::add_row(new_row, .before = row_hits)

	# Optionally run taxonomic database checks
	if (isTRUE(strict)) res <- dct_validate_tax_dat(res)

	res

}
