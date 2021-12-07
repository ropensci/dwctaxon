#' Change the taxonomic status of one entry in a taxonomic database
#'
#' Only one of `taxon_id` or `sci_name` needs to be entered. Either will
#' work if as long as it makes a partial or full match to one row in the data.
#'
#' `usage_id` or `usage_name` should be provided if the new status is a synonym;
#' either will work as it makes a partial or full match to one row in the data.
#'
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format.
#' @param taxon_id Character vector of length 1; taxonID for the entry to be changed.
#' @param sci_name Character vector of length 1; scientificName for the entry to be changed.
#' @param new_status Character vector of length 1; updated taxonomicStatus to use for the entry.
#' @param usage_id Character vector of length 1; taxonID for accepted name if new status is synonym.
#' @param usage_name Character vector of length 1; scientificName for accepted name if new status is synonym.
#' @param clear_usage_id Logical vector of length 1; should `acceptedNameUsageID` be set to `NA`?.
#' Default: TRUE if `new_status` is "accepted" (case insensitive).
#' @param strict Logical vector of length 1; should taxonomic checks be run on the updated
#' taxonomic database?
#'
#' @return Dataframe; taxonomic database in Darwin Core format
#' @autoglobal
#' @export
#' @examples
#' # Swap the accepted / synonym status of Cephalomanes crassum (Copel.) M. G. Price
#' # and Trichomanes crassum Copel.
#' dct_filmies |>
#'   dct_change_status(
#'     sci_name = "Cephalomanes crassum (Copel.) M. G. Price",
#'     new_status = "synonym",
#'     usage_name = "Trichomanes crassum Copel."
#'   ) |>
#'   dct_change_status(
#'     sci_name = "Trichomanes crassum Copel.",
#'     new_status = "accepted"
#'   ) |>
#'   dct_validate()
dct_change_status <- function(
	tax_dat, taxon_id = NULL,
	sci_name = NULL, new_status,
	usage_id = NULL,
	usage_name = NULL,
	clear_usage_id = grepl("accepted", new_status, ignore.case = TRUE),
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

	# Map to acceptedNameUsageID:
	acceptedNameUsageID_matched <- NA
	if (!is.null(usage_name) && is.null(usage_id) && clear_usage_id == FALSE) {
		assertthat::assert_that(
			usage_name %in% tax_dat$scientificName,
			msg = "`usage_name` not detected in `tax_dat$scientificName`")
		acceptedNameUsageID_matched <- tax_dat$taxonID[tax_dat$scientificName == usage_name]
		assertthat::assert_that(
			length(acceptedNameUsageID_matched) == 1,
			msg = "Not exactly one scientific name matches `usage_name`")
	}

	if (!is.null(usage_id) && is.null(usage_name) && clear_usage_id == FALSE) {
		assertthat::assert_that(
			usage_id %in% tax_dat$taxonID,
			msg = "`usage_id` not detected in `tax_dat$taxonID`")
		acceptedNameUsageID_matched <- tax_dat$taxonID[tax_dat$taxonID == usage_id]
		assertthat::assert_that(
			length(acceptedNameUsageID_matched) == 1,
			msg = "Not exactly one taxonID matches `usage_id`")
	}

	assertthat::assert_that(
		sum(!is.null(usage_id), !is.null(usage_name)) != 2,
		msg = "Must use either `usage_id` or `usage_name`, not both")

	new_row <- dplyr::mutate(
		tax_dat_row,
		taxonomicStatus = new_status,
		acceptedNameUsageID = acceptedNameUsageID_matched,
		modified = as.character(Sys.time())
		)

	assertthat::assert_that(
		!isTRUE(all.equal(tax_dat_row$taxonomicStatus, new_row$taxonomicStatus)),
		msg = "Must choose a new taxonomic status"
	)

	# Add any columns that may be missing from original data
	# but are present in row to add (e.g., `modified`)
	missing_cols <- setdiff(colnames(new_row), colnames(tax_dat))
	if (length(missing_cols > 0)) {
		for (i in seq_along(missing_cols)) {
			tax_dat[[missing_cols[[i]]]] <- NA
		}
	}

	# Remove selected row, add back in with modified taxonomic status
	res <- tax_dat |>
		dplyr::slice(-row_hits) |>
		tibble::add_row(new_row, .before = row_hits)

	# Optionally run taxonomic database checks
	if (isTRUE(strict)) res <- dct_validate(res)

	res

}
