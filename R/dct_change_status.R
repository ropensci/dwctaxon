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
dct_change_status_single <- function(
	tax_dat, taxon_id = NULL,
	sci_name = NULL, new_status,
	usage_id = NULL,
	usage_name = NULL,
	clear_usage_id = grepl("accepted", new_status, ignore.case = TRUE),
	strict = FALSE) {

	# Convert any NA input to NULL
	if (!is.null(taxon_id) && is.na(taxon_id)) taxon_id <- NULL
	if (!is.null(sci_name) && is.na(sci_name)) sci_name <- NULL
	if (!is.null(usage_id) && is.na(usage_id)) usage_id <- NULL
	if (!is.null(usage_name) && is.na(usage_name)) usage_name <- NULL
	# Convert missing logicals to FALSE
	if (is.null(strict) || is.na(strict)) strict <- FALSE
	if (is.null(clear_usage_id) || is.na(clear_usage_id)) clear_usage_id <- FALSE

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

	# Make sure update actually changes something
	assertthat::assert_that(
		!isTRUE(all.equal(tax_dat_row$taxonomicStatus, new_row$taxonomicStatus)) ||
			!isTRUE(all.equal(tax_dat_row$acceptedNameUsageID, new_row$acceptedNameUsageID)),
		msg = "Must choose a new taxonomicStatus or acceptedNameUsageID"
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

#' Helper function to get value from a dataframe
#'
#' @param df Dataframe
#' @param col String; name of column to extract value
#' @param i Index of the value to extract in the selected column
#'
#' @return The value at the selected index of the selected column, if
#' that column exists; otherwise `NA`
#'
val_if_in_dat <- function(df, col, i) {
	ifelse(col %in% colnames(df), df[[col]][[i]], NA)
}

#' Change the taxonomic status of one entry in a taxonomic database
#'
#' Only one of `taxon_id` or `sci_name` needs to be entered. Either will
#' work if as long as it makes a partial or full match to one row in the data.
#'
#' `usage_id` or `usage_name` should be provided if the new status is a synonym;
#' either will work as it makes a partial or full match to one row in the data.
#'
#' Can either modify a single row in the input taxonomic database if each
#' argument is supplied as a vector of length 1, or can apply a set of changes
#' to the taxonomic database if the input is supplied as a dataframe.
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
#' @param args_tbl A tibble including columns corresponding to one or more of the
#' above arguments, except for `tax_dat`. In this case, the input taxonomic database
#' will be modified sequentially over each row of input in `args_tbl`.
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
#'
#' # Apply a set of changes
#' library(tibble)
#' updates <- tibble(
#'   sci_name = c("Cephalomanes atrovirens Presl", "Cephalomanes crassum (Copel.) M. G. Price"),
#'   new_status = "synonym",
#'   usage_name = "Trichomanes crassum Copel."
#' )
#' dct_filmies |>
#'   dct_change_status(args_tbl = updates) |>
#'   dct_change_status(sci_name = "Trichomanes crassum Copel.", new_status = "accepted")
dct_change_status <- function(
	tax_dat, taxon_id = NULL,
	sci_name = NULL, new_status,
	usage_id = NULL,
	usage_name = NULL,
	clear_usage_id = grepl("accepted", new_status, ignore.case = TRUE),
	strict = FALSE,
	args_tbl = NULL) {
	# If input is args_tbl, loop over tax_dat, using the previous output of each
	# iteration as input into the next iteration
	if (!is.null(args_tbl)) {
		assertthat::assert_that(
			inherits(args_tbl, "data.frame"),
			msg = "`args_tbl` must be of class data.frame"
		)
		new_dat <- list(tax_dat)
		for (i in 1:nrow(args_tbl)) {
			new_dat[[i + 1]] <- dct_change_status_single(
				new_dat[[i]],
				sci_name = val_if_in_dat(args_tbl, "sci_name", i),
				new_status = val_if_in_dat(args_tbl, "new_status", i),
				usage_id = val_if_in_dat(args_tbl, "usage_id", i),
				usage_name = val_if_in_dat(args_tbl, "usage_name", i),
				clear_usage_id = val_if_in_dat(args_tbl, "clear_usage_id", i),
				strict = val_if_in_dat(args_tbl, "strict", i),
			)
			# Only need contents of most recently modified element.
			# Set previous to NA to save memory
			new_dat[[i]] <- NA
		}
		return(new_dat[[i + 1]])
	}
	# Otherwise, run dct_change_status_single()
	dct_change_status_single(
		tax_dat,
		sci_name = sci_name,
		new_status = new_status,
		usage_id = usage_id,
		usage_name = usage_name,
		clear_usage_id = clear_usage_id,
		strict = strict
	)
}
