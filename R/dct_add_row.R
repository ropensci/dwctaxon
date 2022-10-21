#' Add one entry to a taxonomic database
#'
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format
#' @param taxon_id taxonID to use for new entry; optional, will be
#' assigned automatically if not provided.
#' @param sci_name scientificName to use for new entry
#' @param strict Logical; should taxonomic checks be run on the updated
#' taxonomic database?
#' @param ... Additional data to include, specified as sets of named
#' character strings; e.g., `parentNameUsageID = "6SH4"`. The name of
#' each string must be a valid column name for data in Darwin Core format.
#'
#' @return Dataframe; taxonomic database in Darwin Core format
#' @examples
#' tibble::tibble(
#'   taxonID = "123",
#'   scientificName = "Foogenus barspecies",
#'   acceptedNameUsageID = NA_character_,
#'   taxonomicStatus = "accepted"
#' ) |>
#'   dct_add_row(
#'     sci_name = "Foogenus barspecies var. bla",
#'     parentNameUsageID = "123",
#'     nameAccordingTo = "me",
#'     strict = TRUE
#'   )
#' @autoglobal
#' @export
dct_add_row <- function(tax_dat, taxon_id = NULL, sci_name, strict = FALSE, ...) {

  # Check input
  assertthat::assert_that(assertthat::is.string(sci_name))
  if (!is.null(taxon_id)) assertthat::assert_that(assertthat::is.string(taxon_id)) # nolint
  assertthat::assert_that(assertthat::is.flag(strict))

  # Assign default taxon_id as hash of sci name
  if (is.null(taxon_id)) taxon_id <- digest::digest(sci_name)

  # Create new row to add
  new_row <- tibble::tibble(
    taxonID = taxon_id,
    scientificName = sci_name,
    modified = as.character(Sys.time())
  )

  # Add other data
  other_data <- tibble::tibble(...)

  if (nrow(other_data) > 0) {
    # Run checks on other data
    dct_validate(
      other_data,
      check_taxon_id = FALSE,
      check_mapping = FALSE,
      strict_mapping = FALSE,
      check_taxonomic_status = FALSE,
      check_acc_syn_diff = FALSE,
      check_col_names = TRUE
    )

    assertthat::assert_that(
      nrow(other_data) == 1,
      msg = "Only one row of additional data may be added"
    )

    assertthat::assert_that(
      !any(colnames(other_data) %in% c(
        "taxonID", "scientificName"
      )),
      msg = "Names of additional data may not include 'taxonID' or 'scientificName'"
    )

    new_row <- dplyr::bind_cols(new_row, other_data)
  }

  # Check for duplicated ID
  overlap_id <- dplyr::inner_join(tax_dat, new_row, by = "taxonID")
  assertthat::assert_that(nrow(overlap_id) == 0,
    msg = "`tax_dat` already contains that `taxonID`"
  )

  # Add new row
  res <- dplyr::bind_rows(tax_dat, new_row)

  # Optionally run taxonomic database checks
  if (isTRUE(strict)) dct_validate(res)

  res
}
