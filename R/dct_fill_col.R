#' Fill a column of a taxonomic database
#'
#' Fill a column in a taxonomic database in Darwin Core (DWC) format.
#'
#' Several terms (columns) in DWC format come in pairs of "term" and "termID";
#' for example, "acceptedNameUsage" and "acceptedNameUsageID", where the first
#' is the value in a human-readable form (in this case, scientific name of the
#' accepted taxon) and the second is the value used by a machine (in this case,
#' taxonID of the accepted taxon). Other pairs include "parentNameUsage" and
#' "parentNameUsageID", "scientificName" and "scientificNameID", etc. None are
#' required to be used in a given DWC dataset.
#'
#' Often when updating data, the user may only fill in one value or the other
#' (e.g., "acceptedNameUsage" or "acceptedNameUsageID"), but not both. The
#' purpose of `dct_fill_col()` is to fill the missing column.
#'
#' `match_from` and `match_to` are used to locate the values used for filling
#' each cell. The values in the `match_to` column must be unique.
#'
#' The default settings are to fill acceptedNameUsage with values from
#' scientificName by matching acceptedNameUsageID to taxonID (see Example).
#'
#' @param tax_dat `r param_tax_dat`
#' @param fill_to Character vector of length 1; name of column to fill.
#' @param fill_from Character vector of length 1; name of column to copy
#' values from when filling.
#' @param match_to Character vector of length 1; name of column to match to.
#' @param match_from Character vector of length 1; name of column to match from.
#' @return `r param_tax_dat`
#' @example inst/examples/dct_fill_col.R
#' @autoglobal
#' @export
dct_fill_col <- function(tax_dat,
                         fill_to = "acceptedNameUsage",
                         fill_from = "scientificName",
                         match_to = "taxonID",
                         match_from = "acceptedNameUsageID") {
  # input format checks ----
  assertthat::assert_that(
    inherits(tax_dat, "data.frame"),
    msg = "tax_dat must be of class 'data.frame'"
  )
  assertthat::assert_that(assertthat::is.string(fill_to))
  assertthat::assert_that(assertthat::is.string(fill_from))
  assertthat::assert_that(assertthat::is.string(match_to))
  assertthat::assert_that(assertthat::is.string(match_from))

  # other input checks ----
  assertthat::assert_that(
    is_unique(tax_dat[[match_to]]),
    msg = glue::glue("match_to column ({match_to}) must have unique values")
  )
  assertthat::assert_that(
    fill_to %in% dct_terms$term,
    msg = "fill_to must be a valid DWC term; see `dct_terms`"
  )
  assertthat::assert_that(
    fill_from %in% dct_terms$term,
    msg = "fill_from must be a valid DWC term; see `dct_terms`"
  )
  assertthat::assert_that(
    fill_from %in% colnames(tax_dat),
    msg = "fill_from must be an existing column in tax_dat"
  )
  assertthat::assert_that(
    match_to %in% dct_terms$term,
    msg = "match_to must be a valid DWC term; see `dct_terms`"
  )
  assertthat::assert_that(
    match_to %in% colnames(tax_dat),
    msg = "match_to must be an existing column in tax_dat"
  )
  assertthat::assert_that(
    match_from %in% dct_terms$term,
    msg = "match_from must be a valid DWC term; see `dct_terms`"
  )
  assertthat::assert_that(
    match_from %in% colnames(tax_dat),
    msg = "match_from must be an existing column in tax_dat"
  )

  # Add fill_to as empty col if it does not yet exist
  if (!fill_to %in% colnames(tax_dat)) {
    tax_dat[[fill_to]] <- rep(NA, nrow(tax_dat))
  }

  # Lookup location of values to copy
  loc <- match(
    tax_dat[[match_from]],
    tax_dat[[match_to]]
  )

  # Copy values
  tax_dat[[fill_to]] <- dplyr::coalesce(
    tax_dat[[fill_to]],
    tax_dat[[fill_from]][loc]
  )

  return(tax_dat)
}
