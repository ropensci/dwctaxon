#' Drop rows by taxonID
#'
#' Helper function for dct_drop_row
#'
#' @inheritParams dct_drop_row
#'
#' @return Dataframe
#' @noRd
#' @autoglobal
drop_row_by_taxon_id <- function(tax_dat, taxon_id) {
  # - taxonID of tax_dat must be non-missing and unique
  tryCatch(
    dct_check_taxon_id(tax_dat, on_fail = "error", on_success = "logical"),
    error = function(x) {
      stop(
        paste(
          "tax_dat must include column taxonID, which must be a character or",
          "numeric vector with unique, non-missing values"
        ),
        call. = FALSE
      )
    }
  )
  tax_dat[!tax_dat$taxonID %in% taxon_id, ]
}

#' Drop rows by scientificName
#'
#' Helper function for dct_drop_row
#'
#' @inheritParams dct_drop_row
#'
#' @return Dataframe
#' @noRd
#' @autoglobal
drop_row_by_sci_name <- function(tax_dat, sci_name) {
  # - scientificName of tax_dat must be non-missing and unique
  tryCatch(
    dct_check_sci_name(tax_dat, on_fail = "error", on_success = "logical"),
    error = function(x) {
      stop(
        paste(
          "tax_dat must include column scientificName, which must be a",
          "character vector with unique, non-missing values"
        ),
        call. = FALSE
      )
    }
  )
  tax_dat[!tax_dat$scientificName %in% sci_name, ]
}

#' Drop row(s) of a taxonomic database
#'
#' Drop one or more rows from a taxonomic database in Darwin Core (DwC) format
#' by taxonID or scientificName.
#'
#' Only works if values of taxonID or scientificName are unique and non-missing
#' in the taxonomic database (tax_dat).
#'
#' Either taxonID or scientificName should be provided, but not both.
#'
#' @param tax_dat Dataframe; taxonomic database in DwC format.
#' @param taxonID Character or numeric vector; taxonID of the row(s)
#' to be dropped.
#' @param scientificName Character vector; scientificName of the row(s)
#' to be dropped.
#'
#' @return Dataframe; taxonomic database in DwC format
#' @autoglobal
#' @export
#' @example inst/examples/dct_drop_row.R
dct_drop_row <- function(tax_dat,
                         taxonID = NULL, # nolint
                         scientificName = NULL # nolint
) {
  # Check input
  # - tax_dat must be dataframe
  assertthat::assert_that(
    inherits(tax_dat, "data.frame"),
    msg = "tax_dat must be of class data.frame"
  )
  # - must provide taxonID or scientificName
  assertthat::assert_that(
    sum(is.null(taxonID), is.null(scientificName)) == 1,
    msg = "Either taxonID or scientificName must be provided, but not both"
  )
  # Drop rows
  if (!is.null(taxonID)) {
    res <- drop_row_by_taxon_id(tax_dat, taxonID)
  }
  if (!is.null(scientificName)) {
    res <- drop_row_by_sci_name(tax_dat, scientificName)
  }
  return(res)
}
