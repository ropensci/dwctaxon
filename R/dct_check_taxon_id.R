#' Check taxonID
#'
#' Check for correctly formatted taxonID column in Darwin Core taxonomic data.
#'
#' The following rules are enforced:
#' - taxonID may not be missing (NA)
#' - taxonID must be unique
#'
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format.
#' @param on_fail Character vector of length 1, either "error" or "summary".
#'  Describes what to do if the check fails. Default "error".
#' @param on_success Character vector of length 1, either  "logical" or "data".
#'  Describes what to do if the check passes. Default "data".
#'
#' @return Depends on the result of the check and on values of `on_fail` and
#' `on_success`:
#' - If the check passes and `on_success` is "logical", return `TRUE`
#' - If the check passes and `on_success` is "data", return the input dataframe
#' - If the check fails and `on_fail` is "error", return an error
#' - If the check fails and `on_fail` is "summary", issue a warning and
#'   return a dataframe with a summary of the reasons for failure
#'
#' @examples
#' suppressWarnings(
#'   dct_check_taxon_id(
#'     data.frame(taxonID = NA_character_),
#'     on_fail = "summary")
#' )
#' dct_check_taxon_id(data.frame(taxonID = 1))
#'
dct_check_taxon_id <- function(
  tax_dat,
  on_fail = "error",
  on_success = "data") {
  # Run checks on input
  assertthat::assert_that(
    inherits(tax_dat, "data.frame"),
    msg = "'tax_dat' must be of class 'data.frame'"
  )
  assertthat::assert_that(assertthat::is.string(on_fail))
  assertthat::assert_that(assertthat::is.string(on_success))
  assertthat::assert_that(
    on_fail %in% c("error", "summary"),
    msg = "on_fail must be one of 'error' or 'summary'"
  )
  assertthat::assert_that(
    on_success %in% c("data", "logical"),
    msg = "on_success must be one of 'data' or 'logical'"
  )
  # First check for required column
  req_col_check <- assert_col(
    tax_dat, "taxonID", c("character", "numeric", "integer"),
    req_by = "check_taxon_id", on_fail = on_fail
  )
  if (!isTRUE(req_col_check) && on_fail == "summary") {
    return(req_col_check)
  }
  missing_tax_id <- tax_dat$taxonID[is.na(tax_dat$taxonID)]
  dup_tax_id <- tax_dat$taxonID[duplicated(tax_dat$taxonID)]
  if (on_fail == "error") {
    assertthat::assert_that(
      length(missing_tax_id) == 0,
      msg = glue::glue(
        "check_taxon_id failed
         taxonID detected with missing value
         Bad taxonID: {missing_tax_id}
      ")
    )
    assertthat::assert_that(
      length(dup_tax_id) == 0,
      msg = glue::glue(
        "check_taxon_id failed
         taxonID detected with duplicated value
         Bad taxonID: {dup_tax_id}
      ")
    )
  }
  if (on_fail == "summary") {
    if (length(missing_tax_id) != 0 || length(dup_tax_id) != 0) {
      missing_tax_id_summary <- tibble::tibble(
        taxonID = missing_tax_id,
        check = "check_taxon_id",
        error = "taxonID detected with missing value"
      )
     dup_tax_id_summary <- tibble::tibble(
        taxonID = dup_tax_id,
        check = "check_taxon_id",
        error = "taxonID detected with duplicated value"
     )
     error_summary <- rbind(missing_tax_id_summary, dup_tax_id_summary)
     warning("check_taxon_id failed")
     return(error_summary)
    }
  }
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }
}