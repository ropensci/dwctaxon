#' check_sci_name sub-check: check that no scientificName is missing
#' Required columns:
#' - scientificName
#' @inherit dct_check_sci_name
#' @param run Logical; should this check be run? If FALSE, return NULL
#' @noRd
#' @autoglobal
check_sci_name_not_na <- function(
  tax_dat,
  on_fail,
  on_success,
  run = TRUE
) {

  # Early exit with NULL if req'd cols not present
  if (is.null(tax_dat$scientificName) || run == FALSE) {
    return(NULL)
  }

  # Check for missing ID
  missing_sci_name <- tax_dat$scientificName[is.na(tax_dat$scientificName)]

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      length(missing_sci_name) == 0,
      msg = glue::glue(
        "check_sci_name failed
         scientificName detected with missing value
         {make_msg('scientificName', missing_sci_name)}
      ")
    )
  }
  if (on_fail == "summary") {
    assert_that_d(
      length(missing_sci_name) == 0,
      data = tibble::tibble(
        scientificName = missing_sci_name,
        check = "check_sci_name",
        error = "scientificName detected with missing value"
      )
    )
  }
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }
}

#' check_sci_name sub-check: check that all scientificName values are unique
#' Required columns:
#' - scientificName
#' @inherit check_sci_name_not_na
#' @noRd
#' @autoglobal
check_sci_name_is_uniq <- function(
  tax_dat,
  on_fail,
  on_success,
  run = TRUE
) {

  # Early exit with NULL if req'd cols not present
  if (is.null(tax_dat$scientificName) || run == FALSE) {
    return(NULL)
  }

  # Check for duplicated ID
  duplicated_sci_name <- tax_dat$scientificName[duplicated(tax_dat$scientificName)]

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      length(duplicated_sci_name) == 0,
      msg = glue::glue(
        "check_sci_name failed
         scientificName detected with duplicated value
         {make_msg('scientificName', duplicated_sci_name)}
      ")
    )
  }
  if (on_fail == "summary") {
    assert_that_d(
      length(duplicated_sci_name) == 0,
      data = tibble::tibble(
        scientificName = duplicated_sci_name,
        check = "check_sci_name",
        error = "scientificName detected with duplicated value"
      )
    )
  }
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }
}

#' Check scientificName
#'
#' Check for correctly formatted scientificName column in Darwin Core
#' taxonomic data.
#'
#' The following rules are enforced:
#' - scientificName may not be missing (NA)
#' - scientificName must be unique
#'
#' @inheritParams dct_check_taxon_id
#' @inherit dct_check_taxon_id return
#'
#' @examples
#' suppressWarnings(
#'   dct_check_sci_name(
#'     data.frame(scientificName = NA_character_),
#'     on_fail = "summary")
#' )
#' dct_check_sci_name(data.frame(scientificName = "a"))
#' @autoglobal
#' @export
#'
dct_check_sci_name <- function(
  tax_dat,
  on_fail = "error",
  on_success = "data") {

  # Check input format
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

  # Run main checks
  suppressWarnings(
    check_res <- list(
      # Check for required columns
      assert_col(
        tax_dat, "scientificName", "character",
        req_by = "check_sci_name", on_fail = on_fail
      ),
      # Check taxonID not NA
      check_sci_name_not_na(tax_dat, on_fail = on_fail, on_success = "logical"),
      # Check taxonID is unique
      check_sci_name_is_uniq(tax_dat, on_fail = on_fail, on_success = "logical")
    ) |>
    # drop any NULL results
    purrr::compact()
  )

  # Format results
  if (on_fail == "summary") {
    if (any_not_true(check_res)) {
      warning("check_sci_name failed")
      return(bind_rows_f(check_res))
    }
  }
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }

}