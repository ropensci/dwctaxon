#' check_sci_name sub-check: check that no scientificName is missing
#' Required columns:
#' - scientificName
#' @inherit dct_check_sci_name
#' @param run Logical; should this check be run? If FALSE, return NULL
#' @noRd
#' @autoglobal
check_sci_name_not_na <- function(tax_dat,
                                  on_fail,
                                  on_success,
                                  run = TRUE,
                                  quiet) {
  # Set defaults ----
  if (missing(on_success)) {
    on_success <- get_dct_opt("on_success")
  }
  if (missing(on_fail)) {
    on_fail <- get_dct_opt("on_fail")
  }
  if (missing(quiet)) {
    quiet <- get_dct_opt("quiet")
  }

  # Early exit with NULL if req'd cols not present
  if (!"scientificName" %in% colnames(tax_dat) || run == FALSE) {
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
      "
      )
    )
  }
  if (on_fail == "summary") {
    err_msg <- "scientificName detected with missing value"
    assert_that_d(
      length(missing_sci_name) == 0,
      data = tibble::tibble(
        scientificName = missing_sci_name,
        check = "check_sci_name",
        error = err_msg
      ),
      msg = err_msg,
      quiet = quiet
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
check_sci_name_is_uniq <- function(tax_dat,
                                   on_fail,
                                   on_success,
                                   run = TRUE,
                                   quiet) {
  # Set defaults ----
  if (missing(on_success)) {
    on_success <- get_dct_opt("on_success")
  }
  if (missing(on_fail)) {
    on_fail <- get_dct_opt("on_fail")
  }
  if (missing(quiet)) {
    quiet <- get_dct_opt("quiet")
  }

  # Early exit with NULL if req'd cols not present
  if (!"scientificName" %in% colnames(tax_dat) || run == FALSE) {
    return(NULL)
  }

  # Check for duplicated ID
  duplicated_sci_name <- tax_dat$scientificName[
    duplicated(tax_dat$scientificName)
  ]

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      length(duplicated_sci_name) == 0,
      msg = glue::glue(
        "check_sci_name failed
         scientificName detected with duplicated value
         {make_msg('scientificName', duplicated_sci_name)}
      "
      )
    )
  }
  if (on_fail == "summary") {
    err_msg <- "scientificName detected with duplicated value"
    assert_that_d(
      length(duplicated_sci_name) == 0,
      data = tibble::tibble(
        scientificName = duplicated_sci_name,
        check = "check_sci_name",
        error = err_msg
      ),
      msg = err_msg,
      quiet = quiet
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
#' @param tax_dat `r param_tax_dat`
#' @param on_fail `r param_on_fail`
#' @param on_success `r param_on_success`
#' @param quiet `r param_quiet`
#'
#' @inherit dct_check_taxon_id return
#' @example inst/examples/dct_check_sci_name.R
#' @autoglobal
#' @export
#'
dct_check_sci_name <- function(tax_dat,
                               on_fail,
                               on_success,
                               quiet) {
  # Set defaults ----
  if (missing(on_success)) {
    on_success <- get_dct_opt("on_success")
  }
  if (missing(on_fail)) {
    on_fail <- get_dct_opt("on_fail")
  }
  if (missing(quiet)) {
    quiet <- get_dct_opt("quiet")
  }

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
  check_res <- list(
    # Check for required columns
    assert_col(
      tax_dat, "scientificName", "character",
      req_by = "check_sci_name", on_fail = on_fail, quiet = quiet
    ),
    # Check taxonID not NA
    check_sci_name_not_na(
      tax_dat,
      on_fail = on_fail, on_success = "logical", quiet = quiet
    ),
    # Check taxonID is unique
    check_sci_name_is_uniq(
      tax_dat,
      on_fail = on_fail, on_success = "logical", quiet = quiet
    )
  ) |>
    # drop any NULL results
    purrr::compact()

  # Format results
  if (on_fail == "summary") {
    if (any_not_true(check_res)) {
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
