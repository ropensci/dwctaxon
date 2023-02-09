#' Check that taxonomicStatus is in range of valid values
#'
#' Required columns:
#' - taxonomicStatus
#'
#' @param valid_tax_status See dct_check_tax_status()
#' @inherit check_taxon_id_not_na
#' @noRd
check_tax_status_valid <- function(tax_dat,
                                   on_fail,
                                   on_success,
                                   valid_tax_status,
                                   run = TRUE,
                                   quiet) {
  # Set defaults ----
  if (missing(valid_tax_status)) {
    valid_tax_status <- get_dct_opt("valid_tax_status")
  }
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
  if (
    !"taxonomicStatus" %in% colnames(tax_dat) ||
      run == FALSE) {
    return(NULL)
  }

  # Convert valid_tax_status to vector
  valid_tax_status_v <- strsplit(valid_tax_status, ", *")[[1]] |>
    unique()
  valid_tax_status_v[valid_tax_status_v == "NA"] <- NA_character_
  # Check that that taxonomicStatus is in range of valid values
  tax_status_is_bad <- !tax_dat$taxonomicStatus %in% valid_tax_status_v

  # Get vectors of bad values
  bad_tax_status <- tax_dat$taxonomicStatus[tax_status_is_bad]
  bad_taxon_id <- NULL
  if ("taxonID" %in% colnames(tax_dat)) {
    bad_taxon_id <- tax_dat$taxonID[tax_status_is_bad]
  }
  bad_sci_name <- NULL
  if ("scientificName" %in% colnames(tax_dat)) {
    bad_sci_name <- tax_dat$scientificName[tax_status_is_bad]
  }

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(tax_status_is_bad) == 0,
      msg = glue::glue(
        "check_tax_status failed.
          taxonID detected whose taxonomicStatus is not \\
          in valid_tax_status ({valid_tax_status})
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg('taxonomicStatus', bad_tax_status, is_last = TRUE)}",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    if (length(tax_status_is_bad) != 0) {
      error_msg <- as.character(glue::glue(
        "taxonID detected whose taxonomicStatus is not \\
             in valid_tax_status ({valid_tax_status})"
      ))
      assert_that_d(
        sum(tax_status_is_bad) == 0,
        data = tibble::tibble(
          taxonID = bad_taxon_id,
          scientificName = bad_sci_name,
          taxonomicStatus = bad_tax_status,
          error = error_msg,
          check = "check_tax_status"
        ),
        msg = error_msg,
        quiet = quiet
      )
    }
  }
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }
}

#' Check that taxonomicStatus is within valid values in
#' Darwin Core taxonomic data
#'
#' @param tax_dat `r param_tax_dat`
#' @param on_fail `r param_on_fail`
#' @param on_success `r param_on_success`
#' @param valid_tax_status `r param_valid_tax_status` (see Examples).
#' @param quiet `r param_quiet`
#'
#' @inherit dct_check_taxon_id return
#' @references <https://dwc.tdwg.org/terms/#dwc:taxonomicStatus>
#' @example inst/examples/dct_check_tax_status.R
#' @autoglobal
#' @export
#'
dct_check_tax_status <- function(tax_dat,
                                 on_fail,
                                 on_success,
                                 valid_tax_status,
                                 quiet) {
  # Set defaults ----
  if (missing(valid_tax_status)) {
    valid_tax_status <- get_dct_opt("valid_tax_status")
  }
  if (missing(on_success)) {
    on_success <- get_dct_opt("on_success")
  }
  if (missing(on_fail)) {
    on_fail <- get_dct_opt("on_fail")
  }
  if (missing(quiet)) {
    quiet <- get_dct_opt("quiet")
  }
  # Run main checks
  assertthat::assert_that(assertthat::is.string(valid_tax_status))

  check_res <- list(
    # Check for required columns
    assert_col(
      tax_dat, "taxonomicStatus", "character",
      req_by = "check_tax_status", on_fail = on_fail, quiet = quiet
    ),
    # Check taxonomic status
    check_tax_status_valid(
      tax_dat,
      on_fail = on_fail, on_success = "logical",
      valid_tax_status = valid_tax_status,
      quiet = quiet
    )
  ) |>
    # drop any NULL results
    purrr::compact()

  # Format results
  if (on_fail == "summary") {
    if (any_not_true(check_res)) {
      res <- check_res |>
        bind_rows_f() |>
        sort_cols_dwc()
      return(res)
    }
  }
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }
}
