#' Check that taxonomicStatus is in range of valid values
#' Assumes that required columns
#' (taxonomicStatus) are present.
#' @param valid_tax_status See dct_check_tax_status()
#' @inherit dct_check_taxon_id
#' @noRd
check_tax_status <- function(
  tax_dat,
  on_fail = "error",
  on_success = "data",
  valid_tax_status = Sys.getenv(
    "VALID_TAX_STATUS",
    unset = "accepted, synonym, variant, NA")
  ) {
  # Early exit with NULL if req'd cols not present
  if (is.null((tax_dat$taxonomicStatus))) {
    return(NULL)
  }

  # Convert valid_tax_status to vector
  valid_tax_status_v <- strsplit(valid_tax_status, ", *")[[1]] |>
    unique()
  valid_tax_status_v[valid_tax_status_v == "NA"] <- NA_character_
  # Check that that taxonomicStatus is in range of valid values
  tax_status_is_bad <- !tax_dat$taxonomicStatus %in% valid_tax_status_v

  # Get vectors of bad values
  bad_taxon_id <- tax_dat$taxonID[tax_status_is_bad]
  bad_sci_name <- tax_dat$scientificName[tax_status_is_bad]
  bad_tax_status <- tax_dat$taxonomicStatus[tax_status_is_bad]

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
      warning("check_tax_status failed")
      return(
        tibble::tibble(
          taxonID = bad_taxon_id,
          scientificName = bad_sci_name,
          taxonomicStatus = bad_tax_status,
          error = as.character(glue::glue(
            "taxonID detected whose taxonomicStatus is not \\
             in valid_tax_status ({valid_tax_status})")),
          check = "check_tax_status"
        )
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
#' @param valid_tax_status Character vector of length 1; valid values for
#'   `taxonomicStatus`. Each value must be separated by a comma. Default
#'   `"accepted, synonym, variant, NA"`. `"NA"` indicates that missing (NA)
#'   values are valid. Case-sensitive. Can also be set with the environmental
#'   variable `"VALID_TAX_STATUS"` (see Examples).
#' @inheritParams dct_check_taxon_id
#' @inherit dct_check_taxon_id return
#' @references <https://dwc.tdwg.org/terms/#dwc:taxonomicStatus>
#' @examples
#' # The bad data has an taxonomicStatus (third row, "foo") that is not
#' # a valid value
#' bad_dat <- tibble::tribble(
#'   ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
#'   "1", NA, "accepted", "Species foo",
#'   "2", "1", "synonym", "Species bar",
#'   "3", NA, "foo", "Species bat"
#' )
#' suppressWarnings(
#'   dct_check_tax_status(bad_dat, on_fail = "summary")
#' )
#' # Example of setting valid values of taxonomicStatus via an environmental
#' # variable
#' Sys.setenv(VALID_TAX_STATUS = "provisionally accepted, synonym, NA")
#' tibble::tribble(
#'    ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
#'    "1", NA, "provisionally accepted", "Species foo",
#'    "2", "1", "synonym", "Species bar",
#'    "3", NA, NA, "Strange name"
#'  ) |>
#' dct_check_tax_status()
#' Sys.unsetenv("VALID_TAX_STATUS")
#'
#' @export
dct_check_tax_status  <- function(
  tax_dat,
  on_fail = "error",
  on_success = "data",
  valid_tax_status = Sys.getenv(
    "VALID_TAX_STATUS",
    unset = "accepted, synonym, variant, NA")
  ) {


  # Run main checks
  suppressWarnings(
    check_res <- list(
      # Check for required columns
      assert_col(
        tax_dat, "taxonomicStatus", "character",
        req_by = "check_tax_status", on_fail = on_fail
      ),
      # Check taxonomic status
      check_tax_status(tax_dat, on_fail = on_fail, on_success = "logical"
    )
    ) |>
    # drop any NULL results
    purrr::compact()
  )

  # Format results
  if (on_fail == "summary") {
    if (any_not_true(check_res)) {
      warning("check_tax_status failed")
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