#' check_mapping sub-check: check that no names map to self
#'
#' Required columns:
#' - taxonID
#' - acceptedNameUsageID
#'
#' @inherit check_taxon_id_not_na
#' @noRd
check_mapping_to_self <- function(
  tax_dat,
  on_fail = "error",
  on_success = "data",
  run = TRUE) {

  # Early exit with NULL if req'd cols not present
  if (
    is.null(tax_dat$taxonID) ||
    is.null(tax_dat$acceptedNameUsageID) ||
    run == FALSE
  ) {
    return(NULL)
  }

  # Check for names that lack a taxonID for acceptedNameUsageID
  map_to_self <- tax_dat$acceptedNameUsageID == tax_dat$taxonID
  map_id_is_na <- is.na(tax_dat$acceptedNameUsageID)
  map_id_is_bad <- !map_id_is_na & map_to_self

  # Get vectors of bad values
  bad_taxon_id <- tax_dat$taxonID[map_id_is_bad]
  bad_sci_name <- tax_dat$scientificName[map_id_is_bad]
  bad_acc_id <- tax_dat$acceptedNameUsageID[map_id_is_bad]

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(map_id_is_bad) == 0,
      msg = glue::glue(
        "check_mapping failed.
          taxonID detected with identical acceptedNameUsageID.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg('acceptedNameUsageID', bad_acc_id, is_last = TRUE)}",
          .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    assert_that_d(
      sum(map_id_is_bad) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        acceptedNameUsageID = bad_acc_id,
        error = "taxonID detected with identical acceptedNameUsageID",
        check = "check_mapping"
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

#' check_mapping sub-check: check that no acceptedNameUsageID are missing
#' from taxonID.
#'
#' Required columns:
#' - taxonID
#' - acceptedNameUsageID
#'
#' @inherit check_taxon_id_not_na
#' @noRd
check_mapping_exists <- function(
  tax_dat,
  on_fail = "error",
  on_success = "data",
  run = TRUE) {

  # Early exit with NULL if req'd cols not present
  if (
    is.null(tax_dat$taxonID) ||
    is.null(tax_dat$acceptedNameUsageID) ||
    run == FALSE
  ) {
    return(NULL)
  }

  # Check for names that lack a taxonID for acceptedNameUsageID
  map_id_is_good <- tax_dat$acceptedNameUsageID %in% tax_dat$taxonID
  map_id_is_na <- is.na(tax_dat$acceptedNameUsageID)
  map_id_is_bad <- !map_id_is_na & !map_id_is_good

  # Get vectors of bad values
  bad_taxon_id <- tax_dat$taxonID[map_id_is_bad]
  bad_sci_name <- tax_dat$scientificName[map_id_is_bad]
  bad_acc_id <- tax_dat$acceptedNameUsageID[map_id_is_bad]

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(map_id_is_bad) == 0,
      msg = glue::glue(
        "check_mapping failed.
          taxonID detected whose acceptedNameUsageID value does not \\
          map to taxonID of an existing name.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg('acceptedNameUsageID', bad_acc_id, is_last = TRUE)}",
          .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    assert_that_d(
      sum(map_id_is_bad) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        acceptedNameUsageID = bad_acc_id,
        error = paste(
          "taxonID detected whose acceptedNameUsageID value does not",
          "map to taxonID of an existing name."
        ),
        check = "check_mapping"
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

#' Check mapping of usage names
#'
#' Check that names with acceptedUsageID map properly to taxonID in
#' Darwin Core taxonomic data
#'
#' The following rules are enforced:
#' - taxonID may not be identical to acceptedNameUsageID within a single row
#' - Every acceptedNameUsageID must have a corresponding taxonID
#'
#' @inheritParams dct_check_taxon_id
#' @inherit dct_check_taxon_id return
#' @export
#' @examples
#' # The bad data has an acceptedNameUsageID (third row, "4") that lacks a
#' # corresponding taxonID
#' bad_dat <- tibble::tribble(
#'   ~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
#'   "1", NA, "accepted", "Species foo",
#'   "2", "1", "synonym", "Species bar",
#'   "3", "4", "synonym", "Species bat"
#' )
#' suppressWarnings(
#'   dct_check_mapping(bad_dat, on_fail = "summary")
#' )
dct_check_mapping <- function(
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
          tax_dat, "taxonID", c("character", "numeric", "integer"),
          req_by = "check_mapping", on_fail = on_fail
        ),
      assert_col(
          tax_dat, "acceptedNameUsageID", c("character", "numeric", "integer"),
          req_by = "check_mapping", on_fail = on_fail
        ),
      # Check taxonID not NA
      check_taxon_id_not_na(tax_dat, on_fail = on_fail, on_success = "logical"),
      # Check taxonID is unique
      check_taxon_id_is_uniq(
        tax_dat, on_fail = on_fail, on_success = "logical"),
      # Check no names map to self
      check_mapping_to_self(tax_dat, on_fail = on_fail, on_success = "logical"),
      # Check all names have matching taxonID for acceptedNameUsageID
      check_mapping_exists(tax_dat, on_fail = on_fail, on_success = "logical")
    ) |>
    # drop any NULL results
    purrr::compact()
  )

  # Format results
  if (on_fail == "summary") {
    if (any_not_true(check_res)) {
      warning("check_mapping failed")
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