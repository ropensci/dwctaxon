#' check_mapping sub-check: check that no names map to self
#' Assumes that dct_check_taxon_id has already passed and that required columns
#' (acceptedNameUsageID, taxonID, scientificName) are present.
#' @inherit dct_check_taxon_id
#' @noRd
check_mapping_to_self <- function(
  tax_dat,
  on_fail = "error",
  on_success = "data") {

  # Check for names that lack a taxonID for acceptedNameUsageID
  map_to_self <- tax_dat$acceptedNameUsageID == tax_dat$taxonID
  map_id_is_na <- is.na(tax_dat$acceptedNameUsageID)
  map_id_is_bad <- !map_id_is_na & map_to_self

  # In case of failure
  if (on_fail == "error") {
    # either TRUE or early exit with failure
    assertthat::assert_that(
      sum(map_id_is_bad) == 0,
      msg = glue::glue(
        "check_mapping failed.
          taxonID detected with identical acceptedNameUsageID.
          Bad taxonID: \\
          {paste(tax_dat$taxonID[map_id_is_bad], collapse = ', ')}
          Bad scientificName: \\
          {paste(tax_dat$scientificName[map_id_is_bad], collapse = ', ')}"
      )
    )
  }
  if (on_fail == "summary") {
    #  either skip or early exit with data
    if (length(map_id_is_bad) != 0) {
      return(
        tibble::tibble(
          taxonID = tax_dat$taxonID[map_id_is_bad],
          scientificName = tax_dat$scientificName[map_id_is_bad],
          error = "taxonID detected with identical acceptedNameUsageID",
          check = "check_mapping"
        )
      )
    }
  }
  # In case of success
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }

}

#' check_mapping sub-check: check that no acceptedNameUsageID are missing
#' from taxonID.
#' Assumes that dct_check_taxon_id has already passed and that required columns
#' (acceptedNameUsageID, taxonID, scientificName) are present.
#' @inherit dct_check_taxon_id
#' @noRd
check_mapping <- function(
  tax_dat,
  on_fail = "error",
  on_success = "data") {

  # Check for names that lack a taxonID for acceptedNameUsageID
  map_id_is_good <- tax_dat$acceptedNameUsageID %in% tax_dat$taxonID
  map_id_is_na <- is.na(tax_dat$acceptedNameUsageID)
  map_id_is_bad <- !map_id_is_na & !map_id_is_good

  # In case of failure
  if (on_fail == "error") {
    # either TRUE or early exit with failure
    assertthat::assert_that(
      sum(map_id_is_bad) == 0,
      msg = glue::glue(
        "check_mapping failed.
          taxonID detected whose acceptedNameUsageID value does not \\
          map to taxonID of an existing name.
          Bad taxonID: \\
          {paste(tax_dat$taxonID[map_id_is_bad], collapse = ', ')}
          Bad scientificName: \\
          {paste(tax_dat$scientificName[map_id_is_bad], collapse = ', ')}"
      )
    )
  }
  if (on_fail == "summary") {
    #  either skip or early exit with data
    if (length(map_id_is_bad) != 0) {
      return(
        tibble::tibble(
          taxonID = tax_dat$taxonID[map_id_is_bad],
          scientificName = tax_dat$scientificName[map_id_is_bad],
          error = paste(
            "taxonID detected whose acceptedNameUsageID value does not",
            "map to taxonID of an existing name."
          ),
          check = "check_mapping"
        )
      )
    }
  }
  # In case of success
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

  # Check for required columns
  acc_name_id_exists <- assert_col(
    tax_dat, "acceptedNameUsageID", c("character", "numeric", "integer"),
    req_by = "check_mapping", on_fail = on_fail
  )
  sci_name_exists <- assert_col(
    tax_dat, "scientificName", "character",
    req_by = "check_mapping", on_fail = on_fail
  )
  # Early exit if req cols not present
  if (on_fail == "summary") {
    if (any(!isTRUE(acc_name_id_exists), !isTRUE(sci_name_exists))) {
      return(bind_rows_f(acc_name_id_exists, sci_name_exists))
    }
  }

  # Check taxon ID
  suppressWarnings({
    check_taxon_id_res <- dct_check_taxon_id(
      tax_dat, on_fail = on_fail, on_success = "logical"
    )
    # Check for mapping to self
    check_mapping_to_self_res <- check_mapping_to_self(
      tax_dat, on_fail = on_fail, on_success = "logical"
    )
    # Check for matching taxonID in acceptedNameUsageID
    check_mapping_res <- check_mapping(
      tax_dat, on_fail = on_fail, on_success = "logical"
    )
  })

  # In case of failure
  # (errors will exit early anyways)
  if (on_fail == "summary") {
    if (
      any(
        !isTRUE(check_mapping_to_self_res),
        !isTRUE(check_mapping_res),
        !isTRUE(check_taxon_id_res)
      )
    ) {
      warning("check_mapping failed")
      return(
        bind_rows_f(
          check_taxon_id_res,
          check_mapping_to_self_res,
          check_mapping_res)
        )
    }
  }
  # In case of success
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }

}
