#' Validate a taxonomic database
#'
#' Runs a series of automated checks on a taxonomic database in Darwin Core
#' (DWC) format.
#'
#' For `check_mapping_accepted_status` and `check_status_diff`, "accepted",
#' "synonym", and "variant" are determined by string matching of
#' `taxonomicStatus`; so "provisionally accepted" is counted as "accepted",
#' "ambiguous synonym" is counted as "synonym", etc. (case-sensitive).
#'
#' For `check_mapping_accepted_status`, the following rules are enforced:
#' - Rows with `taxonomicStatus` of "synonym" (synonyms) must have an
#'   `acceptedNameUsageID` matching the `taxonID` of an accepted name (
#'   `taxonomicStatus` of "accepted")
#' - Rows with `taxonomicStatus` of "variant" (orthographic variants) must
#'   have an `acceptedNameUsageID` matching the `taxonID` of an accepted name or
#'   synonym (but not another variant)
#' - Rows with `taxonomicStatus` of "accepted" must not have any value entered
#'   for `acceptedNameUsageID`
#' - Rows with a value for `acceptedNameUsageID` must have a valid value for
#'   `taxonomicStatus`.
#'
#' Default settings of all arguments can be modified with `dct_options()` (see
#' Examples).
#'
#' @param tax_dat `r param_tax_dat`
#' @param check_taxon_id `r param_check_taxon_id`
#' @param check_tax_status `r param_check_tax_status`
#' @param check_mapping_accepted `r param_check_mapping_accepted`
#' @param check_mapping_parent `r param_check_mapping_parent`
#' @param check_mapping_original `r param_check_mapping_original`
#' @param check_mapping_accepted_status `r param_check_mapping_accepted_status`
#' (see Details)
#' @param check_sci_name `r param_check_sci_name`
#' @param check_status_diff `r param_check_status_diff`
#' @param check_col_names `r param_check_col_names`
#' @param valid_tax_status `r param_valid_tax_status`
#' @param on_success `r param_on_success`
#' @param on_fail `r param_on_fail`
#'
#' @inherit dct_check_taxon_id return
#' @autoglobal
#' @example inst/examples/dct_validate.R
#' @export
#'
dct_validate <- function(tax_dat,
                         check_taxon_id,
                         check_tax_status,
                         check_mapping_accepted,
                         check_mapping_parent,
                         check_mapping_original,
                         check_mapping_accepted_status,
                         check_sci_name,
                         check_status_diff,
                         check_col_names,
                         valid_tax_status,
                         on_success,
                         on_fail) {
  # Set defaults ----
  if (missing(check_taxon_id)) {
    check_taxon_id <- get_dct_opt("check_taxon_id")
  }
  if (missing(check_tax_status)) {
    check_tax_status <- get_dct_opt("check_tax_status")
  }
  if (missing(check_mapping_accepted)) {
    check_mapping_accepted <- get_dct_opt("check_mapping_accepted")
  }
  if (missing(check_mapping_parent)) {
    check_mapping_parent <- get_dct_opt("check_mapping_parent")
  }
  if (missing(check_mapping_original)) {
    check_mapping_original <- get_dct_opt("check_mapping_original")
  }
  if (missing(check_mapping_accepted_status)) {
    check_mapping_accepted_status <- get_dct_opt(
      "check_mapping_accepted_status"
    )
  }
  if (missing(check_sci_name)) {
    check_sci_name <- get_dct_opt("check_sci_name")
  }
  if (missing(check_status_diff)) {
    check_status_diff <- get_dct_opt("check_status_diff")
  }
  if (missing(check_col_names)) {
    check_col_names <- get_dct_opt("check_col_names")
  }
  if (missing(valid_tax_status)) {
    valid_tax_status <- get_dct_opt("valid_tax_status")
  }
  if (missing(on_success)) {
    on_success <- get_dct_opt("on_success")
  }
  if (missing(on_fail)) {
    on_fail <- get_dct_opt("on_fail")
  }
  # Check input format
  # - tax_dat must be a dataframe
  assertthat::assert_that(
    inherits(tax_dat, "data.frame"),
    msg = "'tax_dat' must be of class 'data.frame'"
  )
  # - check_* are all logical flags
  assertthat::assert_that(assertthat::is.flag(check_taxon_id))
  assertthat::assert_that(assertthat::is.flag(check_tax_status))
  assertthat::assert_that(assertthat::is.flag(check_mapping_accepted))
  assertthat::assert_that(assertthat::is.flag(check_mapping_parent))
  assertthat::assert_that(assertthat::is.flag(check_mapping_original))
  assertthat::assert_that(assertthat::is.flag(check_mapping_accepted_status))
  assertthat::assert_that(assertthat::is.flag(check_sci_name))
  assertthat::assert_that(assertthat::is.flag(check_status_diff))
  assertthat::assert_that(assertthat::is.flag(check_col_names))
  # - others are strings
  assertthat::assert_that(assertthat::is.string(valid_tax_status))
  assertthat::assert_that(assertthat::is.string(on_success))
  assertthat::assert_that(assertthat::is.string(on_fail))
  assertthat::assert_that(
    on_success %in% c("data", "logical"),
    msg = "on_success must be one of 'data' or 'logical'"
  )
  assertthat::assert_that(
    on_fail %in% c("error", "summary"),
    msg = "on_fail must be one of 'error' or 'summary'"
  )

  # Check pre-requisites
  if (check_mapping_accepted) {
    assertthat::assert_that(
      check_taxon_id,
      msg = "check_mapping_accepted requires check_taxon_id to be TRUE"
    )
  }
  if (check_mapping_parent) {
    assertthat::assert_that(
      check_taxon_id,
      msg = "check_mapping_parent requires check_taxon_id to be TRUE"
    )
  }
  if (check_mapping_original) {
    assertthat::assert_that(
      check_taxon_id,
      msg = "check_mapping_original requires check_taxon_id to be TRUE"
    )
  }
  if (check_mapping_accepted_status) {
    assertthat::assert_that(
      check_taxon_id,
      msg = "check_mapping_accepted_status requires check_taxon_id to be TRUE"
    )
    assertthat::assert_that(
      check_mapping_accepted,
      msg = paste(
        "check_mapping_accepted_status requires check_mapping_accepted to be",
        "TRUE"
      )
    )
    assertthat::assert_that(
      check_tax_status,
      msg = "check_mapping_accepted_status requires check_tax_status to be TRUE"
    )
  }

  # Run main checks ----
  suppressWarnings(
    check_res <- list(
      # Required column checks ----
      assert_col(
        tax_dat, "taxonID", c("character", "numeric", "integer"),
        req_by = "check_taxon_id",
        run = check_taxon_id
      ),
      assert_col(
        tax_dat, "taxonID", c("character", "numeric", "integer"),
        req_by = "check_mapping_accepted_status",
        run = check_mapping_accepted_status
      ),
      assert_col(
        tax_dat, "acceptedNameUsageID", c("character", "numeric", "integer"),
        req_by = "check_mapping_accepted_status",
        run = check_mapping_accepted_status
      ),
      assert_col(
        tax_dat, "taxonomicStatus", "character",
        req_by = "check_tax_status",
        run = check_tax_status
      ),
      assert_col(
        tax_dat, "taxonomicStatus", "character",
        req_by = "check_status_diff",
        run = check_status_diff
      ),
      assert_col(
        tax_dat, "taxonomicStatus", "character",
        req_by = "check_mapping_accepted_status",
        run = check_mapping_accepted_status
      ),
      assert_col(
        tax_dat, "scientificName", "character",
        req_by = "check_status_diff",
        run = check_status_diff
      ),
      assert_col(
        tax_dat, "scientificName", "character",
        req_by = "check_sci_name",
        run = check_sci_name
      ),
      # taxonID ----
      # - taxonID not NA
      check_taxon_id_not_na(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_taxon_id
      ),
      # - each taxonID is unique
      check_taxon_id_is_uniq(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_taxon_id
      ),
      # Taxonomic status ----
      check_tax_status_valid(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        valid_tax_status = valid_tax_status,
        run = check_tax_status
      ),
      # Basic mapping ----
      # - no names map to self
      check_mapping_to_self(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        col_select = "acceptedNameUsageID",
        run = check_mapping_accepted
      ),
      check_mapping_to_self(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        col_select = "parentNameUsageID",
        run = check_mapping_parent
      ),
      check_mapping_to_self(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        col_select = "originalNameUsageID",
        run = check_mapping_original
      ),
      # - all names have matching taxonID for each selected column
      check_mapping_exists(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        col_select = "acceptedNameUsageID",
        run = check_mapping_accepted
      ),
      check_mapping_exists(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        col_select = "parentNameUsageID",
        run = check_mapping_parent
      ),
      check_mapping_exists(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        col_select = "originalNameUsageID",
        run = check_mapping_original
      ),
      # Strict mapping ----
      # - taxonomicStatus includes needed values
      check_mapping_strict_status(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_accepted_status,
        valid_tax_status = valid_tax_status
      ),
      # - synonyms map to accepted names
      check_syn_map_to_acc(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_accepted_status
      ),
      # - any row with acceptedNameUsageID must have non-missing taxonomicStatus
      check_acc_id_has_tax_status(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_accepted_status
      ),
      # - any row with acceptedNameUsageID must have valid taxonomicStatus
      check_acc_id_valid_tax_status(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_accepted_status
      ),
      # - variants cannot map to variants
      check_variant_map_to_nonvar(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_accepted_status
      ),
      # - variants must map to something
      check_variant_map_to_something(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_accepted_status
      ),
      # - accepted names can't map to anything
      check_accepted_map_to_nothing(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_accepted_status
      ),
      # Scientific name ----
      # - scientificName in not NA
      check_sci_name_not_na(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_sci_name
      ),
      # - scientificName is unique
      check_sci_name_is_uniq(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_sci_name
      ),
      # Taxonomic status of sci name -----
      # - Different status for each instance of name
      check_status_diff_p(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_status_diff
      ),
      # Column names ----
      check_col_names_p(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_col_names
      )
    ) |>
      # drop any NULL results
      purrr::compact()
  )

  # Delete any empty dataframes
  empty_df <- sapply(
    check_res, function(y) inherits(y, "data.frame") && nrow(y) == 0
  )
  check_res <- check_res[!empty_df]

  # Format results
  if (on_fail == "summary") {
    if (any_not_true(check_res)) {
      warning("check_mapping failed")
      res <- check_res |>
        bind_rows_f() |>
        sort_cols_dwc() |>
        dplyr::arrange(check, error)
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
