#' Check that a taxonomic database is correctly formatted
#'
#' Stops with an error if any check fails. Most checks are geared towards being
#' able to use the taxonomic database for taxonomic name resolution at the
#' species level.
#'
#' For `check_mapping_strict` and `check_status_diff`, "accepted", "synonym",
#' and "variant" are determined by string matching of `taxonomicStatus`; so
#' "provisionally accepted" is counted as "accepted", "ambiguous synonym" is
#' counted as "synonym", etc (case-sensitive).
#'
#' For `check_mapping_strict`, the following rules are enforced:
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
#' @inheritParams dct_check_taxon_id
#' @inheritParams dct_check_tax_status
#' @inherit dct_check_taxon_id return
#' @param check_taxon_id Logical; should all instances of `taxonID` be required
#' to be non-missing and unique?
#' @param check_tax_status Logical; should all taxonomic names be required
#'   to include a valid value for taxonomic status (by default, "accepted",
#'   "synonym", or "variant")?
#' @param check_mapping Logical; should all values of `acceptedNameUsageID` be
#' required to map to the `taxonID` of an existing name?
#' @param check_mapping_strict Logical; should rules about mapping of variants
#'   and synonyms be enforced? (see Details)
#' @param check_sci_name Logical; should all instances of `scientificName` be
#' required to be non-missing and unique?
#' @param check_status_diff Logical; should each scientific name be allowed
#'   to have only one taxonomic status? Default FALSE if `check_sci_name`
#'   is TRUE, since `check_sci_name` makes this check redundant.
#' @param check_col_names Logical; should all column names be required to
#' be a valid Darwin Core term?
#'
#' @autoglobal
#' @export
#' @example inst/examples/dct_validate.R
#'
dct_validate <- function(tax_dat,
                         check_taxon_id = TRUE,
                         check_tax_status = TRUE,
                         check_mapping = TRUE,
                         check_mapping_strict = TRUE,
                         check_sci_name = TRUE,
                         check_status_diff = !check_sci_name,
                         check_col_names = TRUE,
                         valid_tax_status = Sys.getenv(
                           "VALID_TAX_STATUS",
                           unset = "accepted, synonym, variant, NA"
                         ),
                         on_success = "data",
                         on_fail = "error") {
  # Check input format
  # - tax_dat must be a dataframe
  assertthat::assert_that(
    inherits(tax_dat, "data.frame"),
    msg = "'tax_dat' must be of class 'data.frame'"
  )
  # - check_* are all logical flags
  assertthat::assert_that(assertthat::is.flag(check_taxon_id))
  assertthat::assert_that(assertthat::is.flag(check_tax_status))
  assertthat::assert_that(assertthat::is.flag(check_mapping))
  assertthat::assert_that(assertthat::is.flag(check_mapping_strict))
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
  if (check_mapping_strict) {
    assertthat::assert_that(
      check_taxon_id,
      msg = "check_mapping_strict requires check_taxon_id to be TRUE"
    )
    assertthat::assert_that(
      check_mapping,
      msg = "check_mapping_strict requires check_mapping to be TRUE"
    )
    assertthat::assert_that(
      check_tax_status,
      msg = "check_mapping_strict requires check_tax_status to be TRUE"
    )
  }
  if (check_mapping) {
    assertthat::assert_that(
      check_taxon_id,
      msg = "check_mapping requires check_taxon_id to be TRUE"
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
        req_by = "check_mapping",
        run = check_mapping
      ),
      assert_col(
        tax_dat, "taxonID", c("character", "numeric", "integer"),
        req_by = "check_mapping_strict",
        run = check_mapping_strict
      ),
      assert_col(
        tax_dat, "acceptedNameUsageID", c("character", "numeric", "integer"),
        req_by = "check_mapping",
        run = check_mapping
      ),
      assert_col(
        tax_dat, "acceptedNameUsageID", c("character", "numeric", "integer"),
        req_by = "check_mapping_strict",
        run = check_mapping_strict
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
        req_by = "check_mapping_strict",
        run = check_mapping_strict
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
        run = check_mapping
      ),
      # - all names have matching taxonID for acceptedNameUsageID
      check_mapping_exists(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping
      ),
      # Strict mapping ----
      # - taxonomicStatus includes needed values
      check_mapping_strict_status(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_strict,
        valid_tax_status = valid_tax_status
      ),
      # - synonyms map to accepted names
      check_syn_map_to_acc(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_strict
      ),
      # - any row with acceptedNameUsageID must have non-missing taxonomicStatus
      check_acc_id_has_tax_status(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_strict
      ),
      # - any row with acceptedNameUsageID must have valid taxonomicStatus
      check_acc_id_valid_tax_status(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_strict
      ),
      # - variants cannot map to variants
      check_variant_map_to_nonvar(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_strict
      ),
      # - variants must map to something
      check_variant_map_to_something(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_strict
      ),
      # - accepted names can't map to anything
      check_accepted_map_to_nothing(
        tax_dat,
        on_fail = on_fail, on_success = "logical",
        run = check_mapping_strict
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
