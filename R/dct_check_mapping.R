#' check_mapping sub-check: check that no names map to self
#'
#' Names with taxonomicStatus of "accepted" are allowed to to have equal
#' taxonID and acceptedNameUsageID
#'
#' Required columns:
#' - taxonID
#' - select column
#'
#' @inherit check_taxon_id_not_na
#' @autoglobal
#' @importFrom rlang :=
#' @noRd
check_mapping_to_self <- function(tax_dat,
                                  on_fail = dct_options()$on_fail,
                                  on_success = dct_options()$on_success,
                                  col_select = "acceptedNameUsageID",
                                  run = TRUE,
                                  quiet = dct_options()$quiet) {
  # Early exit with NULL if req'd cols not present
  if (
    !"taxonID" %in% colnames(tax_dat) ||
      !"scientificName" %in% colnames(tax_dat) ||
      (!"taxonomicStatus" %in% colnames(tax_dat) &&
        col_select == "acceptedNameUsageID") ||
      !col_select %in% colnames(tax_dat) ||
      run == FALSE
  ) {
    return(NULL)
  }

  # Check for names that lack a taxonID for acceptedNameUsageID
  map_to_self <- tax_dat[[col_select]] == tax_dat$taxonID
  map_id_is_na <- is.na(tax_dat[[col_select]])
  map_id_is_bad <- !map_id_is_na & map_to_self

  # For acceptedNameUsageID, allow self-mapping for accepted names
  if (col_select == "acceptedNameUsageID") {
    map_id_is_bad <- map_id_is_bad &
      !grepl("^accepted$", tax_dat$taxonomicStatus, ignore.case = TRUE)
  }

  # Get vectors of bad values
  bad_taxon_id <- tax_dat$taxonID[map_id_is_bad]
  bad_sci_name <- tax_dat$scientificName[map_id_is_bad]
  bad_acc_id <- tax_dat[[col_select]][map_id_is_bad]

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(map_id_is_bad) == 0,
      msg = glue::glue(
        "check_mapping failed.
          taxonID detected with identical {col_select}.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg(col_select, bad_acc_id, is_last = TRUE)}",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    error_msg <- glue::glue("taxonID detected with identical {col_select}")
    assert_that_d(
      sum(map_id_is_bad) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        {{ col_select }} := bad_acc_id,
        error = error_msg,
        check = "check_mapping"
      ),
      msg = error_msg,
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

#' check_mapping sub-check: check that no acceptedNameUsageID are missing
#' from taxonID.
#'
#' Required columns:
#' - taxonID
#' - acceptedNameUsageID
#'
#' @inherit check_taxon_id_not_na
#' @noRd
check_mapping_exists <- function(tax_dat,
                                 on_fail = dct_options()$on_fail,
                                 on_success = dct_options()$on_success,
                                 col_select = "acceptedNameUsageID",
                                 run = TRUE,
                                 quiet = dct_options()$quiet) {
  # Early exit with NULL if req'd cols not present
  if (
    !"taxonID" %in% colnames(tax_dat) ||
      !"scientificName" %in% colnames(tax_dat) ||
      !col_select %in% colnames(tax_dat) ||
      run == FALSE
  ) {
    return(NULL)
  }

  # Check for names that lack a taxonID for acceptedNameUsageID
  map_id_is_good <- tax_dat[[col_select]] %in% tax_dat$taxonID
  map_id_is_na <- is.na(tax_dat[[col_select]])
  map_id_is_bad <- !map_id_is_na & !map_id_is_good

  # Get vectors of bad values
  bad_taxon_id <- tax_dat$taxonID[map_id_is_bad]
  bad_sci_name <- tax_dat$scientificName[map_id_is_bad]
  bad_acc_id <- tax_dat[[col_select]][map_id_is_bad]

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(map_id_is_bad) == 0,
      msg = glue::glue(
        "check_mapping failed.
          taxonID detected whose {col_select} value does not \\
          map to taxonID of an existing name.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg(col_select, bad_acc_id, is_last = TRUE)}",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    error_msg <- glue::glue(
      "taxonID detected whose {col_select} value does not \\
      map to taxonID of an existing name."
    )
    assert_that_d(
      sum(map_id_is_bad) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        {{ col_select }} := bad_acc_id,
        error = error_msg,
        check = "check_mapping"
      ),
      msg = error_msg,
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

#' Check mapping of usage taxonomic IDs
#'
#' Check that values of terms like 'acceptedUsageID' map properly to taxonID in
#' Darwin Core (DWC) taxonomic data.
#'
#' The following rules are enforced:
#' - Value of taxonID may not be identical to that of the selected column within
#' a single row (in other words, a name cannot be its own accepted name,
#' parent taxon, or basionym).
#' - Every value in the selected column must have a corresponding taxonID.
#'
#' `col_select` can take one of the following values:
#' - `"acceptedNameUsageID"`: taxonID corresponding to the accepted name (of
#' a synonym).
#' - `"parentNameUsageID"`: taxonID corresponding to the immediate parent taxon
#' of a name (for example, for a species, this would be the genus).
#' - `"originalNameUsageID"`: taxonID corresponding to the basionym of a name.
#'
#' @param tax_dat `r param_tax_dat`
#' @param on_fail `r param_on_fail`
#' @param on_success `r param_on_success`
#' @param col_select Character vector of length 1; the name of the column
#' (DWC term) to check. Default `"acceptedNameUsageID"`.
#' @param quiet `r param_quiet`
#'
#' @inherit dct_check_taxon_id return
#' @example inst/examples/dct_check_mapping.R
#' @autoglobal
#' @export
#'
dct_check_mapping <- function(tax_dat,
                              on_fail = dct_options()$on_fail,
                              on_success = dct_options()$on_success,
                              col_select = "acceptedNameUsageID",
                              quiet = dct_options()$quiet) {
  # Check input format
  assertthat::assert_that(
    inherits(tax_dat, "data.frame"),
    msg = "'tax_dat' must be of class 'data.frame'"
  )
  assertthat::assert_that(assertthat::is.string(on_fail))
  assertthat::assert_that(assertthat::is.string(on_success))
  assertthat::assert_that(assertthat::is.string(col_select))
  assertthat::assert_that(
    on_fail %in% c("error", "summary"),
    msg = "on_fail must be one of 'error' or 'summary'"
  )
  assertthat::assert_that(
    on_success %in% c("data", "logical"),
    msg = "on_success must be one of 'data' or 'logical'"
  )
  valid_col_select <- c(
    "acceptedNameUsageID", "parentNameUsageID", "originalNameUsageID"
  )
  valid_col_select_str <- glue::glue_collapse(
    valid_col_select, "', '",
    last = "', or '"
  )
  valid_col_select_str <- paste0("'", valid_col_select_str, "'")
  assertthat::assert_that(
    col_select %in% valid_col_select,
    msg = glue::glue("on_fail must be one of {valid_col_select_str}")
  )

  # Run main checks
  check_res <- list(
    # Check no names map to self
    check_mapping_to_self(
      tax_dat,
      on_fail = on_fail, on_success = "logical",
      col_select = col_select,
      quiet = quiet
    ),
    # Check all names have matching taxonID for selected column
    check_mapping_exists(
      tax_dat,
      on_fail = on_fail, on_success = "logical",
      col_select = col_select,
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
