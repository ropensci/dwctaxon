#' check_mapping_accepted_status sub-check: check that taxonomicStatus includes
#' needed values
#'
#' Required columns: none
#'
#' Required checks: none
#'
#' @inherit check_taxon_id_not_na
#' @autoglobal
#' @noRd
check_mapping_strict_status <- function(tax_dat,
                                        on_fail = dct_options()$on_fail,
                                        on_success = dct_options()$on_success,
                                        valid_tax_status =
                                          dct_options()$valid_tax_status,
                                        run = TRUE,
                                        quiet = dct_options()$quiet) {
  # Set defaults ----

  if (run == FALSE) {
    return(NULL)
  }

  # Check that valid_tax_status includes needed values
  valid_tax_status_lacks_syn <- !grepl(
    "synonym", valid_tax_status,
    ignore.case = FALSE
  )
  valid_tax_status_lacks_acc <- !grepl(
    "accepted", valid_tax_status,
    ignore.case = FALSE
  )
  valid_tax_status_lacks_var <- !grepl(
    "variant", valid_tax_status,
    ignore.case = FALSE
  )
  any_missing <- any(c(
    valid_tax_status_lacks_syn,
    valid_tax_status_lacks_acc,
    valid_tax_status_lacks_var
  ))

  missing_tax_status <- c("synonym", "accepted", "variant")[
    c(
      valid_tax_status_lacks_syn,
      valid_tax_status_lacks_acc,
      valid_tax_status_lacks_var
    )
  ]

  missing_tax_status <- paste(missing_tax_status, collapse = ", ")

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      !any_missing,
      msg = glue::glue(
        "check_mapping_accepted_status failed
         valid_tax_status missing required value or values.
         Missing values: {missing_tax_status}
         Current valid_tax_status: '{valid_tax_status}'"
      )
    )
  }
  if (on_fail == "summary") {
    error_msg <- glue::glue(
      "valid_tax_status missing required value or values: \\
          {missing_tax_status}"
    )
    assert_that_d(
      !any_missing,
      data = tibble::tibble(
        error = error_msg,
        check = "check_mapping_accepted_status"
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

#' check_mapping_accepted_status sub-check: check that synonyms map to accepted
#' names
#'
#' Required columns:
#' - taxonID
#' - acceptedNameUsageID
#'
#' Required checks:
#' - check_mapping_strict_status
#'
#' @inherit check_taxon_id_not_na
#' @autoglobal
#' @noRd
check_syn_map_to_acc <- function(tax_dat,
                                 on_fail = dct_options()$on_fail,
                                 on_success = dct_options()$on_success,
                                 run = TRUE,
                                 quiet = dct_options()$quiet) {
  # Early exit with NULL if req'd cols not present
  if (
    !"taxonID" %in% colnames(tax_dat) ||
      !"acceptedNameUsageID" %in% colnames(tax_dat) ||
      !"taxonomicStatus" %in% colnames(tax_dat) ||
      run == FALSE
  ) {
    return(NULL)
  }

  # Separate accepted names and synonyms
  tax_dat_accepted <-
    tax_dat |>
    dplyr::filter(
      stringr::str_detect(
        taxonomicStatus, stringr::fixed("accepted", ignore_case = FALSE)
      )
    )
  tax_dat_synonyms <-
    tax_dat |>
    dplyr::filter(
      stringr::str_detect(
        taxonomicStatus, stringr::fixed("synonym", ignore_case = FALSE)
      )
    )

  # Check all synonyms map to accepted names
  syn_id_not_in_acc_id <- !tax_dat_synonyms$acceptedNameUsageID %in%
    tax_dat_accepted$taxonID
  bad_taxon_id <- tax_dat_synonyms$taxonID[syn_id_not_in_acc_id]
  bad_acc_id <- tax_dat_synonyms$acceptedNameUsageID[syn_id_not_in_acc_id]
  bad_sci_name <- NULL
  if ("scientificName" %in% colnames(tax_dat_synonyms)) {
    bad_sci_name <- tax_dat_synonyms$scientificName[syn_id_not_in_acc_id]
  }

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(syn_id_not_in_acc_id) == 0,
      msg = glue::glue(
        "check_mapping_accepted_status failed.
          synonym detected whose acceptedNameUsageID value does not \\
          map to taxonID of an accepted name.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg('acceptedNameUsageID', bad_acc_id, is_last = TRUE)}",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    err_msg <- paste(
      "synonym detected whose acceptedNameUsageID value does not",
      "map to taxonID of an accepted name"
    )
    assert_that_d(
      sum(syn_id_not_in_acc_id) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        acceptedNameUsageID = bad_acc_id,
        error = err_msg,
        check = "check_mapping_accepted_status"
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

#' check_mapping_accepted_status sub-check: any row with acceptedNameUsageID
#' must have non-missing taxonomicStatus
#'
#' Required columns:
#' - acceptedNameUsageID
#' - taxonomicStatus
#'
#' Required checks: none
#' @noRd
#' @autoglobal
check_acc_id_has_tax_status <- function(tax_dat,
                                        on_fail = dct_options()$on_fail,
                                        on_success = dct_options()$on_success,
                                        run = TRUE,
                                        quiet = dct_options()$quiet) {
  # Early exit with NULL if req'd cols not present
  if (
    !"taxonID" %in% colnames(tax_dat) ||
      !"acceptedNameUsageID" %in% colnames(tax_dat) ||
      !"taxonomicStatus" %in% colnames(tax_dat) ||
      run == FALSE
  ) {
    return(NULL)
  }

  # Filter to names with acceptedNameUsageID
  tax_dat_with_acc_usage_id <- dplyr::filter(
    tax_dat, !is.na(acceptedNameUsageID)
  )

  tax_status_is_missing <- is.na(tax_dat_with_acc_usage_id$taxonomicStatus)
  bad_taxon_id <- tax_dat_with_acc_usage_id$taxonID[tax_status_is_missing]
  bad_acc_id <- tax_dat_with_acc_usage_id$acceptedNameUsageID[
    tax_status_is_missing
  ]
  bad_sci_name <- NULL
  if ("scientificName" %in% colnames(tax_dat_with_acc_usage_id)) {
    bad_sci_name <- tax_dat_with_acc_usage_id$scientificName[
      tax_status_is_missing
    ]
  }

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(tax_status_is_missing) == 0,
      msg = glue::glue(
        "check_mapping_accepted_status failed.
          rows detected whose acceptedNameUsageID value is not missing, \\
          but have missing taxonomicStatus.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg('acceptedNameUsageID', bad_acc_id, is_last = TRUE)}",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    error_msg <- paste(
      "rows detected whose acceptedNameUsageID value is not missing,",
      "but have missing taxonomicStatus"
    )
    assert_that_d(
      sum(tax_status_is_missing) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        acceptedNameUsageID = bad_acc_id,
        error = error_msg,
        check = "check_mapping_accepted_status"
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

#' check_mapping_strict_status sub-check: any row with acceptedNameUsageID must
#' have valid taxonomicStatus
#'
#' Required columns:
#' - acceptedNameUsageID
#' - taxonomicStatus
#'
#' Required checks: none
#' @noRd
#' @autoglobal
check_acc_id_valid_tax_status <- function(tax_dat,
                                          on_fail = dct_options()$on_fail,
                                          on_success =
                                            dct_options()$"on_success",
                                          run = TRUE,
                                          quiet = dct_options()$quiet) {
  # Early exit with NULL if req'd cols not present
  if (
    !"taxonID" %in% colnames(tax_dat) ||
      !"acceptedNameUsageID" %in% colnames(tax_dat) ||
      !"taxonomicStatus" %in% colnames(tax_dat) ||
      run == FALSE
  ) {
    return(NULL)
  }

  # Filter to names with acceptedNameUsageID
  tax_dat_with_acc_usage_id <- dplyr::filter(
    tax_dat, !is.na(acceptedNameUsageID)
  )

  # Don't count NA as non-valid, since these are caught separately by
  # check_acc_id_has_tax_status
  acc_usage_id_is_not_valid <- !grepl(
    "accepted|synonym|variant",
    tax_dat_with_acc_usage_id$taxonomicStatus
  ) &
    !is.na(tax_dat_with_acc_usage_id$taxonomicStatus)

  bad_taxon_id <- tax_dat_with_acc_usage_id$taxonID[acc_usage_id_is_not_valid]
  bad_tax_status <- tax_dat_with_acc_usage_id$taxonomicStatus[
    acc_usage_id_is_not_valid
  ]
  bad_acc_id <- tax_dat_with_acc_usage_id$acceptedNameUsageID[
    acc_usage_id_is_not_valid
  ]
  bad_sci_name <- NULL
  if ("scientificName" %in% colnames(tax_dat_with_acc_usage_id)) {
    bad_sci_name <- tax_dat_with_acc_usage_id$scientificName[
      acc_usage_id_is_not_valid
    ]
  }

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(acc_usage_id_is_not_valid) == 0,
      msg = glue::glue(
        "check_mapping_accepted_status failed.
          rows detected whose acceptedNameUsageID value is not missing, \\
          but with taxonomicStatus that is not 'accepted', 'synonym', or \\
          'variant'.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('acceptedNameUsageID', bad_acc_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg('taxonomicStatus', bad_tax_status, is_last = TRUE)}",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    err_msg <- paste(
      "rows detected whose acceptedNameUsageID value is not missing,",
      "but with taxonomicStatus that is not 'accepted', 'synonym', or",
      "'variant'"
    )
    assert_that_d(
      sum(acc_usage_id_is_not_valid) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        acceptedNameUsageID = bad_acc_id,
        scientificName = bad_sci_name,
        taxonomicStatus = bad_tax_status,
        error = err_msg,
        check = "check_mapping_accepted_status"
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

#' check_mapping_accepted_status sub-check: variants cannot map to variants
#'
#' Required columns:
#' - taxonID
#' - acceptedNameUsageID
#'
#' @param tax_dat_variants Input taxonomic data filtered to taxonomicStatus
#'   containing "variant"
#' Required checks: none
#' @noRd
#' @autoglobal
check_variant_map_to_nonvar <- function(tax_dat,
                                        on_fail = dct_options()$on_fail,
                                        on_success = dct_options()$on_success,
                                        run = TRUE,
                                        quiet = dct_options()$quiet) {
  # Early exit with NULL if req'd cols not present
  if (
    !"taxonID" %in% colnames(tax_dat) ||
      !"acceptedNameUsageID" %in% colnames(tax_dat) ||
      !"taxonomicStatus" %in% colnames(tax_dat) ||
      run == FALSE
  ) {
    return(NULL)
  }

  tax_dat_variants <-
    tax_dat |>
    dplyr::filter(
      stringr::str_detect(
        taxonomicStatus, stringr::fixed("variant", ignore_case = FALSE)
      )
    )

  var_id_maps_to_var_id <- tax_dat_variants$acceptedNameUsageID %in%
    tax_dat_variants$taxonID

  bad_taxon_id <- tax_dat_variants$taxonID[var_id_maps_to_var_id]
  bad_acc_id <- tax_dat_variants$acceptedNameUsageID[var_id_maps_to_var_id]

  bad_sci_name <- NULL
  if ("scientificName" %in% colnames(tax_dat_variants)) {
    bad_sci_name <- tax_dat_variants$scientificName[var_id_maps_to_var_id]
  }

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(var_id_maps_to_var_id) == 0,
      msg = glue::glue(
        "check_mapping_accepted_status failed.
          variant(s) detected whose acceptedNameUsageID value maps to \\
          taxonID of a variant.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg('acceptedNameUsageID', bad_acc_id, is_last = TRUE)}",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    error_msg <- paste(
      "variant(s) detected whose acceptedNameUsageID value maps to",
      "taxonID of a variant"
    )
    assert_that_d(
      sum(var_id_maps_to_var_id) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        acceptedNameUsageID = bad_acc_id,
        error = error_msg,
        check = "check_mapping_accepted_status"
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

#' check_mapping_accepted_status sub-check: variants must map to something
#'
#' Required columns:
#' - taxonomicStatus
#' - acceptedNameUsageID
#'
#' Required checks: none
#' @noRd
#' @autoglobal
check_variant_map_to_something <- function(tax_dat,
                                           on_fail = dct_options()$on_fail,
                                           on_success =
                                             dct_options()$on_success,
                                           run = TRUE,
                                           quiet = dct_options()$quiet) {
  # Early exit with NULL if req'd cols not present
  if (
    !"taxonID" %in% colnames(tax_dat) ||
      !"acceptedNameUsageID" %in% colnames(tax_dat) ||
      !"taxonomicStatus" %in% colnames(tax_dat) ||
      run == FALSE
  ) {
    return(NULL)
  }

  tax_dat_variants <-
    tax_dat |>
    dplyr::filter(
      stringr::str_detect(
        taxonomicStatus, stringr::fixed("variant", ignore_case = FALSE)
      )
    )

  var_id_no_acc_id <- is.na(tax_dat_variants$acceptedNameUsageID)
  bad_taxon_id <- tax_dat_variants$taxonID[var_id_no_acc_id]

  bad_sci_name <- NULL
  if ("scientificName" %in% colnames(tax_dat_variants)) {
    bad_sci_name <- tax_dat_variants$scientificName[var_id_no_acc_id]
  }

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(var_id_no_acc_id) == 0,
      msg = glue::glue(
        "check_mapping_accepted_status failed.
          variant(s) detected who lack an acceptedNameUsageID.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name, is_last = TRUE)}",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    err_msg <- "variant(s) detected who lack an acceptedNameUsageID"
    assert_that_d(
      sum(var_id_no_acc_id) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        error = err_msg,
        check = "check_mapping_accepted_status"
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

#' check_mapping_accepted_status sub-check: accepted names can't map to anything
#'
#' Required columns:
#' - taxonomicStatus
#' - acceptedNameUsageID
#'
#' Required checks: none
#' @noRd
#' @autoglobal
check_accepted_map_to_nothing <- function(tax_dat,
                                          on_fail = dct_options()$on_fail,
                                          on_success = dct_options()$on_success,
                                          run = TRUE,
                                          quiet = dct_options()$quiet) {
  # Early exit with NULL if req'd cols not present
  if (
    !"taxonID" %in% colnames(tax_dat) ||
      !"acceptedNameUsageID" %in% colnames(tax_dat) ||
      !"taxonomicStatus" %in% colnames(tax_dat) ||
      run == FALSE
  ) {
    return(NULL)
  }

  tax_dat_accepted <-
    tax_dat |>
    dplyr::filter(
      stringr::str_detect(
        taxonomicStatus, stringr::fixed("accepted", ignore_case = FALSE)
      )
    )

  acc_id_map_to_something <- !is.na(tax_dat_accepted$acceptedNameUsageID)
  bad_taxon_id <- tax_dat_accepted$taxonID[acc_id_map_to_something]

  bad_sci_name <- NULL
  if ("scientificName" %in% colnames(tax_dat_accepted)) {
    bad_sci_name <- tax_dat_accepted$scientificName[acc_id_map_to_something]
  }

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(acc_id_map_to_something) == 0,
      msg = glue::glue(
        "check_mapping_accepted_status failed.
          accepted name(s) detected with a non-missing value for \\
          acceptedNameUsageID.
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name, is_last = TRUE)}",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    err_msg <- paste(
      "accepted name(s) detected with a non-missing value for",
      "acceptedNameUsageID"
    )
    assert_that_d(
      sum(acc_id_map_to_something) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        error = err_msg,
        check = "check_mapping_accepted_status"
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
