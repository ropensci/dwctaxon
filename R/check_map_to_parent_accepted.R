#' check_mapping_parent_accepted
#'
#' Required columns:
#' - taxonID
#' - taxonomicStatus
#' - parentNameUsageID
#'
#' Required checks: none
#' @noRd
#' @autoglobal
check_map_to_parent_accepted <- function(tax_dat,
                                         on_fail = dct_options()$on_fail,
                                         on_success = dct_options()$on_success,
                                         run = TRUE,
                                         quiet = dct_options()$quiet) {
  # Early exit with NULL if req'd cols not present
  if (
    !"taxonID" %in% colnames(tax_dat) ||
      !"parentNameUsageID" %in% colnames(tax_dat) ||
      !"taxonomicStatus" %in% colnames(tax_dat) ||
      run == FALSE
  ) {
    return(NULL)
  }

  # taxonID of names that are parents
  tax_dat_parent_ids <-
    tax_dat |>
    dplyr::filter(!is.na(parentNameUsageID)) |>
    dplyr::pull(parentNameUsageID)

  # taxonID of names that are synonyms
  tax_dat_synonym_ids <-
    tax_dat |>
    dplyr::filter(stringr::str_detect(taxonomicStatus, "synonym")) |>
    dplyr::pull(taxonID)

  parent_id_is_synonym <- tax_dat_parent_ids %in% tax_dat_synonym_ids
  bad_parent_taxon_id <- tax_dat_parent_ids[parent_id_is_synonym]
  parent_id_maps_to_synonym <-
    tax_dat$parentNameUsageID %in% bad_parent_taxon_id
  bad_taxon_id <- tax_dat$taxonID[parent_id_maps_to_synonym]

  bad_sci_name <- NULL
  if ("scientificName" %in% colnames(tax_dat)) {
    bad_sci_name <- tax_dat$scientificName[parent_id_maps_to_synonym]
  }

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      sum(parent_id_is_synonym) == 0,
      msg = glue::glue(
        "check_map_to_parent_accepted failed.
          name(s) detected with parentNameUsageID that corresponds to a \\
          synonym
          {make_msg('taxonID', bad_taxon_id)}\\
          {make_msg('scientificName', bad_sci_name)}\\
          {make_msg('parentNameUsageID', bad_parent_taxon_id, is_last = TRUE)}\\
          ",
        .transformer = null_transformer("")
      )
    )
  }
  if (on_fail == "summary") {
    err_msg <-
      "name(s) detected with parentNameUsageID that corresponds to a synonym"
    assert_that_d(
      sum(parent_id_is_synonym) == 0,
      data = tibble::tibble(
        taxonID = bad_taxon_id,
        scientificName = bad_sci_name,
        error = err_msg,
        check = "check_map_to_parent_accepted"
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
