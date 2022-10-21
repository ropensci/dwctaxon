#' Check that a taxonomic database is correctly formatted
#'
#' Stops with an error if any check fails. Most checks are geared towards being
#' able to use the taxonomic database for taxonomic name resolution at the
#' species level.
#'
#' For `check_acc_syn_diff` and `strict_mapping`, "accepted", "synonym", and
#' "variant" are determined by string matching of `taxonomicStatus`; so
#' "provisionally accepted" is counted as "accepted", "ambiguous synonym" is
#' counted as "synonym", etc (case-sensitive).
#'
#' For `strict_mapping`, the following rules are enforced:
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
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format
#' @param check_taxon_id Logical; should all instances of `taxonID` be required
#' to be non-missing and unique?
#' @param check_mapping Logical; should all values of `acceptedNameUsageID` be
#' required to map to the `taxonID` of an existing name?
#' @param check_taxonomic_status Logical; should all taxonomic names be required
#'   to include a valid value for taxonomic status (by default, "accepted",
#'   "synonym", or "variant")?
#' @param check_acc_syn_diff Logical; should each scientific name be allowed
#'   to have only one taxonomic status?
#' @param check_col_names Logical; should all column names be required to
#' be a valid Darwin Core term?
#' @param strict_mapping Logical; should rules about mapping of variants and
#'   synonyms be enforced? (see Details)
#' @param valid_tax_status Character vector of length 1; valid values for
#'   `taxonomicStatus`. Each value must be separated by a space. Default
#'   "accepted synonym variant NA". "NA" indicates that missing (NA) values are
#'   valid. Case-sensitive. Can also be set with the environmental variable
#'   "VALID_TAX_STATUS".
#'
#' @return Dataframe; taxonomic database in Darwin Core format. Will
#' be the same as the input.
#' @autoglobal
#' @export
#' @examples
#' dct_validate(dct_filmies)
#'
dct_validate <- function(tax_dat,
                         check_taxon_id = TRUE,
                         check_mapping = TRUE,
                         check_taxonomic_status = TRUE,
                         check_acc_syn_diff = TRUE,
                         check_col_names = TRUE,
                         strict_mapping = TRUE,
                         valid_tax_status = Sys.getenv(
                          "VALID_TAX_STATUS",
                          unset = "accepted synonym variant NA")
 ) {

  # tax_dat must be a dataframe
  assertthat::assert_that(
    inherits(tax_dat, "data.frame"),
    msg = "'tax_dat' must be of class 'data.frame'"
  )
  # others are all logical flags
  assertthat::assert_that(assertthat::is.flag(check_taxon_id))
  assertthat::assert_that(assertthat::is.flag(check_mapping))
  assertthat::assert_that(assertthat::is.flag(check_taxonomic_status))
  assertthat::assert_that(assertthat::is.flag(check_acc_syn_diff))
  assertthat::assert_that(assertthat::is.flag(check_col_names))
  assertthat::assert_that(assertthat::is.flag(strict_mapping))
  assertthat::assert_that(assertthat::is.string(valid_tax_status))

  # Check for required column types (only if columns are present)
  # taxonID could be numeric or character
  assert_col(
    tax_dat, "taxonID", c("character", "numeric", "integer"), req_col = FALSE
  )
  assert_col(
      tax_dat, "acceptedNameUsageID", c("character", "numeric", "integer"),
      req_col = FALSE
    )
  assert_col(tax_dat, "scientificName", "character", req_col = FALSE)
  assert_col(tax_dat, "taxonomicStatus", "character", req_col = FALSE)

  # Check for taxon ID -----
  if (isTRUE(check_taxon_id)) {
    assert_col(tax_dat, "taxonID", req_by = "check_taxon_id")
    assert_dat(tax_dat, assertr::not_na, taxonID)
    assert_dat(tax_dat, assertr::is_uniq, taxonID)
  }

  # Check for name mapping -----
  if (isTRUE(check_mapping)) {
    # check pre-reqs
    assertthat::assert_that(
      isTRUE(check_taxon_id),
      msg = "`check_mapping` requires `check_taxon_id` to be TRUE"
    )
    assert_col(tax_dat, "acceptedNameUsageID", req_by = "check_mapping")
    assert_col(tax_dat, "scientificName", req_by = "check_mapping")

    map_id_is_good <- tax_dat$acceptedNameUsageID %in% tax_dat$taxonID
    map_id_is_na <- is.na(tax_dat$acceptedNameUsageID)
    map_id_is_bad <- !map_id_is_na & !map_id_is_good
    assertthat::assert_that(
      sum(map_id_is_bad) == 0,
      msg = glue::glue(
        "`check_mapping` failed.
          `taxonID`(s) detected whose `acceptedNameUsageID` value does not \\
          map to `taxonID` of an existing name.
          Bad `taxonID`: \\
          {paste(tax_dat$taxonID[map_id_is_bad], collapse = ', ')}
          Bad `scientificName`: \\
          {paste(tax_dat$scientificName[map_id_is_bad], collapse = ', ')}"
      )
    )
  }

  # Check that all names have valid taxonomic status
  if (isTRUE(check_taxonomic_status)) {
    # check pre-reqs
    assertthat::assert_that(
      isTRUE(check_taxon_id),
      msg = "`check_taxonomic_status` requires `check_taxon_id` to be TRUE"
    )
    # taxonID already checked
    assert_col(tax_dat, "taxonomicStatus", req_by = "check_taxonomic_status")
    # Convert valid_tax_status to vector
    valid_tax_status_v <- strsplit(valid_tax_status, " +")[[1]] |>
      unique()
    valid_tax_status_v[valid_tax_status_v == "NA"] <- NA_character_
    tax_status_is_bad <- !tax_dat$taxonomicStatus %in% valid_tax_status_v
    assertthat::assert_that(
      sum(tax_status_is_bad) == 0,
      msg = glue::glue(
        "`check_taxonomic_status` failed.
          `taxonID`(s) detected whose `taxonomicStatus` value is not \\
          in `valid_tax_status` ('{valid_tax_status}')
          Bad `taxonID`: \\
          {paste(tax_dat$taxonID[tax_status_is_bad], collapse = ', ')}
          Bad `taxonomicStatus`: \\
          {paste(tax_dat$taxonomicStatus[tax_status_is_bad], collapse = ', ')}"
      )
    )
  }

  if (isTRUE(strict_mapping)) {
    # Check pre-reqs
    # require check_mapping
    assertthat::assert_that(
      isTRUE(check_mapping),
      msg = "`strict_mapping` requires `check_mapping` to be TRUE"
    )
    # require check_taxonomic_status
    assertthat::assert_that(
      isTRUE(check_taxonomic_status),
      msg = "`strict_mapping` requires `check_taxonomic_status` to be TRUE"
    )
    # require accepted, synonym, variant in valid status
    assertthat::assert_that(
      grepl("synonym", valid_tax_status, ignore.case = FALSE),
      msg = "'synonym' not in `valid_tax_status`"
    )
    assertthat::assert_that(
      grepl("accepted", valid_tax_status, ignore.case = FALSE),
      msg = "'accepted' not in `valid_tax_status`"
    )
    assertthat::assert_that(
      grepl("variant", valid_tax_status, ignore.case = FALSE),
      msg = "'variant' not in `valid_tax_status`"
    )
    # taxonID, acceptedNameUsageID, scientificName already required

    # Separate accepted names, synonyms, and variants
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
    tax_dat_variants <-
      tax_dat |>
      dplyr::filter(
        stringr::str_detect(
          taxonomicStatus, stringr::fixed("variant", ignore_case = FALSE)
        )
      )
    tax_dat_with_acc_usage_id <- dplyr::filter(
      tax_dat, !is.na(acceptedNameUsageID)
    )
    # all synonyms must map to accepted names
    syn_id_not_in_acc_id <- !tax_dat_synonyms$acceptedNameUsageID %in%
      tax_dat_accepted$taxonID
    assertthat::assert_that(
      sum(syn_id_not_in_acc_id) == 0,
      msg = glue::glue(
        "`strict_mapping` failed.
          synonym(s) detected whose `acceptedNameUsageID` value does not \\
          map to `taxonID` of an accepted name.
          Bad `taxonID`: \\
          {paste(tax_dat_synonyms$taxonID[syn_id_not_in_acc_id], \\
            collapse = ', ')}
          Bad `scientificName`: \\
          {paste(tax_dat_synonyms$scientificName[syn_id_not_in_acc_id], collapse = ', ')}" # nolint
      )
    )
    # any row with acceptedNameUsageID must have non-missing taxonomicStatus
    missing_acc_usage_id <- is.na(tax_dat_with_acc_usage_id$taxonomicStatus)
    assertthat::assert_that(
      sum(missing_acc_usage_id) == 0,
      msg = glue::glue(
        "`strict_mapping` failed.
          rows(s) detected whose `acceptedNameUsageID` value is not missing, \\
          but have missing `taxonomicStatus`.
          Bad `taxonID`: \\
          {paste(tax_dat_with_acc_usage_id$taxonID[missing_acc_usage_id], \\
            collapse = ', ')}
          Bad `scientificName`: \\
          {paste(tax_dat_with_acc_usage_id$scientificName[missing_acc_usage_id], collapse = ', ')}" # nolint
      )
    )
    # any row with acceptedNameUsageID must have valid taxonomicStatus
    bad_acc_usage_id <- !grepl("accepted|synonym|variant",
      tax_dat_with_acc_usage_id$taxonomicStatus)
    assertthat::assert_that(
      sum(bad_acc_usage_id) == 0,
      msg = glue::glue(
        "`strict_mapping` failed.
          rows(s) detected whose `acceptedNameUsageID` value is not missing, \\
          but have `taxonomicStatus` that is not 'accepted', 'synonym', or \\
          'variant'.
          Bad `taxonID`: \\
          {paste(tax_dat_with_acc_usage_id$taxonID[bad_acc_usage_id], \\
            collapse = ', ')}
          Bad `scientificName`: \\
          {paste(tax_dat_with_acc_usage_id$scientificName[bad_acc_usage_id], \\
            collapse = ', ')}
          Bad `taxonomicStatus`: \\
          {paste(tax_dat_with_acc_usage_id$taxonomicStatus[bad_acc_usage_id], \\
            collapse = ', ')}"
      )
    )

    # variants cannot map to variants
    var_id_map_to_var_id <- tax_dat_variants$acceptedNameUsageID %in%
      tax_dat_variants$taxonID
    assertthat::assert_that(
      sum(var_id_map_to_var_id) == 0,
      msg = glue::glue(
        "`strict_mapping` failed.
          variants(s) detected whose `acceptedNameUsageID` value maps to \\
          `taxonID` of a variant.
          Bad `taxonID`: \\
          {paste(tax_dat_variants$taxonID[var_id_map_to_var_id], \\
            collapse = ', ')}
          Bad `scientificName`: \\
          {paste(tax_dat_variants$scientificName[var_id_map_to_var_id], collapse = ', ')}" # nolint
      )
    )
    # variants must map to something
    var_id_no_acc_id <- is.na(tax_dat_variants$acceptedNameUsageID)
    assertthat::assert_that(
      sum(var_id_no_acc_id) == 0,
      msg = glue::glue(
        "`strict_mapping` failed.
          variants(s) detected who lack an `acceptedNameUsageID`.
          Bad `taxonID`: \\
          {paste(tax_dat_variants$taxonID[var_id_no_acc_id], \\
            collapse = ', ')}
          Bad `scientificName`: \\
          {paste(tax_dat_variants$scientificName[var_id_no_acc_id], collapse = ', ')}" # nolint
      )
    )
    # accepted names cannot map to anything
    acc_id_map_to_something <- !is.na(tax_dat_accepted$acceptedNameUsageID)
    assertthat::assert_that(
      sum(acc_id_map_to_something) == 0,
      msg = glue::glue(
        "`strict_mapping` failed.
          Accepted names(s) detected with a non-missing value for \\
          `acceptedNameUsageID`.
          Bad `taxonID`: \\
          {paste(tax_dat_accepted$taxonID[acc_id_map_to_something], \\
            collapse = ', ')}
          Bad `scientificName`: \\
          {paste(tax_dat_accepted$scientificName[acc_id_map_to_something], collapse = ', ')}" # nolint
      )
    )
  }

  # Check that accepted names and synonyms are distinct
  if (isTRUE(check_acc_syn_diff)) {
    assert_col(tax_dat, "taxonomicStatus")
    assert_col(tax_dat, "scientificName")
    # Separate accepted names and synonyms
    tax_dat_accepted <-
      tax_dat |>
      dplyr::filter(
        stringr::str_detect(
          taxonomicStatus, stringr::fixed("accepted", ignore_case = TRUE)
        )
      )
    tax_dat_synonyms <-
      tax_dat |>
      dplyr::filter(
        stringr::str_detect(
          taxonomicStatus, stringr::fixed("synonym", ignore_case = TRUE)
        )
      )
    if (nrow(tax_dat_accepted) > 0 && nrow(tax_dat_synonyms) > 0) {
      tax_dat_no_overlap_check <-
        tax_dat_accepted |>
        dplyr::inner_join(tax_dat_synonyms, by = "scientificName")
      # Extract bad taxon IDs and species
      bad_taxon_id <- ""
      if (!is.null(tax_dat_no_overlap_check[["taxonID"]])) {
        bad_taxon_id <- paste(
          tax_dat_no_overlap_check$taxonID, collapse = ", ")
      }
      bad_taxon_species <- ""
      if (!is.null(tax_dat_no_overlap_check[["scientificName"]])) {
        bad_taxon_species <- paste(
          tax_dat_no_overlap_check$scientificName,
          collapse = ", "
        )
      }
      assertthat::assert_that(
        nrow(tax_dat_no_overlap_check) == 0,
        msg = glue::glue(
          "`check_acc_syn_diff` failed.
          `taxonID`(s) detected whose scientific names \\
          appear in both accepted names and synonyms
          Bad `taxonID`: {bad_taxon_id}
          Bad `scientificName`: {bad_taxon_species}"
        )
      )
    }
  }

  # Check that column names are valid
  if (isTRUE(check_col_names)) {
    bad_col_names <- setdiff(colnames(tax_dat), dct_terms$term)
    assertthat::assert_that(
      length(bad_col_names) == 0,
      msg = glue::glue(
        "`check_col_names` failed. Invalid column names present: \\
        {paste(bad_col_names, collapse = ', ')}. \\
        See dct_terms for valid column names.")
    )
  }

  tax_dat
}
