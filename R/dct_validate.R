#' Check that a taxonomic database is correctly formatted
#'
#' Stops with an error if any check fails. Most checks are geared towards being
#' able to use the taxonomic database for taxonomic name resolution at the
#' species level.
#'
#' For `check_taxonomic_status` and `check_acc_syn_diff`, "accepted" and
#' "synonym" are determined by string matching of `taxonomicStatus`; so
#' "provisionally accepted" is counted as "accepted", "ambiguous synonym" is
#' counted as "synonym", etc (not case-sensitive).
#'
#' For `check_mapping`, the `acceptedNameUsageID` must map to the `taxonID` of
#' an existing name which itself does not have an `acceptedNameUsageID` (i.e.,
#' no multiple levels of mapping allowed).
#'
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format
#' @param check_taxon_id Logical; should all instances of `taxonID` be required
#' to be non-missing and unique?
#' @param check_mapping Logical; should all values of `acceptedNameUsageID` be
#' required to map to the `taxonID` of an existing name?
#' @param check_taxonomic_status Logical; should all taxonomic names be required
#' to include the status of either "accepted" or "synonym"?
#' @param check_acc_syn_diff Logical; should accepted names and synonyms be
#' required to be different?
#' @param check_col_names Logical; should all column names be required to
#' be a valid Darwin Core term?
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
                         check_col_names = TRUE) {

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


  # Check for unique, non-missing taxon ID
  if (isTRUE(check_taxon_id)) {
    assert_col(tax_dat, "taxonID")
    assert_dat(tax_dat, assertr::not_na, taxonID)
    assert_dat(tax_dat, assertr::is_uniq, taxonID)
  }

  # Check for name mapping
  if (isTRUE(check_mapping)) {
    # check pre-reqs
    assertthat::assert_that(
      isTRUE(check_taxon_id),
      msg = "`check_mapping` requires `check_taxon_id` to be TRUE"
    )
    assert_col(tax_dat, "acceptedNameUsageID")
    assert_col(tax_dat, "scientificName")

    map_id_is_good <- tax_dat$acceptedNameUsageID %in% tax_dat$taxonID
    map_id_is_na <- is.na(tax_dat$acceptedNameUsageID)
    map_id_is_bad <- !map_id_is_na & !map_id_is_good
    assertthat::assert_that(
      sum(map_id_is_bad) == 0,
      msg = glue::glue(
        "`check_mapping` failed.
          `taxonID`(s) detected whose `acceptedNameUsageID` value does not \\
          map to `taxonID` of an existing name.
          Bad `taxonID`: {tax_dat$taxonID[map_id_is_bad]}
          Bad `scientificName`: {tax_dat$scientificName[map_id_is_bad]}"
      )
    )
  }

  # Check that all names have either accepted or synonym
  if (isTRUE(check_taxonomic_status)) {
    # check pre-reqs
    assertthat::assert_that(
      isTRUE(check_taxon_id),
      msg = "`check_taxonomic_status` requires `check_taxon_id` to be TRUE"
    )
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
    # Make sure all accepted names and synonyms are accounted for:
    # anti_join should result in zero rows
    tax_dat_accepted_check <-
      tax_dat |>
      dplyr::anti_join(
        dplyr::bind_rows(tax_dat_accepted, tax_dat_synonyms),
        by = "taxonID"
      )
    # Extract bad taxon IDs and species
    bad_taxon_id <- ""
    if (!is.null(tax_dat_accepted_check[["taxonID"]])) {
      bad_taxon_id <- paste(tax_dat_accepted_check$taxonID, collapse = ", ")
    }
    bad_taxon_species <- ""
    if (!is.null(tax_dat_accepted_check[["scientificName"]])) {
      bad_taxon_species <- paste(
        tax_dat_accepted_check$scientificName,
        collapse = ", "
      )
    }
    assertthat::assert_that(
      nrow(tax_dat_accepted_check) == 0,
      msg = glue::glue(
        "`check_taxonomic_status` failed.
         `taxonID`(s) detected whose `taxonomicStatus` is neither an \\
         accepted name nor synonym.
         Bad `taxonID`: {bad_taxon_id}
         Bad `scientificName`: {bad_taxon_species}"
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
          `taxonID`(s) detected whose `taxonomicStatus` scientific names \\
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
