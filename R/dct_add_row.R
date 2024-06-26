#' Add row(s) to a taxonomic database
#'
#' Add one or more rows to a taxonomic database in Darwin Core (DwC) format.
#'
#' `fill_taxon_id` and `fill_usage_id` only act on the newly added data (they
#' do not fill columns in `tax_dat`).
#'
#' If "taxonID" is not provided for the new row and `fill_taxon_id` is `TRUE`,
#' a value for taxonID will be automatically generated from the md5 hash digest
#' of the scientific name.
#'
#' To modify settings used for validation if `strict` is `TRUE`,
#' use `dct_options()`.
#'
#' @param tax_dat `r param_tax_dat`
#' @param taxonID Character or numeric vector; values to add to taxonID column.
#' Ignored if `new_dat` is not `NULL`.
#' @param scientificName Character vector; values to add to scientificName
#' column. Ignored if `new_dat` is not `NULL`.
#' @param taxonomicStatus Character vector; values to add to taxonomicStatus
#' column. Ignored if `new_dat` is not `NULL`.
#' @param acceptedNameUsageID Character or numeric vector; values to add to
#' acceptedNameUsageID column. Ignored if `new_dat` is not `NULL`.
#' @param acceptedNameUsage Character vector; values to add to acceptedNameUsage
#' column. Ignored if `new_dat` is not `NULL`.
#' @param new_dat A dataframe including columns corresponding to one or more of
#' the above arguments, except for `tax_dat`. Other DwC terms can also be
#' included as additional columns. All rows in `new_dat` will be appended to the
#' input data (`tax_dat`).
#' @param fill_taxon_id `r param_fill_taxon_id`
#' @param fill_usage_id `r param_fill_usage_id`
#' @param taxon_id_length `r param_taxon_id_length`
#' @param stamp_modified `r param_stamp_modified`
#' @param stamp_modified_by_id `r param_stamp_modified_by_id`
#' @param stamp_modified_by `r param_stamp_modified_by_name`
#' @param strict `r param_strict`
#' @param ... Additional data to add, specified as sets of named
#' character or numeric vectors; e.g., `parentNameUsageID = "6SH4"`. The name of
#' each must be a valid column name for data in DwC format. Ignored if `new_dat`
#' is not `NULL`.
#'
#' @return `r param_tax_dat`
#' @example inst/examples/dct_add_row.R
#' @autoglobal
#' @export
dct_add_row <- function(tax_dat,
                        taxonID = NULL, # nolint
                        scientificName = NULL, # nolint
                        taxonomicStatus = NULL, # nolint
                        acceptedNameUsageID = NULL, # nolint
                        acceptedNameUsage = NULL, # nolint
                        new_dat = NULL,
                        fill_taxon_id = dct_options()$fill_taxon_id,
                        fill_usage_id = dct_options()$fill_usage_id,
                        taxon_id_length = dct_options()$taxon_id_length,
                        stamp_modified = dct_options()$stamp_modified,
                        stamp_modified_by_id =
                          dct_options()$stamp_modified_by_id,
                        stamp_modified_by =
                          dct_options()$stamp_modified_by,
                        strict = dct_options()$strict,
                        ...) {
  # Create new_dat from direct input if provided
  if (is.null(new_dat)) {
    new_dat <- tibble::tibble(
      taxonID = taxonID,
      scientificName = scientificName,
      acceptedNameUsageID = acceptedNameUsageID,
      acceptedNameUsage = acceptedNameUsage,
      taxonomicStatus = taxonomicStatus,
      ...
    )
  }

  assertthat::assert_that(
    nrow(new_dat) > 0,
    msg = "Row cannot be added without specifying new data"
  )

  # Fill in taxonID for those missing
  if (isTRUE(fill_taxon_id)) {
    # in this case taxonID will be character
    if (!"taxonID" %in% colnames(new_dat)) {
      new_dat[["taxonID"]] <- NA_character_
    }
    if ("scientificName" %in% colnames(new_dat)) {
      assertthat::assert_that(assertthat::is.number(taxon_id_length))
      assertthat::assert_that(
        !is.null(taxon_id_length),
        msg = "taxon_id_length required to generate taxonID values"
      )
      assertthat::assert_that(taxon_id_length >= 1,
        msg = "taxon_id_length must be >= 1"
      )
      assertthat::assert_that(taxon_id_length <= 32,
        msg = "taxon_id_length must be <= 32"
      )
      taxon_id_length <- as.integer(taxon_id_length)
      new_dat <- dplyr::mutate(
        new_dat,
        taxonID = make_taxon_id_from_sci_name(
          taxonID, scientificName,
          max_len = taxon_id_length
        )
      )
    }
  }

  # Check on taxonID class, duplication between old and new data
  if ("taxonID" %in% colnames(new_dat) && "taxonID" %in% colnames(tax_dat)) {
    # - class
    if (class(new_dat$taxonID) != class(tax_dat$taxonID)) {
      new_dat <- dplyr::mutate(new_dat, taxonID = as.character(taxonID))
      tax_dat <- dplyr::mutate(tax_dat, taxonID = as.character(taxonID))
      warning(
        paste(
          "Class of taxonID column changed in either new_dat or tax_dat",
          "so new data could be added"
        ),
        call. = FALSE
      )
    }
    # - duplicated taxonID in new and old data
    new_tax_id <- new_dat$taxonID[!is.na(new_dat$taxonID)]
    old_tax_id <- tax_dat$taxonID[!is.na(tax_dat$taxonID)]
    if (length(new_tax_id) > 0 && length(old_tax_id) > 0) {
      assertthat::assert_that(
        !any(new_tax_id %in% old_tax_id),
        msg = "taxonID in new data must be different from that in existing data"
      )
    }
  }

  # Fill in acceptedUsageID in new_dat for those missing
  assertthat::assert_that(
    !(isTRUE(fill_usage_id) && !"taxonID" %in% colnames(tax_dat)),
    msg = "tax_dat must include column taxonID if fill_usage_id is TRUE"
  )
  if (isTRUE(fill_usage_id) &&
        "acceptedNameUsage" %in% colnames(new_dat) &&
        "scientificName" %in% colnames(tax_dat) &&
        "taxonID" %in% colnames(tax_dat)) {
    # Add "acceptedNameUsageID" as empty col if it does not yet exist
    if (!"acceptedNameUsageID" %in% colnames(new_dat)) {
      new_dat[["acceptedNameUsageID"]] <- rep(NA, nrow(new_dat))
    }
    # Lookup acceptedNameUsageID
    acc_name_usage_id_lookup <- dplyr::left_join(
      dplyr::select(new_dat, scientificName, acceptedNameUsage),
      dplyr::select(tax_dat, scientificName, acceptedNameUsageID = taxonID),
      na_matches = "never",
      by = c(acceptedNameUsage = "scientificName"),
      multiple = "all"
    )
    # Check for unique matches
    assertthat::assert_that(
      nrow(acc_name_usage_id_lookup) == nrow(new_dat),
      msg = paste(
        "fill_usage_id requires unique match between acceptedNameUsage and",
        "scientificName"
      )
    )
    # Copy values
    new_dat[["acceptedNameUsageID"]] <-
      acc_name_usage_id_lookup[["acceptedNameUsageID"]]
  }

  # Add modified timestamp
  if (isTRUE(stamp_modified)) {
    new_dat <- dplyr::mutate(
      new_dat,
      modified = as.character(Sys.time())
    )
  }

  # Add modifiedBy
  if (isTRUE(stamp_modified_by)) {
    assertthat::assert_that(
      isTRUE("modifiedBy" %in% dct_options()$extra_cols),
      msg = "stamp_modified_by requires 'modifiedBy' in extra_cols"
    )
    new_dat <- dplyr::mutate(
      new_dat,
      modifiedBy = dct_options()$user_name
    )
  }

  # Add modifiedByUserID
  if (isTRUE(stamp_modified_by_id)) {
    assertthat::assert_that(
      isTRUE("modifiedByID" %in% dct_options()$extra_cols),
      msg = "stamp_modified_by_id requires 'modifiedByID' in extra_cols"
    )
    new_dat <- dplyr::mutate(
      new_dat,
      modifiedByID = dct_options()$user_id
    )
  }

  # Check column names
  check_col_names_p(
    new_dat,
    on_fail = "error", on_success = "logical",
    run = TRUE,
    quiet = FALSE
  )

  # Add new data
  res <- dplyr::bind_rows(tax_dat, new_dat)

  # Validate
  if (strict) {
    dct_validate(res, on_success = "logical", on_fail = "error")
  }

  res
}
