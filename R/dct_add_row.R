#' Add only one entry to a taxonomic database
#'
#' @param other_terms Tibble of additional DWC terms to add to data
#' @return Dataframe; taxonomic database in Darwin Core format
#' @inheritParams dct_add_row
#' @autoglobal
#' @noRd
dct_add_row_single <- function(tax_dat,
                               taxon_id = NULL,
                               sci_name,
                               strict = FALSE,
                               other_terms = NULL) {
  # Check input
  assertthat::assert_that(assertthat::is.string(sci_name))
  if (!is.null(taxon_id)) {
    assertthat::assert_that(assertthat::is.string(taxon_id))
  }
  assertthat::assert_that(assertthat::is.flag(strict))

  # Assign default taxon_id as hash of sci name
  if (is.null(taxon_id)) taxon_id <- digest::digest(sci_name)

  # Create new row to add
  new_row <- tibble::tibble(
    taxonID = taxon_id,
    scientificName = sci_name,
    modified = as.character(Sys.time())
  )

  # Add other data
  if (nrow(other_terms) > 0) {
    # Run checks on other data
    dct_validate(
      other_terms,
      check_taxon_id = FALSE,
      check_mapping = FALSE,
      check_mapping_strict = FALSE,
      check_sci_name = FALSE,
      check_tax_status = FALSE,
      check_status_diff = FALSE,
      check_col_names = TRUE
    )

    assertthat::assert_that(
      nrow(other_terms) == 1,
      msg = "Only one row of additional data may be added"
    )

    assertthat::assert_that(
      !any(colnames(other_terms) %in% c(
        "taxonID", "scientificName"
      )),
      msg = paste(
        "Names of additional data may not include 'taxonID' or",
        "'scientificName'"
      )
    )

    new_row <- dplyr::bind_cols(new_row, other_terms)
  }

  # Check for duplicated ID
  overlap_id <- dplyr::inner_join(tax_dat, new_row, by = "taxonID")
  assertthat::assert_that(nrow(overlap_id) == 0,
    msg = "`tax_dat` already contains that `taxonID`"
  )

  # Add new row
  res <- dplyr::bind_rows(tax_dat, new_row)

  # Optionally run taxonomic database checks
  if (isTRUE(strict)) dct_validate(res)

  res
}

#' Add row(s) to a taxonomic database
#'
#' Add one or more rows to a taxonomic database in Darwin Core (DWC) format.
#'
#' @param tax_dat Dataframe; taxonomic database in DWC format
#' @param taxon_id taxonID to use for new row; optional, will be
#' assigned automatically if not provided.
#' @param sci_name scientificName to use for new row
#' @param strict Logical; should taxonomic checks be run on the updated
#' taxonomic database?
#' @param args_tbl A dataframe with columns that are named by DWC terms.
#'   All rows will be added at once.
#' @param ... Additional data to include, specified as sets of named
#' character strings; e.g., `parentNameUsageID = "6SH4"`. The name of
#' each string must be a valid column name for data in DWC format.
#'
#' @return Dataframe; taxonomic database in DWC format
#' @examples
#' tibble::tibble(
#'   taxonID = "123",
#'   scientificName = "Foogenus barspecies",
#'   acceptedNameUsageID = NA_character_,
#'   taxonomicStatus = "accepted"
#' ) |>
#'   dct_add_row(
#'     sci_name = "Foogenus barspecies var. bla",
#'     parentNameUsageID = "123",
#'     nameAccordingTo = "me",
#'     strict = TRUE
#'   )
#' @autoglobal
#' @export
dct_add_row <- function(tax_dat,
                        taxon_id = NULL,
                        sci_name = NULL,
                        usage_id = NULL,
                        usage_name = NULL,
                        tax_status = NULL,
                        fill_taxon_id = TRUE,
                        fill_usage_id = TRUE,
                        fill_parent_name = FALSE,
                        fill_parent_id = TRUE,
                        stamp_modified = TRUE,
                        strict = FALSE,
                        new_dat = NULL,
                        ...) {
  # Check if new_dat overlaps with abbreviated terms and convert
  if (!is.null(new_dat)) {
    # - taxon_id
    assertthat::assert_that(
      sum(!is.null(taxon_id), "taxonID" %in% colnames(new_dat)) != 2,
      msg = "Must use either taxon_id or taxonID"
    )
    new_dat <- convert_col(new_dat, "taxonID", "taxon_id")
    # - sci_name
    assertthat::assert_that(
      sum(
        !is.null(sci_name), "scientificName" %in% colnames(new_dat)
      ) != 2,
      msg = "Must use either sci_name or scientificName"
    )
    new_dat <- convert_col(new_dat, "scientificName", "sci_name")
    # - usage_id
    assertthat::assert_that(
      sum(
        !is.null(usage_id),
        "acceptedNameUsageID" %in% colnames(new_dat)
      ) != 2,
      msg = "Must use either usage_id or acceptedNameUsageID"
    )
    new_dat <- convert_col(new_dat, "acceptedNameUsageID", "usage_id")
    # - usage_name
    assertthat::assert_that(
      sum(
        !is.null(usage_name),
        "acceptedNameUsage" %in% colnames(new_dat)
      ) != 2,
      msg = "Must use either usage_name or acceptedNameUsage"
    )
    new_dat <- convert_col(new_dat, "acceptedNameUsage", "usage_name")
    # - tax_status
    assertthat::assert_that(
      sum(
        !is.null(tax_status),
        "taxonomicStatus" %in% colnames(new_dat)
      ) != 2,
      msg = "Must use either tax_status or taxonomicStatus"
    )
    new_dat <- convert_col(new_dat, "taxonomicStatus", "tax_status")
  }

  # or create new_dat from direct input
  if (is.null(new_dat)) {
    tryCatch(
      expr = {
        new_dat <- tibble::tibble(
          taxonID = taxon_id,
          scientificName = sci_name,
          acceptedNameUsageID = usage_id,
          acceptedNameUsage = usage_name,
          taxonomicStatus = tax_status,
          ...
        )
      },
      # Provide a more useful error msg than tibble default for
      # duplicated column names
      error = function(e) {
        err_msg <- as.character(e)
        if (grepl("Column name .* must not be duplicated", err_msg)) {
          dup_col <- stringr::str_match(
            err_msg, "Column name `(.*)` must not be duplicated"
          )[, 2]
          stop(
            glue::glue("Duplicate column name detected: {dup_col}
            Did you try to use both the shortened and full version \\
            of a DWC term?")
          )
        } else {
          stop(e)
        }
      }
    )
  }

  # fill in taxonID for those missing
  if (isTRUE(fill_taxon_id)) {
    if (!"taxonID" %in% colnames(new_dat)) {
      new_dat[["taxonID"]] <- NA_character_
    }
    if ("scientificName" %in% colnames(new_dat)) {
      new_dat <- dplyr::mutate(
        new_dat,
        taxonID = make_taxon_id_from_sci_name(taxonID, scientificName)
      )
    }
  }

  if ("taxonID" %in% colnames(new_dat) && "taxonID" %in% colnames(tax_dat)) {
    if (class(new_dat$taxonID) != class(tax_dat$taxonID)) {
      new_dat <- dplyr::mutate(new_dat, taxonID = as.character(taxonID))
      tax_dat <- dplyr::mutate(tax_dat, taxonID = as.character(taxonID))
      warning(
        paste(
          "Class of taxonID column changed in either new_dat or tax_dat",
          "so new data could be added"
        )
      )
    }
  }

  # fill in acceptedUsageID for those missing

  dplyr::bind_rows(tax_dat, new_dat)
}
