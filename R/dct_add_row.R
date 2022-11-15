#' Add row(s) to a taxonomic database
#'
#' Add one or more rows to a taxonomic database in Darwin Core (DWC) format.
#'
#' Arguments `taxon_id`, `sci_name`, `tax_status`, `usage_id`, and `usage_name`
#' are provided as convenience to avoid typing the longer "taxonomicID",
#' "scientificName", "taxonomicStatus", "acceptedNameUsageID", and
#' "acceptedNameUsage", respectively, but the latter may be used instead.
#'
#' `fill_taxon_id` and `fill_usage_id` only act on the newly added data (they
#' do not fill columns in `tax_dat`).
#'
#' @param tax_dat `r param_tax_dat`
#' @param taxon_id Character or numeric vector; values to add to taxonID column.
#' Can also use `taxonomicID`. Ignored if `new_dat` is not `NULL`.
#' @param sci_name Character vector; values to add to scientificName column. Can
#' also use `scientificName`. Ignored if `new_dat` is not `NULL`.
#' @param tax_status Character vector; values to add to taxonomicStatus column.
#' Can also use `taxonomicStatus`. Ignored if `new_dat` is not `NULL`.
#' @param usage_id Character or numeric vector; values to add to
#' acceptedNameUsageID column. Can also use `acceptedNameUsageID`. Ignored if
#' `new_dat` is not `NULL`.
#' @param usage_name Character vector; values to add to acceptedNameUsage
#' column. Can also use `acceptedNameUsage`. Ignored if `new_dat` is not `NULL`.
#' @param new_dat A dataframe including columns corresponding to one or more of
#' the above arguments, except for `tax_dat`. Other DWC terms can also be
#' included as additional columns. All rows in `new_dat` will be appended to the
#' input data (`tax_dat`).
#' @param fill_taxon_id Logical vector of length 1; if `taxon_id` is not
#' provided, should values in the taxonID column be filled in by looking
#' them up from the scientificName? Default `TRUE`.
#' @param fill_usage_id Logical vector of length 1; if `usage_id` is not
#' provided, should values in the acceptedNameUsageID column be filled in by
#' matching acceptedNameUsage to scientificName? Default `TRUE`.
#' @param stamp_modified Logical vector of length 1; should the `modified`
#' column of any new row include a a timestamp with the date and time of its
#' creation?
#' @param strict Logical vector of length 1; should taxonomic checks be run on
#' the updated taxonomic database? Default `FALSE`.
#' @param ... Additional data to add, specified as sets of named
#' character or numeric vectors; e.g., `parentNameUsageID = "6SH4"`. The name of
#' each must be a valid column name for data in DWC format. Ignored if `new_dat`
#' is not `NULL`.
#'
#' @return `r param_tax_dat`
#' @example inst/examples/dct_add_row.R
#' @autoglobal
#' @export
dct_add_row <- function(tax_dat,
                        taxon_id = NULL,
                        sci_name = NULL,
                        tax_status = NULL,
                        usage_id = NULL,
                        usage_name = NULL,
                        new_dat = NULL,
                        fill_taxon_id = TRUE,
                        fill_usage_id = TRUE,
                        stamp_modified = TRUE,
                        strict = FALSE,
                        ...) {
  # Check if new_dat overlaps with abbreviated terms and convert
  if (!is.null(new_dat)) {
    # - taxon_id
    assert_that_uses_one_name(new_dat, "taxon_id", "taxonID")
    new_dat <- convert_col(new_dat, "taxonID", "taxon_id")
    # - sci_name
    assert_that_uses_one_name(new_dat, "scientificName", "sci_name")
    new_dat <- convert_col(new_dat, "scientificName", "sci_name")
    # - usage_id
    assert_that_uses_one_name(new_dat, "acceptedNameUsageID", "usage_id")
    new_dat <- convert_col(new_dat, "acceptedNameUsageID", "usage_id")
    # - usage_name
    assert_that_uses_one_name(new_dat, "acceptedNameUsage", "usage_name")
    new_dat <- convert_col(new_dat, "acceptedNameUsage", "usage_name")
    # - tax_status
    assert_that_uses_one_name(new_dat, "taxonomicStatus", "tax_status")
    new_dat <- convert_col(new_dat, "taxonomicStatus", "tax_status")
    # TODO Check that all columns are DWC terms
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

  # Fill in taxonID for those missing
  if (isTRUE(fill_taxon_id)) {
    # in this case taxonID will be character
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
        )
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
  if (isTRUE(fill_usage_id) &&
    "acceptedNameUsage" %in% colnames(new_dat) &&
    "acceptedNameUsageID" %in% colnames(tax_dat) &&
    "scientificName" %in% colnames(tax_dat) &&
    "taxonID" %in% colnames(tax_dat)) {
    # Add "acceptedNameUsageID" as empty col if it does not yet exist
    if (!"acceptedNameUsageID" %in% colnames(new_dat)) {
      new_dat[["acceptedNameUsageID"]] <- rep(NA, nrow(new_dat))
    }
    # Check that all tax dat sci names are unique
    assertthat::assert_that(
      is_unique(tax_dat[["scientificName"]]),
      msg =
        "fill_usage_id requires values of tax_dat$scientificName to be unique"
    )
    # Lookup location of values to copy
    loc <- match(
      new_dat[["acceptedNameUsage"]],
      tax_dat[["scientificName"]]
    )
    # Copy values
    new_dat[["acceptedNameUsageID"]] <- dplyr::coalesce(
      tax_dat[["acceptedNameUsageID"]],
      tax_dat[["taxonID"]][loc]
    )
  }

  # Add timestamp
  if (isTRUE(stamp_modified)) {
    new_dat <- dplyr::mutate(
      new_dat,
      modified = as.character(Sys.time())
    )
  }

  # Add new data
  res <- dplyr::bind_rows(tax_dat, new_dat)

  # Validate
  if (strict) {
    dct_validate(res, on_success = "logical", on_fail = "error")
  }

  res
}
