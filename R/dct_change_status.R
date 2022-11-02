#' Change the taxonomic status of one entry in a taxonomic database
#'
#' Only one of `taxon_id` or `sci_name` needs to be entered. Either will
#' work if as long as it makes a partial or full match to one row in the data.
#'
#' `usage_id` or `usage_name` should be provided if the new status is a synonym;
#' either will work as it makes a partial or full match to one row in the data.
#'
#' @param other_terms Tibble of additional DWC terms to add to data;
#'   will over-write if any columns already exist
#' @inheritParams dct_change_status
#'
#' @return Dataframe; taxonomic database in Darwin Core format
#' @autoglobal
#' @noRd
dct_change_status_single <- function(tax_dat,
                                     taxon_id = NULL,
                                     sci_name = NULL,
                                     new_status,
                                     usage_id = NULL,
                                     usage_name = NULL,
                                     clear_usage_id = grepl(
                                      "accepted",
                                      new_status,
                                      ignore.case = TRUE),
                                     remap_variety = FALSE,
                                     stamp_modified = TRUE,
                                     strict = FALSE,
                                     quiet = FALSE,
                                     other_terms = NULL
                                     ) {

  # Convert any NA input (from args_tbl) to NULL
  if (!is.null(taxon_id) && is.na(taxon_id)) taxon_id <- NULL
  if (!is.null(sci_name) && is.na(sci_name)) sci_name <- NULL
  if (!is.null(usage_id) && is.na(usage_id)) usage_id <- NULL
  if (!is.null(usage_name) && is.na(usage_name)) usage_name <- NULL
  if (!is.null(other_terms) && isTRUE(is.na(other_terms))) other_terms <- NULL

  # Convert missing logicals to FALSE
  if (is.null(clear_usage_id) || is.na(clear_usage_id)) clear_usage_id <- FALSE
  if (is.null(remap_variety) || is.na(remap_variety)) remap_variety <- FALSE
  if (is.null(stamp_modified) || is.na(stamp_modified)) stamp_modified <- FALSE
  if (is.null(strict) || is.na(strict)) strict <- FALSE
  if (is.null(quiet) || is.na(quiet)) quiet <- FALSE

  # Check if other_terms overlaps with abbreviated terms and convert
  if (nrow(other_terms) > 0) {
    # - taxon_id
    assertthat::assert_that(
      sum(!is.null(taxon_id), "taxonID" %in% colnames(other_terms)) != 2,
      msg = "Must use either taxon_id or taxonID"
    )
    if ("taxonID" %in% colnames(other_terms)) {
      taxon_id <- other_terms$taxonID[[1]]
      other_terms$taxonID <- NULL
    }
    # - sci_name
    assertthat::assert_that(
      sum(!is.null(sci_name), "scientificName" %in% colnames(other_terms)) != 2,
      msg = "Must use either sci_name or scientificName"
    )
    if ("scientificName" %in% colnames(other_terms)) {
      sci_name <- other_terms$scientificName[[1]]
      other_terms$scientificName <- NULL
    }
    # - usage_id
    assertthat::assert_that(
      sum(
        !is.null(usage_id),
        "acceptedNameUsageID" %in% colnames(other_terms)
      ) != 2,
      msg = "Must use either usage_id or acceptedNameUsageID"
    )
    if ("acceptedNameUsageID" %in% colnames(other_terms)) {
      usage_id <- other_terms$acceptedNameUsageID[[1]]
      other_terms$acceptedNameUsageID <- NULL
    }
    # - usage_name
    assertthat::assert_that(
      sum(
        !is.null(usage_name),
        "acceptedNameUsage" %in% colnames(other_terms)
      ) != 2,
      msg = "Must use either usage_name or acceptedNameUsage"
    )
    if ("acceptedNameUsage" %in% colnames(other_terms)) {
      usage_name <- other_terms$acceptedNameUsage[[1]]
      other_terms$acceptedNameUsage <- NULL
    }
    # - new_status
    assertthat::assert_that(
      sum(
        !is.null(new_status),
        "taxonomicStatus" %in% colnames(other_terms)
      ) != 2,
      msg = "Must use either new_status or taxonomicStatus"
    )
    if ("taxonomicStatus" %in% colnames(other_terms)) {
      new_status <- other_terms$taxonomicStatus[[1]]
      other_terms$taxonomicStatus <- NULL
    }
  }

  # Assumptions:
  # - all taxonID are non-missing and unique
  # - either sci_name or taxon_id is provided, but not both
  # - either usage_name or usage_id is provided, but not both
  # - clear_usage_id is FALSE if either usage_id or usage_name are provided

  # Isolate row to change by sci_name or taxon_id
  if (!is.null(sci_name)) {
    tax_dat_row <- dplyr::filter(tax_dat, scientificName == sci_name)
  }
  if (!is.null(taxon_id)) {
    tax_dat_row <- dplyr::filter(tax_dat, taxonID == taxon_id)
  }
  assertthat::assert_that(nrow(tax_dat_row) == 1,
    msg = "Not exactly one row selected"
  )

  # Look up usage_id if usage_name provided
  if (!is.null(usage_name)) {
    usage_id <- tax_dat$taxonID[tax_dat$scientificName == usage_name]
    assertthat::assert_that(
      length(usage_id) == 1,
      msg = glue::glue(
        "Not exactly one scientificName matches usage_name '{usage_name}'"
      )
    )
  }

  # Create new row by modification
  new_row <- dplyr::mutate(tax_dat_row, taxonomicStatus = new_status)

  # - add acceptedNameUsageID
  if (!is.null(usage_id)) {
    new_row <- dplyr::mutate(new_row, acceptedNameUsageID = usage_id)
  }

  if (isTRUE(clear_usage_id)) {
    new_row <- dplyr::mutate(new_row, acceptedNameUsageID = NA)
  }

  # - add other DWC terms, overwriting existing values
  if (nrow(other_terms) > 0) {
    new_row <-
      new_row |>
      dplyr::select(-dplyr::any_of(colnames(other_terms))) |>
      dplyr::bind_cols(other_terms)
  }

  # - add timestamp
  if (isTRUE(stamp_modified)) {
    new_row <- dplyr::mutate(
      new_row, modified = as.character(Sys.time())
    )
  }

  # For change to synonym or variety (acceptedNameUsageID not NA),
  # check if other names will be affected
  new_row_other <- NULL
  if (!is.null(usage_id)) {
    if (isTRUE(remap_variety)) {
      new_row_other <- dplyr::filter(
        tax_dat, acceptedNameUsageID == tax_dat_row$taxonID
        )
    } else {
      new_row_other <- dplyr::filter(
        tax_dat,
        acceptedNameUsageID == tax_dat_row$taxonID,
        stringr::str_detect(taxonomicStatus, "variety", negate = TRUE)
        )
    }
    if (nrow(new_row_other) > 0) {
      if (isTRUE(stamp_modified)) {
        new_row_other <- dplyr::mutate(
          new_row_other,
          acceptedNameUsageID = usage_id,
          modified = as.character(Sys.time())
        )
      } else {
        new_row_other <- dplyr::mutate(
          new_row_other,
          acceptedNameUsageID = usage_id
        )
      }
    }
  }

  # Return input if update doesn't modify changes anything
  if (
    isTRUE(all.equal(tax_dat_row$taxonomicStatus, new_row$taxonomicStatus)) &&
      isTRUE(all.equal(
        as.character(tax_dat_row$acceptedNameUsageID),
        as.character(new_row$acceptedNameUsageID)
      ))
  ) {
    if (quiet == FALSE) {
      warning(
        glue::glue(
          "No change to taxonomicStatus or acceptedNameUsageID for selected \\
          row (taxonID {tax_dat_row$taxonID}); returning original input"
        )
      )
    }
    return(tax_dat)
  }

  # Remove selected row, add back in with modified taxonomic status
  res <- tax_dat |>
    dplyr::anti_join(new_row, by = "taxonID") |>
    dplyr::bind_rows(new_row)

  # Do same for other synonyms, if present
  if (!is.null(new_row_other)) {
    res <-
      res |>
      dplyr::anti_join(new_row_other, by = "taxonID") |>
      dplyr::bind_rows(new_row_other)
  }

  # Restore original order
  res <-
    tax_dat |>
    dplyr::select(taxonID) |>
    dplyr::inner_join(res, by = "taxonID")

  # Optionally run taxonomic database checks
  if (isTRUE(strict)) {
    res <- dct_validate(res)
  }

  res
}

#' Change the taxonomic status of data in a taxonomic database
#'
#' Arguments `taxon_id`, `sci_name`, `new_status`, `usage_id`, and `usage_name`
#' are provided as convenience to avoid typing "taxonomicID", "scientificName",
#' "taxonomicStatus", "acceptedNameUsageID", and "acceptedNameUsage",
#' respectively, but the latter also work.
#'
#' Only one of `taxon_id` or `sci_name` needs to be entered. Either will
#' work if as long as it makes a partial or full match to one row in the data.
#'
#' `usage_id` or `usage_name` should be provided if the new status is a synonym;
#' either will work as it makes a partial or full match to one row in the data.
#'
#' Can either modify a single row in the input taxonomic database if each
#' argument is supplied as a vector of length 1, or can apply a set of changes
#' to the taxonomic database if the input is supplied as a dataframe via
#' `args_tbl`.
#'
#' @param tax_dat Dataframe; taxonomic database in Darwin Core format.
#' @param taxon_id Character vector of length 1; taxonID for the entry to be
#' changed. Can also use "taxonomicID".
#' @param sci_name Character vector of length 1; scientificName for the entry to
#' be changed. Can also use "scientificName".
#' @param new_status Character vector of length 1; updated taxonomicStatus to
#' use for the entry. Can also use "taxonomicStatus"
#' @param usage_id Character vector of length 1; acceptedNameUsageID
#' (taxonID for accepted name) if new status is synonym. Can also use
#' "acceptedNameUsageID".
#' @param usage_name Character vector of length 1; scientificName for accepted
#' name if new status is synonym. Can also use "acceptedNameUsage".
#' @param clear_usage_id Logical vector of length 1; should
#' `acceptedNameUsageID` be set to `NA`?. Default: TRUE if `new_status` is
#' "accepted" (case insensitive).
#' @param remap_variety Logical vector of length 1; should the
#'  `acceptedNameUsageID` of names with `taxonomicStatus` of "variety"
#'   be updated (remapped) if they are varieties of the entry to be changed
#'   (i.e., if their `acceptedNameUsageID` is the same as the `taxonID` of the
#'   entry to be changed)?
#' @param stamp_modified Logical vector of length 1; should the `modified`
#' column of any modified row be changed to a timestamp with the date and time
#' of modification?
#' @param strict Logical vector of length 1; should taxonomic checks be run on
#' the updated taxonomic database?
#' @param quiet Logical vector of length 1; should warnings be silenced?
#' @param args_tbl A dataframe including columns corresponding to one or more of
#' the above arguments, except for `tax_dat`. In this case, the input taxonomic
#' database will be modified sequentially over each row of input in `args_tbl`.
#' Other Darwin Core terms can also be included as additional columns,
#' similar to using `...` to modify a single row.
#' @param ... other Darwin Core terms to modify, specified as sets of named
#'   values.
#'   Each element of the vector must have a name corresponding to a valid
#'   DWC term; see [dct_terms].
#'
#' @return Dataframe; taxonomic database in Darwin Core format
#' @autoglobal
#' @export
#' @examples
#' # Swap the accepted / synonym status of
#' # Cephalomanes crassum (Copel.) M. G. Price
#' # and Trichomanes crassum Copel.
#' dct_filmies |>
#'   dct_change_status(
#'     sci_name = "Cephalomanes crassum (Copel.) M. G. Price",
#'     new_status = "synonym",
#'     usage_name = "Trichomanes crassum Copel."
#'   ) |>
#'   dct_change_status(
#'     sci_name = "Trichomanes crassum Copel.",
#'     new_status = "accepted"
#'   ) |>
#'   dct_validate(
#'     check_tax_status = FALSE,
#'     check_mapping_strict = FALSE,
#'     check_sci_name = FALSE)
#'
#' # Sometimes changing one name will affect others, if they map
#' # to the new synonym
#' dct_change_status(
#'   tax_dat = dct_filmies |> head(),
#'   sci_name = "Cephalomanes crassum (Copel.) M. G. Price",
#'   new_status = "synonym",
#'   usage_name = "Cephalomanes densinervium (Copel.) Copel."
#' )
#'
#' # Apply a set of changes
#' library(tibble)
#' updates <- tibble(
#'   sci_name = c("Cephalomanes atrovirens Presl",
#'     "Cephalomanes crassum (Copel.) M. G. Price"),
#'   new_status = "synonym",
#'   usage_name = "Trichomanes crassum Copel."
#' )
#' dct_filmies |>
#'   dct_change_status(args_tbl = updates) |>
#'   dct_change_status(sci_name = "Trichomanes crassum Copel.",
#'     new_status = "accepted")
dct_change_status <- function(tax_dat,
                              taxon_id = NULL,
                              sci_name = NULL,
                              new_status = NULL,
                              usage_id = NULL,
                              usage_name = NULL,
                              clear_usage_id = grepl(
                                "accepted",
                                new_status,
                                ignore.case = TRUE
                              ),
                              remap_variety = FALSE,
                              stamp_modified = FALSE,
                              strict = FALSE,
                              quiet = FALSE,
                              args_tbl = NULL,
                              ...) {
  # Set default values ----
  # - when using args_tbl and others default,
  # clear_usage_id will be 0 length logical, so change to FALSE
  if (length(clear_usage_id) == 0 && !is.null(args_tbl)) {
    clear_usage_id <- FALSE
  }
  # Check input classes ----
  assertthat::assert_that(
    inherits(tax_dat, "data.frame"),
    msg = "tax_dat must be of class data.frame"
  )
  if (!is.null(taxon_id)) {
    assertthat::assert_that(
      assertthat::is.string(taxon_id) | assertthat::is.scalar(usage_id)
    )
  }
  if (!is.null(sci_name)) {
    assertthat::assert_that(assertthat::is.string(sci_name))
  }
  if (!is.null(new_status)) {
    assertthat::assert_that(assertthat::is.string(new_status))
  }
  if (!is.null(usage_id)) {
    assertthat::assert_that(
      assertthat::is.string(usage_id) | assertthat::is.scalar(usage_id)
    )
  }
  if (!is.null(usage_name)) {
    assertthat::assert_that(assertthat::is.string(usage_name))
  }
  assertthat::assert_that(assertthat::is.flag(clear_usage_id))
  assertthat::assert_that(assertthat::is.flag(strict))
  assertthat::assert_that(assertthat::is.flag(quiet))
  if (!is.null(args_tbl)) {
    assertthat::assert_that(
      inherits(args_tbl, "data.frame"),
      msg = "args_tbl must be of class data.frame"
    )
  }
  # Other input checks ----
  # - should use either taxon_id or sci_name
  assertthat::assert_that(
    sum(!is.null(taxon_id), !is.null(sci_name)) != 2,
    msg = "Must use either `taxon_id` or `sci_name`, not both"
  )
  # - should use either usage_id or usage_name
  assertthat::assert_that(
    sum(!is.null(usage_id), !is.null(usage_name)) != 2,
    msg = "Must use either `usage_id` or `usage_name`, not both"
  )
  # - taxonID must be non-missing and unique
  dct_check_taxon_id(tax_dat, on_fail = "error", on_success = "logical")
  # - clear_usage_id should not be FALSE if usage_id or usage_name are
  # provided
  if (isTRUE(clear_usage_id)) {
    assertthat::assert_that(
      is.null(usage_id) && is.null(usage_name),
      msg =
        "clear_usage_id cannot be TRUE if usage_id or usage_name is non-NULL"
    )
  }
  # -usage_name must exist in input data
  if (!is.null(usage_name)) {
    assertthat::assert_that(
      usage_name %in% tax_dat$scientificName,
      msg = "`usage_name` not detected in `tax_dat$scientificName`"
    )
  }
  # -usage_id must exist in input data
  if (!is.null(usage_id)) {
    assertthat::assert_that(
      usage_id %in% tax_dat$taxonID,
      msg = "`usage_id` not detected in `tax_dat$taxonID`"
    )
  }
  std_args <- c(
    "tax_dat", "taxon_id", "sci_name", "new_status",
    "usage_id", "usage_name", "other_terms", "clear_usage_id", "remap_variety",
    "stamp_modified", "strict"
  )
  # If input is args_tbl, loop over tax_dat, using the previous output of each
  # iteration as input into the next iteration
  if (!is.null(args_tbl)) {
    for (i in seq_len(nrow(args_tbl))) {
      tax_dat <- dct_change_status_single(
        tax_dat,
        taxon_id = val_if_in_dat(args_tbl, "taxon_id", i),
        sci_name = val_if_in_dat(args_tbl, "sci_name", i),
        new_status = val_if_in_dat(args_tbl, "new_status", i),
        usage_id = val_if_in_dat(args_tbl, "usage_id", i),
        usage_name = val_if_in_dat(args_tbl, "usage_name", i),
        clear_usage_id = val_if_in_dat(args_tbl, "clear_usage_id", i),
        remap_variety = val_if_in_dat(args_tbl, "remap_variety", i),
        stamp_modified = val_if_in_dat(args_tbl, "stamp_modified", i),
        strict = val_if_in_dat(args_tbl, "strict", i),
        quiet = quiet,
        other_terms = args_tbl[i, !colnames(args_tbl) %in% std_args]
      )
    }

    return(tax_dat)
  }

  # Otherwise, run dct_change_status_single()
  dct_change_status_single(
    tax_dat,
    taxon_id = taxon_id,
    sci_name = sci_name,
    new_status = new_status,
    usage_id = usage_id,
    usage_name = usage_name,
    clear_usage_id = clear_usage_id,
    remap_variety = remap_variety,
    stamp_modified = stamp_modified,
    strict = strict,
    quiet = quiet,
    other_terms = tibble::tibble(...)
  )
}
