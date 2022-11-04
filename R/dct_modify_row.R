#' Modify one row of a taxonomic database
#'
#' @param other_terms Tibble of additional DWC terms to add to data;
#'   will over-write if any columns already exist
#' @inheritParams dct_modify_row
#'
#' @return Dataframe; taxonomic database in Darwin Core format
#' @autoglobal
#' @noRd
dct_modify_row_single <- function(tax_dat,
                                  taxon_id = NULL,
                                  sci_name = NULL,
                                  tax_status = NULL,
                                  usage_id = NULL,
                                  usage_name = NULL,
                                  clear_usage_id = FALSE,
                                  fill_usage_name = FALSE,
                                  remap_names = TRUE,
                                  remap_variant = FALSE,
                                  stamp_modified = TRUE,
                                  strict = FALSE,
                                  quiet = FALSE,
                                  other_terms = NULL
                                  ) {

  # Convert any NA input (from args_tbl) to NULL
  if (!is.null(taxon_id) && is.na(taxon_id)) taxon_id <- NULL
  if (!is.null(sci_name) && is.na(sci_name)) sci_name <- NULL
  if (!is.null(tax_status) && is.na(tax_status)) tax_status <- NULL
  if (!is.null(usage_id) && is.na(usage_id)) usage_id <- NULL
  if (!is.null(usage_name) && is.na(usage_name)) usage_name <- NULL
  if (!is.null(other_terms) && isTRUE(is.na(other_terms))) other_terms <- NULL

  # Set clear_usage_id to TRUE if tax_status is "accepted" and clear_usage_id
  # not set
  if (!is.null(tax_status) && is.null(clear_usage_id)) {
    clear_usage_id <- grepl("accepted", tax_status, ignore.case = TRUE)
  }
  # Convert missing logicals to FALSE
  if (is.null(clear_usage_id) || is.na(clear_usage_id)) clear_usage_id <- FALSE
  if (is.null(fill_usage_name) || is.na(fill_usage_name)) fill_usage_name <-
    FALSE
  if (is.null(remap_names) || is.na(remap_names)) remap_names <- FALSE
  if (is.null(remap_variant) || is.na(remap_variant)) remap_variant <- FALSE
  if (is.null(stamp_modified) || is.na(stamp_modified)) stamp_modified <- FALSE
  if (is.null(strict) || is.na(strict)) strict <- FALSE
  if (is.null(quiet) || is.na(quiet)) quiet <- FALSE

  # Check input classes ----
  if (!is.null(taxon_id)) {
    assertthat::assert_that(
      assertthat::is.string(taxon_id) | assertthat::is.scalar(usage_id)
    )
  }
  if (!is.null(sci_name)) {
    assertthat::assert_that(assertthat::is.string(sci_name))
  }
  if (!is.null(tax_status)) {
    assertthat::assert_that(assertthat::is.string(tax_status))
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
  assertthat::assert_that(assertthat::is.flag(fill_usage_name))
  assertthat::assert_that(assertthat::is.flag(remap_names))
  assertthat::assert_that(assertthat::is.flag(remap_variant))
  assertthat::assert_that(assertthat::is.flag(stamp_modified))
  assertthat::assert_that(assertthat::is.flag(strict))
  assertthat::assert_that(assertthat::is.flag(quiet))

  # Other input checks ----
  # - clear_usage_id should not be FALSE if usage_id or usage_name are
  # provided
  if (isTRUE(clear_usage_id)) {
    assertthat::assert_that(
      is.null(usage_id) && is.null(usage_name),
      msg =
        "clear_usage_id cannot be TRUE if usage_id or usage_name is non-NULL"
    )
  }
  # - usage_name must exist in input data
  if (!is.null(usage_name)) {
    assertthat::assert_that(
      usage_name %in% tax_dat$scientificName,
      msg = "`usage_name` not detected in `tax_dat$scientificName`"
    )
  }
  # - usage_id must exist in input data
  if (!is.null(usage_id)) {
    assertthat::assert_that(
      usage_id %in% tax_dat$taxonID,
      msg = "`usage_id` not detected in `tax_dat$taxonID`"
    )
  }
  # - clear_usage_id and fill_usage_name cannot both be TRUE
  assertthat::assert_that(
    sum(c(fill_usage_name, clear_usage_id)) < 2,
    msg = "fill_usage_name and clear_usage_id cannot both be TRUE"
  )

  # Check if other_terms overlaps with abbreviated terms and convert
  if (!is.null(other_terms)) {
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
        sum(
          !is.null(sci_name), "scientificName" %in% colnames(other_terms)) != 2,
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
      # - tax_status
      assertthat::assert_that(
        sum(
          !is.null(tax_status),
          "taxonomicStatus" %in% colnames(other_terms)
        ) != 2,
        msg = "Must use either tax_status or taxonomicStatus"
      )
      if ("taxonomicStatus" %in% colnames(other_terms)) {
        tax_status <- other_terms$taxonomicStatus[[1]]
        other_terms$taxonomicStatus <- NULL
      }
    }
  }

  # Isolate row to change by sci_name or taxon_id
  assertthat::assert_that(
    !(is.null(sci_name) && is.null(taxon_id)),
    msg = "Must provide one or both of sci_name and taxon_id"
  )
  if (!is.null(taxon_id)) {
    tax_dat_row <- dplyr::filter(tax_dat, taxonID == taxon_id)
  }
  if (is.null(taxon_id) && !is.null(sci_name)) {
    tax_dat_row <- dplyr::filter(tax_dat, scientificName == sci_name)
    assertthat::assert_that(nrow(tax_dat_row) == 1,
      msg = glue::glue(
        "Not exactly one scientificName matches sci_name '{sci_name}'"
      )
    )
  }

  # Look up usage_id if usage_name provided
  if (!is.null(usage_name)) {
    usage_id_match <- tax_dat$taxonID[tax_dat$scientificName == usage_name]
    assertthat::assert_that(
      length(usage_id_match) == 1,
      msg = glue::glue(
        "Not exactly one scientificName matches usage_name '{usage_name}'"
      )
    )
    # If both usage_id and usage_name provided, they must agree
    if (!is.null(usage_id)) {
      assertthat::assert_that(
        usage_id_match == usage_id,
        msg = glue::glue(
          "usage_id and usage_name do not agree for usage_name '{usage_name}'"
        )
      )
    }
    usage_id <- usage_id_match
  }

  # Create new row by modification
  new_row <- tax_dat_row
  # - modify taxonomicStatus
  if (!is.null(tax_status)) {
    new_row <- dplyr::mutate(new_row, taxonomicStatus = tax_status)
  }
  # - modify scientificName only if both taxon_id and sci_name provided
  if (!is.null(sci_name) && !is.null(taxon_id)) {
    new_row <- dplyr::mutate(new_row, scientificName = sci_name)
  }
  # - modify or clear acceptedNameUsageID
  if (!is.null(usage_id)) {
    new_row <- dplyr::mutate(new_row, acceptedNameUsageID = usage_id)
  }
  if (isTRUE(clear_usage_id)) {
    if (inherits(tax_dat_row$acceptedNameUsageID, "character")) {
      new_row <- dplyr::mutate(new_row, acceptedNameUsageID = NA_character_)
    }
    if (inherits(tax_dat_row$acceptedNameUsageID, "numeric")) {
      new_row <- dplyr::mutate(new_row, acceptedNameUsageID = NaN)
    }
  }
  # - fill usage name
  if (isTRUE(fill_usage_name)) {
    usage_name_match <- tax_dat$scientificName[tax_dat$taxonID == usage_id]
    assertthat::assert_that(
      length(usage_name_match) == 1,
      msg = glue::glue(
        "Not exactly one scientificName matches acceptedNameUsageID \\
        '{usage_id}'"
      )
    )
    new_row <- dplyr::mutate(new_row, acceptedNameUsage = usage_name_match)
  }
  # - add other DWC terms, overwriting existing values
  if (!is.null(other_terms)) {
    if (nrow(other_terms) > 0) {
      new_row <-
        new_row |>
        dplyr::select(-dplyr::any_of(colnames(other_terms))) |>
        dplyr::bind_cols(other_terms)
    }
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
  if (!is.null(usage_id) && isTRUE(remap_names)) {
    # - find other rows affected
    if (isTRUE(remap_variant)) {
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
    # - update other rows affected
    if (nrow(new_row_other) > 0) {
      new_row_other <- dplyr::mutate(
        new_row_other,
        acceptedNameUsageID = usage_id
      )
      if (isTRUE(fill_usage_name)) {
        new_row_other <- dplyr::mutate(
          new_row_other,
          acceptedNameUsage = usage_name_match
        )
      }
      if (isTRUE(stamp_modified)) {
        new_row_other <- dplyr::mutate(
          new_row_other,
          modified = as.character(Sys.time())
        )
      }
    }
  }

  # Return input if update doesn't modify changes anything
  if (isTRUE(all.equal(tax_dat_row, new_row))) {
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
  if (!is.null(new_row_other) && isTRUE(nrow(new_row_other) > 0)) {
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

#' Modify row(s) of a taxonomic database
#'
#' Modify one or more rows in a taxonomic database in Darwin Core (DWC) format.
#'
#' Arguments `taxon_id`, `sci_name`, `tax_status`, `usage_id`, and `usage_name`
#' are provided as convenience to avoid typing the longer "taxonomicID",
#' "scientificName", "taxonomicStatus", "acceptedNameUsageID", and
#' "acceptedNameUsage", respectively, but the latter may be used instead.
#'
#' `taxon_id` is only used to identify the row(s) to modify and is not itself
#' modified. `sci_name` can be used in the same way if `taxon_id` is not
#' provided (as long as `sci_name` matches a single row). If both `taxon_id` and
#' `sci_name` are provided, `sci_name` will be assigned to the scientificName of
#' the row identified by `taxon_id`, replacing any value that already exists.
#'
#' `tax_status`, `usage_id`, `usage_name`, and any other arguments provided that
#' are DWC terms will be assigned to the selected row (i.e., they will
#' modify the row).
#'
#' If `remap_names` is `TRUE` (default) and `usage_id` (i.e.,
#' acceptedNameUsageID) is provided, any names that have an acceptedNameUsageID
#' matching the taxonID of the selected row (i.e., synonyms of that row) will
#' also have their acceptedNameUsageID replaced with the new
#' acceptedNameUsageID. This behavior is not applied to names with
#' taxonomicStatus of "variant" by default, but can be turned on for such names
#' with `remap_variant`.
#'
#' Can either modify a single row in the input taxonomic database if each
#' argument is supplied as a vector of length 1, or can apply a set of changes
#' to the taxonomic database if the input is supplied as a dataframe via
#' `args_tbl`.
#'
#' @param tax_dat Dataframe; taxonomic database in DWC format.
#' @param taxon_id Character or numeric vector of length 1; taxonID of the row
#' to be modified (the selected row). Can also use `taxonomicID`.
#' @param sci_name Character vector of length 1; scientificName of the row to
#' be modified if `taxon_id` is `NULL`, OR the scientificName to assign to the
#' selected row if `taxon_id` is provided (see Details).
#' Can also use `scientificName`.
#' @param tax_status Character vector of length 1; taxonomicStatus to
#' assign to the selected row. Can also use `taxonomicStatus`.
#' @param usage_id Character or numeric vector of length 1; acceptedNameUsageID
#' to assign to the selected row. Can also use `acceptedNameUsageID`
#' @param usage_name Character vector of length 1; acceptedNameUsage to assign
#' to the selected row. Can also use `acceptedNameUsage`.
#' @param clear_usage_id Logical vector of length 1; should
#' acceptedNameUsageID of the selected row be set to `NA`? Default: `TRUE` if
#' `tax_status` matches "accepted" (case insensitive).
#' @param fill_usage_name Logical vector of length 1; should the
#' acceptedNameUsage of the selected row be set to the
#' scientificName corresponding to its acceptedNameUsageID? Default `FALSE`.
#' @param remap_names Logical vector of length 1; should the
#' acceptedNameUsageID be updated (remapped) for rows with the same
#' acceptedNameUsageID as the taxonID of the row to be modified? Default `TRUE`.
#' @param remap_variant Same as `remap_names`, but applies specifically to
#' rows with taxonomicStatus of "variant". Default `FALSE`.
#' @param stamp_modified Logical vector of length 1; should the `modified`
#' column of any modified row be assigned a timestamp with the date and time
#' of modification?
#' @param strict Logical vector of length 1; should taxonomic checks be run on
#' the updated taxonomic database?
#' @param quiet Logical vector of length 1; should warnings be silenced?
#' @param args_tbl A dataframe including columns corresponding to one or more of
#' the above arguments, except for `tax_dat`. In this case, the input taxonomic
#' database will be modified sequentially over each row of input in `args_tbl`.
#' Other DWC terms can also be included as additional columns,
#' similar to using `...` to modify a single row.
#' @param ... other DWC terms to modify, specified as sets of named values.
#' Each element of the vector must have a name corresponding to a valid
#' DWC term; see [dct_terms].
#'
#' @return Dataframe; taxonomic database in DWC format
#' @autoglobal
#' @export
#' @examples
#' # Swap the accepted / synonym status of
#' # Cephalomanes crassum (Copel.) M. G. Price
#' # and Trichomanes crassum Copel.
#' dct_filmies |>
#'   dct_modify_row(
#'     sci_name = "Cephalomanes crassum (Copel.) M. G. Price",
#'     tax_status = "synonym",
#'     usage_name = "Trichomanes crassum Copel."
#'   ) |>
#'   dct_modify_row(
#'     sci_name = "Trichomanes crassum Copel.",
#'     tax_status = "accepted"
#'   ) |>
#'   dct_validate(
#'     check_tax_status = FALSE,
#'     check_mapping_strict = FALSE,
#'     check_sci_name = FALSE)
#'
#' # Sometimes changing one name will affect others, if they map
#' # to the new synonym
#' dct_modify_row(
#'   tax_dat = dct_filmies |> head(),
#'   sci_name = "Cephalomanes crassum (Copel.) M. G. Price",
#'   tax_status = "synonym",
#'   usage_name = "Cephalomanes densinervium (Copel.) Copel."
#' )
#'
#' # Apply a set of changes
#' library(tibble)
#' updates <- tibble(
#'   sci_name = c("Cephalomanes atrovirens Presl",
#'     "Cephalomanes crassum (Copel.) M. G. Price"),
#'   tax_status = "synonym",
#'   usage_name = "Trichomanes crassum Copel."
#' )
#' dct_filmies |>
#'   dct_modify_row(args_tbl = updates) |>
#'   dct_modify_row(sci_name = "Trichomanes crassum Copel.",
#'     tax_status = "accepted")
dct_modify_row <- function(tax_dat,
                           taxon_id = NULL,
                           sci_name = NULL,
                           tax_status = NULL,
                           usage_id = NULL,
                           usage_name = NULL,
                           clear_usage_id = NULL,
                           fill_usage_name = FALSE,
                           remap_names = TRUE,
                           remap_variant = FALSE,
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

  # Check input ----
  # - tax_dat must be dataframe
  assertthat::assert_that(
    inherits(tax_dat, "data.frame"),
    msg = "tax_dat must be of class data.frame"
  )
  # - args_tbl must be dataframe
  if (!is.null(args_tbl)) {
    assertthat::assert_that(
      inherits(args_tbl, "data.frame"),
      msg = "args_tbl must be of class data.frame"
    )
  }
  # - taxonID of tax_dat must be non-missing and unique
  dct_check_taxon_id(tax_dat, on_fail = "error", on_success = "logical")
  # - others checked at level of dct_modify_row_single()

  # define standard arguments to exclude from other_terms
  std_args <- c(
    "tax_dat", "taxon_id", "sci_name", "tax_status",
    "usage_id", "usage_name", "clear_usage_id", "fill_usage_name",
    "remap_names", "remap_variant",
    "stamp_modified", "strict", "quiet"
  )

  # If input is args_tbl, loop over tax_dat, using the previous output of each
  # iteration as input into the next iteration
  if (!is.null(args_tbl)) {
    for (i in seq_len(nrow(args_tbl))) {
      tax_dat <- dct_modify_row_single(
        tax_dat,
        taxon_id = val_if_in_dat(args_tbl, "taxon_id", i),
        sci_name = val_if_in_dat(args_tbl, "sci_name", i),
        tax_status = val_if_in_dat(args_tbl, "tax_status", i),
        usage_id = val_if_in_dat(args_tbl, "usage_id", i),
        usage_name = val_if_in_dat(args_tbl, "usage_name", i),
        clear_usage_id = val_if_in_dat(args_tbl, "clear_usage_id", i),
        fill_usage_name = val_if_in_dat(args_tbl, "fill_usage_name", i),
        remap_names = val_if_in_dat(args_tbl, "remap_names", i),
        remap_variant = val_if_in_dat(args_tbl, "remap_variant", i),
        stamp_modified = val_if_in_dat(args_tbl, "stamp_modified", i),
        strict = val_if_in_dat(args_tbl, "strict", i),
        quiet = quiet,
        other_terms = args_tbl[i, !colnames(args_tbl) %in% std_args]
      )
    }
    return(tax_dat)
  }

  # Otherwise, run dct_modify_row_single()
  dct_modify_row_single(
    tax_dat,
    taxon_id = taxon_id,
    sci_name = sci_name,
    tax_status = tax_status,
    usage_id = usage_id,
    usage_name = usage_name,
    clear_usage_id = clear_usage_id,
    fill_usage_name = fill_usage_name,
    remap_names = remap_names,
    remap_variant = remap_variant,
    stamp_modified = stamp_modified,
    strict = strict,
    quiet = quiet,
    other_terms = tibble::tibble(...)
  )
}
