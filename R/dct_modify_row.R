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
                                  clear_usage_id = dct_options()$clear_usage_id,
                                  clear_usage_name =
                                    dct_options()$clear_usage_name,
                                  fill_usage_name =
                                    dct_options()$fill_usage_name,
                                  remap_names = dct_options()$remap_names,
                                  remap_variant = dct_options()$remap_variant,
                                  stamp_modified = dct_options()$stamp_modified,
                                  strict = dct_options()$strict,
                                  quiet = dct_options()$quiet,
                                  other_terms = NULL) {
  # Convert any NA input (from args_tbl) to NULL
  if (!is.null(taxon_id) && is.na(taxon_id)) taxon_id <- NULL
  if (!is.null(sci_name) && is.na(sci_name)) sci_name <- NULL
  if (!is.null(tax_status) && is.na(tax_status)) tax_status <- NULL
  if (!is.null(usage_id) && is.na(usage_id)) usage_id <- NULL
  if (!is.null(usage_name) && is.na(usage_name)) usage_name <- NULL
  if (!is.null(other_terms) && isTRUE(is.na(other_terms))) other_terms <- NULL

  # Check input classes ----
  ifelse(
    !is.null(taxon_id),
    assertthat::assert_that(
      assertthat::is.string(taxon_id) | assertthat::is.scalar(taxon_id)
    ),
    TRUE
  )
  ifelse(
    !is.null(sci_name),
    assertthat::assert_that(assertthat::is.string(sci_name)),
    TRUE
  )
  ifelse(
    !is.null(tax_status),
    assertthat::assert_that(assertthat::is.string(tax_status)),
    TRUE
  )
  ifelse(
    !is.null(usage_id),
    assertthat::assert_that(
      assertthat::is.string(usage_id) | assertthat::is.scalar(usage_id)
    ),
    TRUE
  )
  ifelse(
    !is.null(usage_name),
    assertthat::assert_that(assertthat::is.string(usage_name)),
    TRUE
  )
  assertthat::assert_that(assertthat::is.flag(clear_usage_id))
  assertthat::assert_that(assertthat::is.flag(fill_usage_name))
  assertthat::assert_that(assertthat::is.flag(remap_names))
  assertthat::assert_that(assertthat::is.flag(remap_variant))
  assertthat::assert_that(assertthat::is.flag(stamp_modified))
  assertthat::assert_that(assertthat::is.flag(strict))
  assertthat::assert_that(assertthat::is.flag(quiet))
  # Other input checks ----
  # - usage_name must exist in input data
  ifelse(
    !is.null(usage_name),
    assertthat::assert_that(
      usage_name %in% tax_dat$scientificName,
      msg = "`usage_name` not detected in `tax_dat$scientificName`"
    ),
    TRUE
  )
  # - usage_id must exist in input data
  ifelse(
    !is.null(usage_id),
    assertthat::assert_that(
      usage_id %in% tax_dat$taxonID,
      msg = "`usage_id` not detected in `tax_dat$taxonID`"
    ),
    TRUE
  )

  # Convert aliases ----
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
          !is.null(sci_name), "scientificName" %in% colnames(other_terms)
        ) != 2,
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

  # Isolate row to change ----
  # by sci_name or taxon_id
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
    ifelse(
      !is.null(usage_id),
      assertthat::assert_that(
        usage_id_match == usage_id,
        msg = glue::glue(
          "usage_id and usage_name do not agree for usage_name '{usage_name}'"
        )
      ),
      TRUE
    )
    usage_id <- usage_id_match
  }

  # Create new row by modification ----
  new_row <- tax_dat_row
  # - modify taxonomicStatus
  if (!is.null(tax_status)) {
    new_row <- dplyr::mutate(new_row, taxonomicStatus = tax_status)
  }
  # - modify scientificName only if both taxon_id and sci_name provided
  if (!is.null(sci_name) && !is.null(taxon_id)) {
    new_row <- dplyr::mutate(new_row, scientificName = sci_name)
  }
  # - modify acceptedNameUsageID
  if (!is.null(usage_id)) {
    new_row <- dplyr::mutate(new_row, acceptedNameUsageID = usage_id)
  }
  # - clear acceptedNameUsageID if taxonomicStatus includes "accepted"
  # (so if usage_id is provided and taxonomicStatus includes "accepted",
  # the acceptedNameUsageID will be cleared)
  if (isTRUE(clear_usage_id) &&
    "taxonomicStatus" %in% colnames(new_row) &&
    "acceptedNameUsageID" %in% colnames(new_row) &&
    "acceptedNameUsageID" %in% colnames(tax_dat_row)) {
    is_acc <- grepl("accepted", new_row$taxonomicStatus, ignore.case = TRUE)
    if (inherits(tax_dat_row$acceptedNameUsageID, "character") && is_acc) {
      new_row <- dplyr::mutate(new_row, acceptedNameUsageID = NA_character_)
    }
    if (inherits(tax_dat_row$acceptedNameUsageID, "numeric") && is_acc) {
      new_row <- dplyr::mutate(new_row, acceptedNameUsageID = NaN)
    }
  }
  # - fill usage name
  usage_name_match <- NULL
  if (isTRUE(fill_usage_name) &&
    !is.null(usage_id) &&
    "scientificName" %in% colnames(tax_dat) &&
    "taxonID" %in% colnames(tax_dat) &&
    "acceptedNameUsage" %in% colnames(tax_dat_row)) {
    usage_name_match <- tax_dat$scientificName[tax_dat$taxonID == usage_id]
    assertthat::assert_that(
      isTRUE(length(usage_name_match) == 1),
      msg = glue::glue(
        "Not exactly one scientificName matches acceptedNameUsageID \\
        '{usage_id}'"
      )
    )
    new_row <- dplyr::mutate(new_row, acceptedNameUsage = usage_name_match)
  }
  # - clear acceptedNameUsage if taxonomicStatus includes "accepted"
  # (so if usage_name is provided and taxonomicStatus includes "accepted",
  # the acceptedNameUsage will be cleared)
  if (isTRUE(clear_usage_name) &&
    "taxonomicStatus" %in% colnames(new_row) &&
    "acceptedNameUsage" %in% colnames(new_row) &&
    "acceptedNameUsage" %in% colnames(tax_dat_row)) {
    is_acc <- grepl("accepted", new_row$taxonomicStatus, ignore.case = TRUE)
    if (inherits(tax_dat_row$acceptedNameUsage, "character") && is_acc) {
      new_row <- dplyr::mutate(new_row, acceptedNameUsage = NA_character_)
    }
    if (inherits(tax_dat_row$acceptedNameUsage, "numeric") && is_acc) {
      new_row <- dplyr::mutate(new_row, acceptedNameUsage = NaN)
    }
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
      new_row,
      modified = as.character(Sys.time())
    )
  }

  # Change other rows -----
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
      if (isTRUE(fill_usage_name) && !is.null(usage_name_match)) {
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

  # Format output ----
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
#' `usage_id` and `usage_name` must match existing values of acceptedNameUsageID
#' and acceptedNameUsage in the input data (`tax_dat`). On default settings,
#' either can be used and the other will be filled in automatically
#' (`fill_usage_id` and `fill_usage_name` are both `TRUE`).
#' `r check_fill_usage_id_name()`
#'
#' `tax_status` any other arguments provided that
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
#' If `clear_usage_id` or `clear_usage_name` is `TRUE` and `tax_status`
#' (or `taxonomicStatus`) includes the word "accepted", acceptedNameUsageID
#' or acceptedNameUsage will be set to NA respectively, regardless of the
#' values of `usage_id`, `usage_name`, or `fill_usage_name`.
#'
#' Can either modify a single row in the input taxonomic database if each
#' argument is supplied as a vector of length 1, or can apply a set of changes
#' to the taxonomic database if the input is supplied as a dataframe via
#' `args_tbl`.
#'
#' @param tax_dat Dataframe; taxonomic database in DWC format.
#' @param taxon_id Character or numeric vector of length 1; taxonID of the row
#' to be modified (the selected row). Can also use `taxonID`.
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
#' @param clear_usage_id `r param_clear_usage_id`
#' @param clear_usage_name `r param_clear_usage_id`
#' @param fill_usage_name `r param_fill_usage_name`
#' @param remap_names `r param_remap_names`
#' @param remap_variant `r param_remap_variant`
#' @param stamp_modified `r param_stamp_modified`
#' @param strict `r param_strict`.
#' @param quiet `r param_quiet`.
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
#' @example inst/examples/dct_modify_row.R
dct_modify_row <- function(tax_dat,
                           taxon_id = NULL,
                           sci_name = NULL,
                           tax_status = NULL,
                           usage_id = NULL,
                           usage_name = NULL,
                           clear_usage_id = dct_options()$clear_usage_id,
                           clear_usage_name = dct_options()$clear_usage_name,
                           fill_usage_name = dct_options()$fill_usage_name,
                           remap_names = dct_options()$remap_names,
                           remap_variant = dct_options()$remap_variant,
                           stamp_modified = dct_options()$stamp_modified,
                           strict = dct_options()$strict,
                           quiet = dct_options()$quiet,
                           args_tbl = NULL,
                           ...) {
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
  std_args <- formals(dct_modify_row) |>
    names()

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
        clear_usage_name = val_if_in_dat(args_tbl, "clear_usage_name", i),
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
    clear_usage_name = clear_usage_name,
    fill_usage_name = fill_usage_name,
    remap_names = remap_names,
    remap_variant = remap_variant,
    stamp_modified = stamp_modified,
    strict = strict,
    quiet = quiet,
    other_terms = tibble::tibble(...)
  )
}
