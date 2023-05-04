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

  # - scientificName is present if usage_name is specified
  ifelse(
    !is.null(usage_name),
    assertthat::assert_that(
      isTRUE("scientificName" %in% colnames(tax_dat)),
      msg = paste(
        "tax_dat must include column 'scientificName' to look up rows by",
        "acceptedNameUsage"
      )
    ),
    TRUE
  )
  # - usage_name must exist in input data
  ifelse(
    !is.null(usage_name),
    assertthat::assert_that(
      usage_name %in% tax_dat$scientificName,
      msg = "Input acceptedNameUsage not detected in tax_dat$scientificName"
    ),
    TRUE
  )
  # - usage_id must exist in input data
  ifelse(
    !is.null(usage_id),
    assertthat::assert_that(
      usage_id %in% tax_dat$taxonID,
      msg = "Input acceptedNameUsageID not detected in tax_dat$taxonID"
    ),
    TRUE
  )

  # - other_terms must be valid DWC terms or an exception specified by
  # options
  ifelse(
    isTRUE(nrow(other_terms) > 0),
    assertthat::assert_that(
      all(
        colnames(other_terms) %in% c(dct_terms$term, dct_options()$extra_cols)
      ),
      msg = paste(
        "All terms to modify must be valid DWC taxon terms (`dct_terms$term`)",
        "or specified by `dct_options$()extra_cols`"
      )
    ),
    TRUE
  )

  # Isolate row to change ----
  # by sci_name or taxon_id
  assertthat::assert_that(
    !(is.null(sci_name) && is.null(taxon_id)),
    msg = "Must provide one or both of scientificName and taxonID"
  )
  if (!is.null(taxon_id)) {
    tax_dat_row <- dplyr::filter(tax_dat, taxonID == taxon_id)
  }
  if (is.null(taxon_id) && !is.null(sci_name)) {
    tax_dat_row <- dplyr::filter(tax_dat, scientificName == sci_name)
    assertthat::assert_that(nrow(tax_dat_row) == 1,
      msg = glue::glue(
        "Not exactly one scientificName in data \\
        matches input scientificName '{sci_name}'"
      )
    )
  }

  # Look up usage_id if usage_name provided
  if (!is.null(usage_name)) {
    usage_id_match <- tax_dat$taxonID[tax_dat$scientificName == usage_name]
    assertthat::assert_that(
      length(usage_id_match) == 1,
      msg = glue::glue(
        "Not exactly one scientificName in data \\
        matches input acceptedNameUsage '{usage_name}'"
      )
    )
    # If both usage_id and usage_name provided, they must agree
    ifelse(
      !is.null(usage_id),
      assertthat::assert_that(
        usage_id_match == usage_id,
        msg = glue::glue(
          "Input acceptedNameUsageID and acceptedNameUsage do not agree for \\
          acceptedNameUsage '{usage_name}'"
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
#' `taxonID` is only used to identify the row(s) to modify and is not itself
#' modified. `scientificName` can be used in the same way if `taxonID` is not
#' provided (as long as `scientificName` matches a single row). If both
#' `taxonID` and `scientificName` are provided, `scientificName` will be
#' assigned to the scientificName of the row identified by `taxonID`, replacing
#' any value that already exists.
#'
#' `acceptedNameUsageID` and `acceptedNameUsage` must match existing values of
#' acceptedNameUsageID and acceptedNameUsage in the input data (`tax_dat`). On
#' default settings, either can be used and the other will be filled in
#' automatically (`fill_usage_id` and `fill_usage_name` are both `TRUE`). `r
#' check_fill_usage_id_name()`
#'
#' Any other arguments provided that
#' are DWC terms will be assigned to the selected row (i.e., they will
#' modify the row).
#'
#' If `remap_names` is `TRUE` (default) and `acceptedNameUsageID` is provided,
#' any names that have an acceptedNameUsageID matching the taxonID of the
#' selected row (i.e., synonyms of that row) will also have their
#' acceptedNameUsageID replaced with the new acceptedNameUsageID. This behavior
#' is not applied to names with taxonomicStatus of "variant" by default, but can
#' be turned on for such names with `remap_variant`.
#'
#' If `clear_usage_id` or `clear_usage_name` is `TRUE` and `taxonomicStatus`
#' includes the word "accepted", acceptedNameUsageID
#' or acceptedNameUsage will be set to NA respectively, regardless of the
#' values of `acceptedNameUsageID`, `acceptedNameUsage`, or `fill_usage_name`.
#'
#' Can either modify a single row in the input taxonomic database if each
#' argument is supplied as a vector of length 1, or can apply a set of changes
#' to the taxonomic database if the input is supplied as a dataframe via
#' `args_tbl`.
#'
#' @param tax_dat Dataframe; taxonomic database in DWC format.
#' @param taxonID Character or numeric vector of length 1; taxonID of the row
#' to be modified (the selected row).
#' @param scientificName Character vector of length 1; scientificName of the row
#' to be modified if `taxonID` is `NULL`, OR the scientificName to assign to the
#' selected row if `taxonID` is provided (see Details).
#' @param taxonomicStatus Character vector of length 1; taxonomicStatus to
#' assign to the selected row.
#' @param acceptedNameUsageID Character or numeric vector of length 1;
#' acceptedNameUsageID to assign to the selected row.
#' @param acceptedNameUsage Character vector of length 1; acceptedNameUsage to
#' assign to the selected row.
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
                           taxonID = NULL, # nolint
                           scientificName = NULL, # nolint
                           taxonomicStatus = NULL, # nolint
                           acceptedNameUsageID = NULL, # nolint
                           acceptedNameUsage = NULL, # nolint
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
  tryCatch(
    dct_check_taxon_id(tax_dat, on_fail = "error", on_success = "logical"),
    error = function(x) {
      stop(
        paste(
          "tax_dat must include column taxonID, the values of which must be",
          "unique and non-missing"
        ),
        call. = FALSE
      )
    }
  )

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
        taxon_id = val_if_in_dat(args_tbl, "taxonID", i),
        sci_name = val_if_in_dat(args_tbl, "scientificName", i),
        tax_status = val_if_in_dat(args_tbl, "taxonomicStatus", i),
        usage_id = val_if_in_dat(args_tbl, "acceptedNameUsageID", i),
        usage_name = val_if_in_dat(args_tbl, "acceptedNameUsage", i),
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
    taxon_id = taxonID,
    sci_name = scientificName,
    tax_status = taxonomicStatus,
    usage_id = acceptedNameUsageID,
    usage_name = acceptedNameUsage,
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
