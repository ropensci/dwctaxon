#' Isolate a row of taxonomic data to modify
#'
#' Helper function for dct_modify_row_single
#'
#' @param tax_dat Dataframe; taxonomic data.
#' @param sci_name scientificName of row to isolate.
#' @param taxon_id taxonID of row to isolate.
#'
#' @return Dataframe with one row.
#' @noRd
#' @autoglobal
isolate_row <- function(tax_dat, sci_name = NULL, taxon_id = NULL) {
  assertthat::assert_that(
    !(is.null(sci_name) && is.null(taxon_id)),
    msg = "Must provide one or both of scientificName and taxonID"
  )
  # Identify by taxon_id first
  if (!is.null(taxon_id)) {
    tax_dat_row <- dplyr::filter(tax_dat, taxonID == taxon_id)
    return(tax_dat_row)
  }
  if (!is.null(sci_name)) {
    tax_dat_row <- dplyr::filter(tax_dat, scientificName == sci_name)
    assertthat::assert_that(nrow(tax_dat_row) == 1,
      msg = glue::glue(
        "Not exactly one scientificName in data \\
        matches input scientificName '{sci_name}'"
      )
    )
    return(tax_dat_row)
  }
}

#' Lookup acceptedNameUsageID from a scientific name
#'
#' Helper function for dct_modify_row_single
#'
#' @param tax_dat Dataframe; taxonomic data.
#' @param usage_name scientificName (of synonym) of row to isolate.
#' @param usage_id taxonID (of synonym) of row to isolate.
#'
#' @return Value of taxonID for the row with scientificName matching usage_name
#' @noRd
#' @autoglobal
lookup_usage_id <- function(tax_dat, usage_name = NULL, usage_id = NULL) {
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
  return(usage_id)
}

#' Create a new row of taxonomic data by modification
#'
#' Helper function for dct_modify_row_single
#'
#' @param tax_dat_row Dataframe with a single row; original taxonomic data to
#'   modify.
#' @inheritParams dct_modify_row
#'
#' @return Value of taxonID for the row with scientificName matching usage_name
#' @noRd
#' @autoglobal
create_new_row_by_modification <- function(tax_dat,
                                           tax_dat_row,
                                           tax_status = NULL,
                                           sci_name = NULL,
                                           taxon_id = NULL,
                                           usage_id = NULL,
                                           clear_usage_id = NULL,
                                           fill_usage_name = NULL,
                                           clear_usage_name = NULL,
                                           other_terms = NULL,
                                           stamp_modified = NULL) {
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
    # Need usage_name_match later, so store as attribute
    attributes(new_row) <- c(
      attributes(new_row),
      usage_name_match = usage_name_match
    )
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
  # - add other DwC terms, overwriting existing values
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
  return(new_row)
}

#' Create a new row of taxonomic data by modification
#'
#' Helper function for dct_modify_row_single
#'
#' @param tax_dat_row Dataframe with a single row; original taxonomic data to
#'   modify.
#' @inheritParams dct_modify_row
#'
#' @return Dataframe of additional rows affected by modification
#' @noRd
#' @autoglobal
change_other_rows <- function(tax_dat,
                              tax_dat_row,
                              usage_name_match = NULL,
                              usage_id = NULL,
                              remap_names = NULL,
                              remap_variant = NULL,
                              fill_usage_name = NULL,
                              stamp_modified = NULL) {
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
  return(new_row_other)
}

#' Format output after modifying rows
#'
#' Helper function for dct_modify_row_single
#'
#' @param tax_dat_row Dataframe; original taxonomic data
#' @param tax_dat_row Dataframe with a single row; original taxonomic data to
#'   modify.
#' @param new_row Dataframe with a single row; modified row
#' @param new_row_other Dataframe; other rows of modified data
#' @inheritParams dct_modify_row
#'
#' @return Dataframe including modifications
#' @noRd
#' @autoglobal
format_modified_row_output <- function(tax_dat,
                                       tax_dat_row,
                                       new_row,
                                       new_row_other,
                                       quiet,
                                       strict) {
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

#' Convert NA arguments to NULL
#'
#' Helper function for dct_modify_row_single()
#'
#' If x is a matrix or dataframe, will check for all cells being NA
#'
#' @param x An object to check for being NA or NULL.
#'
#' @return NULL if x was NA
#' @autoglobal
#' @noRd
na_to_null <- function(x) {
  if (isTRUE(!is.null(x) && all(is.na(x)))) {
    x <- NULL
  }
  x
}

#' Modify one row of a taxonomic database
#'
#' @param other_terms Tibble of additional DwC terms to add to data;
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
  taxon_id <- na_to_null(taxon_id)
  sci_name <- na_to_null(sci_name)
  tax_status <- na_to_null(tax_status)
  usage_id <- na_to_null(usage_id)
  usage_name <- na_to_null(usage_name)
  other_terms <- na_to_null(other_terms)

  # Check input ----
  assertthat::assert_that(
    is.null(taxon_id) ||
      assertthat::is.string(taxon_id) ||
      assertthat::is.scalar(taxon_id),
    msg = "taxonID must be of type character or numeric"
  )
  assertthat::assert_that(
    is.null(sci_name) ||
      assertthat::is.string(sci_name),
    msg = "scientificName must be of type character"
  )
  assertthat::assert_that(
    is.null(tax_status) || assertthat::is.string(tax_status),
    msg = "taxonomicStatus must be of type character"
  )
  assertthat::assert_that(
    is.null(usage_id) || assertthat::is.string(usage_id) ||
      assertthat::is.scalar(usage_id),
    msg = "acceptedNameUsageID must be of type character or numeric"
  )
  assertthat::assert_that(
    is.null(usage_name) || assertthat::is.string(usage_name),
    msg = "usage_name is not a string (a length one character vector)."
  )
  assertthat::assert_that(assertthat::is.flag(clear_usage_id))
  assertthat::assert_that(assertthat::is.flag(fill_usage_name))
  assertthat::assert_that(assertthat::is.flag(remap_names))
  assertthat::assert_that(assertthat::is.flag(remap_variant))
  assertthat::assert_that(assertthat::is.flag(stamp_modified))
  assertthat::assert_that(assertthat::is.flag(strict))
  assertthat::assert_that(assertthat::is.flag(quiet))
  # - scientificName is present if usage_name is specified
  assertthat::assert_that(
    is.null(usage_name) ||
      isTRUE("scientificName" %in% colnames(tax_dat)),
    msg = paste(
      "tax_dat must include column 'scientificName' to look up rows by",
      "acceptedNameUsage"
    )
  )
  # - usage_name must exist in input data
  assertthat::assert_that(
    is.null(usage_name) ||
      usage_name %in% tax_dat$scientificName,
    msg = "Input acceptedNameUsage not detected in tax_dat$scientificName"
  )
  # - usage_id must exist in input data
  assertthat::assert_that(
    is.null(usage_id) ||
      usage_id %in% tax_dat$taxonID,
    msg = "Input acceptedNameUsageID not detected in tax_dat$taxonID"
  )
  # - other_terms must be valid DwC terms or an exception specified by
  # options
  assertthat::assert_that(
    is.null(other_terms) ||
      all(
        colnames(other_terms) %in% c(dct_terms$term, dct_options()$extra_cols)
      ),
    msg = paste(
      "All terms to modify must be valid DwC taxon terms (`dct_terms$term`)",
      "or specified by `dct_options$()extra_cols`"
    )
  )

  # Isolate row to change
  tax_dat_row <- isolate_row(tax_dat, sci_name, taxon_id)

  # Look up usage_id if usage_name provided
  usage_id <- lookup_usage_id(tax_dat, usage_name, usage_id)

  # Create new row by modification
  # adds usage_name_match as attribute
  new_row <- create_new_row_by_modification(
    tax_dat = tax_dat,
    tax_dat_row = tax_dat_row,
    tax_status = tax_status,
    sci_name = sci_name,
    taxon_id = taxon_id,
    usage_id = usage_id,
    clear_usage_id = clear_usage_id,
    fill_usage_name = fill_usage_name,
    clear_usage_name = clear_usage_name,
    other_terms = other_terms,
    stamp_modified = stamp_modified
  )

  # Extract usage_name_match from new_row, which is stored as an attribute
  usage_name_match <- attributes(new_row)$usage_name_match
  attributes(new_row)$usage_name_match <- NULL

  # Change other rows
  # For change to synonym or variety (acceptedNameUsageID not NA),
  # check if other names will be affected
  new_row_other <- change_other_rows(
    tax_dat = tax_dat,
    tax_dat_row = tax_dat_row,
    usage_name_match = usage_name_match,
    usage_id = usage_id,
    remap_names = remap_names,
    remap_variant = remap_variant,
    fill_usage_name = fill_usage_name,
    stamp_modified = stamp_modified
  )

  # Format output
  res <- format_modified_row_output(
    tax_dat = tax_dat,
    tax_dat_row = tax_dat_row,
    new_row = new_row,
    new_row_other = new_row_other,
    quiet = quiet,
    strict = strict
  )

  res
}

#' Modify row(s) of a taxonomic database
#'
#' Modify one or more rows in a taxonomic database in Darwin Core (DwC) format.
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
#' automatically (`fill_usage_id` and `fill_usage_name` are both `TRUE`).
#' `r check_fill_usage_id_name()`
#'
#' Any other arguments provided that are DwC terms will be assigned to the
#' selected row (i.e., they will modify the row).
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
#' @param tax_dat Dataframe; taxonomic database in DwC format.
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
#' Other DwC terms can also be included as additional columns,
#' similar to using `...` to modify a single row.
#' @param ... other DwC terms to modify, specified as sets of named values.
#' Each element of the vector must have a name corresponding to a valid
#' DwC term; see [dct_terms].
#'
#' @return Dataframe; taxonomic database in DwC format
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
  assertthat::assert_that(
    is.null(args_tbl) ||
      inherits(args_tbl, "data.frame"),
    msg = "args_tbl must be of class data.frame"
  )
  # - taxonID of tax_dat must be non-missing and unique
  tryCatch(
    dct_check_taxon_id(tax_dat, on_fail = "error", on_success = "logical"),
    error = function(x) {
      stop(
        paste(
          "tax_dat must include column taxonID, which must be a character or",
          "numeric vector with unique, non-missing values"
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
