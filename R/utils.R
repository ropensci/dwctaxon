#' Drop the first element of a vector
#'
#' @param x Vector
#' @noRd
#' @autoglobal
drop_first <- function(x) {
  x[-1]
}

#' Make an assertion and return a dataframe if not true
#'
#' Similar to assertthat::assert_that(), but returns a dataframe (and a warning)
#' on failure.
#'
#' Must only be used inside a function, in which case the function will exit
#' early with the warning and return the data.
#'
#' @param condition The test condition
#' @param data Dataframe to return if test is false
#' @param msg Warning message to return if test if false
#' @param env Environment in which to evaluate the function; default is
#'   the parent frame (function)
#' @param quiet Should warnings be silenced?
#' @return TRUE if test is true or `data` if test is false
#' @noRd
#' @autoglobal
assert_that_d <- function(
  condition,
  data,
  msg = NULL,
  env = parent.frame(),
  quiet = FALSE
) {
  assert_res <- tryCatch(
    expr = assertthat::assert_that(isTRUE(condition), msg = msg),
    error = function(e) {
      return(e)
    },
    warning = function(w) {
      return(w)
    }
  )
  if (isTRUE(assert_res)) {
    return(TRUE)
  } else {
    if (!quiet) {
      warning(
        unclass(assert_res)$message,
        call. = FALSE
      )
    }
    do.call("return", list(data), envir = env)
  }
}

#' Make an assertion about columns of a dataframe
#'
#' @param dat Input dataframe
#' @param col String; name of column that must be included
#' @param class Character vector of classes. `col` must inherit from at least
#' one of the classes.
#' @param req_by Name of check that requires this assertion
#' @param on_fail String; either "error" (return error) or "summary" (return
#'   tibble with summary of failure)
#' @param run Logical; should this check be run? If FALSE, return NULL
#' @param quiet Logical; should warnings be silenced?
#'
#' @noRd
#' @autoglobal
assert_col <- function(
  dat,
  col,
  class = NULL,
  req_by = NULL,
  on_fail = "error",
  run = TRUE,
  quiet = FALSE
) {
  if (run == FALSE) {
    return(NULL)
  }
  # Check for required column
  req_col_exists <- col %in% colnames(dat)
  # Format error message
  col_err_msg <- if (!is.null(req_by)) {
    glue::glue("{req_by} requires column {col} in input data")
  } else {
    glue::glue("Column {col} required in input data")
  }
  # Format output
  if (on_fail == "error") {
    assertthat::assert_that(
      req_col_exists,
      msg = col_err_msg
    )
  } else if (on_fail == "summary") {
    req_col_res <- assert_that_d(
      req_col_exists,
      data = tibble::tibble(check = req_by, error = col_err_msg),
      msg = col_err_msg,
      quiet = quiet
    )
    # Early exit if required col not present
    if (!isTRUE(req_col_res)) {
      return(req_col_res)
    }
  } else {
    stop("on_fail must be 'error' or 'summary'")
  }
  # Continue to check for class
  if (!is.null(class)) {
    # Check for required class, if req col is present
    col_inherits_req_class <- inherits(dat[[col]], class)
    # Format error message
    if (length(class) > 2) {
      class_list <- c(
        class[seq_along(class) - 1],
        "or",
        class[length(class)]
      )
      class_list <- paste(class_list, collapse = ", ") |>
        gsub("or,", "or", x = _)
    } else if (length(class) == 2) {
      class_list <- glue::glue("{class[[1]]} or {class[[2]]}")
    } else {
      class_list <- glue::glue("{class}")
    }
    class_err_msg <- glue::glue("Column {col} must be of class {class_list}")
    # Format output
    if (on_fail == "error") {
      assertthat::assert_that(
        col_inherits_req_class,
        msg = class_err_msg
      )
    } else if (on_fail == "summary") {
      return(
        assert_that_d(
          col_inherits_req_class,
          data = tibble::tibble(check = req_by, error = class_err_msg),
          msg = class_err_msg,
          quiet = quiet
        )
      )
    } else {
      stop("on_fail must be 'error' or 'summary'")
    }
  }
  # If nothing else to check, return TRUE
  TRUE
}

#' Bind rows of several dataframes
#'
#' Similar to dplyr::bind_rows(), but if the input contains anything
#' that is not a dataframe with at least one row it will be excluded
#' @param x List possibly including multiple dataframes
#' @noRd
#' @autoglobal
bind_rows_f <- function(x) {
  x <- x[purrr::map_lgl(x, function(y) inherits(y, "data.frame"))]
  x <- x[purrr::map_lgl(x, function(y) nrow(y) > 0)]
  do.call(dplyr::bind_rows, x)
}

#' Sort columns to match order of those in Darwin Core Taxon
#'
#' Also adds columns "error" and "check"
#'
#' @param x Dataframe
#' @noRd
#' @autoglobal
sort_cols_dwc <- function(x) {
  # valid col names are those in DwC terms plus "error" and "check"
  val_cols <- c(dct_terms$term, "error", "check")
  # Sort colnames in same order as valid col names
  cols <- colnames(x)
  cols_sorted <- val_cols[val_cols %in% cols]
  res <- x[, cols_sorted]
  if (ncol(x) != ncol(res)) {
    stop("Non-valid column names in data")
  }
  res
}

#' Check if any elements of a list are not TRUE
#' @param x A list
#' @noRd
#' @autoglobal
any_not_true <- function(x) {
  purrr::map_lgl(x, function(y) !isTRUE(y)) |>
    any()
}

#' Transformer for glue
#' @param str String to print for NULL in glue expressions
#' @noRd
#' @autoglobal
null_transformer <- function(str = "NULL") {
  function(text, envir) {
    out <- glue::identity_transformer(text, envir)
    if (is.null(out)) {
      return(str)
    }
    out
  }
}

#' Helper function to make an error message
#' @param bad_col Name of column with bad values
#' @param values The bad values
#' @param is_last Logical; is this the last of a series of strings
#'   in an error message? (in which case it should not have a line break
#'   after it).
#' @noRd
#' @autoglobal
make_msg <- function(bad_col, values, is_last = FALSE) {
  if (is.null(values)) {
    return(NULL)
  }
  if (length(values) == 0) {
    return(NULL)
  }
  glue::glue(
    "Bad {bad_col}: {paste(values, collapse = ', ')}{txt}",
    txt = ifelse(is_last, "", "\n")
  )
}

#' Helper function to get value from a dataframe
#'
#' @param df Dataframe
#' @param col String; name of column to extract value
#' @param i Index of the value to extract in the selected column
#'
#' @return The value at the selected index of the selected column, if
#' that column exists; otherwise `NA`
#' @noRd
#' @autoglobal
val_if_in_dat <- function(df, col, i) {
  ifelse(col %in% colnames(df), df[[col]][[i]], NA)
}

#' Generate a taxonID from a scientific name, single version.
#'
#' @param taxon_id String or number; taxonID (may be NA).
#' @param sci_name String; scientific name.
#' @param max_len Number; number of characters to use for generated taxonID.
#'
#' @return String; a taxonID generated from the hash of the scientific name.
#' @noRd
#' @autoglobal
make_taxon_id_from_sci_name_1 <- function(taxon_id, sci_name, max_len = 8) {
  if (!is.na(taxon_id)) {
    return(taxon_id)
  }
  assertthat::assert_that(
    !is.na(sci_name),
    msg = "Cannot generate taxon_id from sci_name because sci_name is NA"
  )
  digest::digest(sci_name) |>
    substr(1, max_len)
}

#' Generate a taxonID from a scientific name, vectorized.
#'
#' @param taxon_id Character vector; taxonID (may be NA).
#' @param sci_name Character vector; scientific name.
#' @param max_len Numeric vector; number of characters to use for generated
#'   taxonID.
#'
#' @return String: a taxonID generated from the hash of the scientific name.
#' @noRd
#' @autoglobal
make_taxon_id_from_sci_name <- function(taxon_id, sci_name, max_len = 8) {
  purrr::map2(
    taxon_id,
    sci_name,
    ~ make_taxon_id_from_sci_name_1(
      taxon_id = .x,
      sci_name = .y,
      max_len = max_len
    )
  ) |>
    unlist()
}

#' Are all values of a vector unique?
#'
#' @param x Vector.
#' @param allow_na Logical; should NA values count when checking for
#' uniqueness?.
#'
#' @return Logical vector of length 1.
#' @noRd
#' @autoglobal
is_unique <- function(x, allow_na = TRUE) {
  if (allow_na) {
    x <- x[!is.na(x)]
  }
  !any(duplicated(x))
}

#' Is the zip file used in real-data.Rmd OK to download?
#'
#' @param url Character vector of length 1; URL pointing to zip file to
#' download ie "https://data.canadensys.net/ipt/archive.do?r=vascan&v=37.12"
#' @param online Logical vector of length 1; is this computer connected to the
#' internet? Defaults to curl::has_internet(), but provided for testing
#' purposes.
#'
#' @return Logical vector of length 1.
#' @noRd
#' @autoglobal
safe_to_download <- function(url, online = curl::has_internet()) {
  get_safely <- purrr::safely(httr::GET)
  # Check for internet connection
  if (!online) {
    return(FALSE)
  }
  # Check for functioning URL
  dl_check <- get_safely(url)
  if (!is.null(dl_check$error)) {
    return(FALSE)
  }
  dl_check_result <- unclass(dl_check$result)
  # Check for client error
  if (dl_check_result$status_code >= 400) {
    return(FALSE)
  }
  # Check that URL points to a zip file
  zip_check <- grepl("zip", dl_check_result$headers$`content-type`)
  if (zip_check == FALSE) {
    return(FALSE)
  }
  TRUE
}

#' Download and unzip a file with error handling
#'
#' @param url Character vector of length 1; URL pointing to zip file to
#' download.
#' @param destfile Character vector of length 1; path where the zip file
#' should be saved.
#' @param exdir Character vector of length 1; directory where the zip file
#' should be extracted.
#' @param quiet Logical vector of length 1; should messages be suppressed?
#'
#' @return Logical vector of length 1; TRUE if successful, FALSE otherwise.
#' @noRd
#' @autoglobal
safe_download_unzip <- function(url, destfile, exdir, quiet = FALSE) {
  # Download data
  download_result <- try(
    suppressWarnings(
      download.file(url = url, destfile = destfile, mode = "wb", quiet = quiet)
    ),
    silent = TRUE
  )

  # Check if download failed
  if (inherits(download_result, "try-error")) {
    if (!quiet) {
      message(
        paste0(
          "Failed to download file from ",
          url
        )
      )
    }
    return(FALSE)
  }

  # Unzip
  unzip_result <- try(
    unzip(destfile, exdir = exdir),
    silent = TRUE
  )

  # Check if unzip failed
  if (inherits(unzip_result, "try-error")) {
    if (!quiet) {
      message("Failed to unzip file.")
    }
    return(FALSE)
  }

  TRUE
}
