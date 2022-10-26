#' paste3
#'
#' Paste while removing NAs
#'
#' Removes NAs from pasted elements, but if ALL elements are NA, the result is
#' NA.
#'
#' Shamelessly copied from
#' \url{https://stackoverflow.com/questions/13673894/suppress-nas-in-paste}
#' @param ... Strings to paste
#' @param sep Character used to separate pasted strings
#' @noRd
paste3 <- function(..., sep = " ") {
  L <- list(...)
  L <- lapply(L, function(x) {
    x[is.na(x)] <- ""
    x
  })
  ret <- gsub(
    paste0("(^", sep, "|", sep, "$)"), "",
    gsub(
      paste0(sep, sep), sep,
      do.call(paste, c(L, list(sep = sep)))
    )
  )
  is.na(ret) <- ret == ""
  ret
}

#' Drop the first element of a vector
#'
#' @param x Vector
#' @noRd
drop_first <- function(x) {
  x[-1]
}

#' Make an assertion and return an error message
#'
#' assertr::assert() normally prints the diagnostic part of the error
#' to the screen instead of returning it as a proper error. This will
#' return an error, not a printed message
#'
#' @param ... Arguments passed to assertr::assert()
#' @noRd
assert_dat <- function(...) {
  assertthat::assert_that(
    assertr::assert(
        ...,
        success_fun = assertr::success_logical,
        error_fun = assertr::error_logical
      ),
    msg = utils::capture.output(
            assertr::assert(
              ...,
              success_fun = assertr::success_logical,
              error_fun = assertr::error_return
            )
          ) |>
      drop_first() |>
      paste(collapse = "\n")
  )
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
#' @return TRUE if test is true or `data` if test is false
#' @noRd
assert_that_d <- function(condition, data, msg = NULL, env = parent.frame()) {
  assert_res <- tryCatch(
    expr = assertthat::assert_that(condition, msg = msg),
    error = function(e) return(e),
    warning = function(w) return(w)
  )
  if (isTRUE(assert_res)) {
    return(TRUE)
  } else {
    warning(unclass(assert_res)$message)
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
#'
#' @noRd
assert_col <- function(dat, col, class = NULL, req_by = NULL,
  on_fail = "error", run = TRUE) {
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
      msg = col_err_msg
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
          class[seq_along(class) - 1], "or", class[length(class)]
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
          msg = class_err_msg
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
bind_rows_f <- function(x) {
  x <- x[sapply(x, function(y) inherits(y, "data.frame"))]
  x <- x[sapply(x, function(y) nrow(y) > 0)]
  do.call(dplyr::bind_rows, x)
}

#' Sort columns to match order of those in Darwin Core Taxon
#'
#' Also adds columns "error" and "check"
#'
#' @param x Dataframe
#' @noRd
sort_cols_dwc <- function(x) {
  # valid col names are those in DWC terms plus "error" and "check"
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
any_not_true <- function(x) {
  sapply(x, function(y) !isTRUE(y)) |>
    any()
}

#' Transformer for glue
#' @param str String to print for NULL in glue expressions
#' @noRd
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
make_msg <- function(bad_col, values, is_last = FALSE) {
  if (is.null(values)) return(NULL)
  if (length(values) == 0) return(NULL)
  glue::glue(
    "Bad {bad_col}: {paste(values, collapse = ', ')}{txt}",
    txt = ifelse(is_last, "", "\n"))
}