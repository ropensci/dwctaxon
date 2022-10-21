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

#' Make an assertion about columns of a dataframe
#'
#' assertr::assert() normally prints the diagnostic part of the error
#' to the screen instead of returning it as a proper error. This will
#' return an error, not a printed message
#'
#' @param dat Input dataframe
#' @param col String; name of column that must be included
#' @param class Character vector of classes. `col` must inherit from at least
#' one of the classes.
#' @param req_by Name of check that requires this assertion
#' @param req_col Logical; is `col` required to be present? Set to FALSE to
#'   check for classes only if that column is present
#' @noRd
assert_col <- function(dat, col, class = NULL, req_by = NULL, req_col = TRUE) {
  if (isTRUE(req_col)) {
    col_err_msg <- if (!is.null(req_by)) {
      glue::glue("`{req_by}` requires column '{col}' in input data")
    } else {
      glue::glue("Column '{col}' required in input data")
    }
    assertthat::assert_that(
        col %in% colnames(dat),
        msg = col_err_msg
      )
  }
  if (!is.null(class)) {
    # early exit if don't require column and it isn't there
    if (!col %in% colnames(dat) && req_col == FALSE) return(TRUE)
    if (length(class) > 2) {
      class_list <- c(
          class[1:length(class) - 1], "or", class[length(class)]
        )
      class_list <- paste(class_list, collapse = "', '")
      class_list <- paste0("'", class_list, "'") |>
        gsub("'or',", "or", x = _)
    } else if (length(class) == 2) {
      class_list <- glue::glue("'{class[[1]]}' or '{class[[2]]}'")
    } else {
      class_list <- glue::glue("'{class}'")
    }
    assertthat::assert_that(
        inherits(dat[[col]], class),
        msg = glue::glue("Column '{col}' must be of class {class_list}")
      )
  }
}