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
    msg = capture.output(
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
