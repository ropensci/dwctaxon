#' check_col_names: check that no invalid (non-DWC) columns are present
#'
#' Required columns: none
#' Required checks: none
#'
#' @inherit check_taxon_id_not_na
#' @autoglobal
#' @noRd
check_col_names_p <- function(tax_dat,
                              on_fail = dct_options()$on_fail,
                              on_success = dct_options()$on_success,
                              run = TRUE,
                              quiet = dct_options()$quiet) {
  # Set defaults ----

  if (run == FALSE) {
    return(NULL)
  }

  bad_col_names <- setdiff(colnames(tax_dat), dct_terms$term) |>
    unique() |>
    sort()

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      length(bad_col_names) == 0,
      msg = glue::glue(
        "check_col_names failed
         Invalid column name(s) detected.
         {make_msg('column names', bad_col_names)}"
      )
    )
  }
  if (on_fail == "summary") {
    err_msg <- glue::glue(
      "Invalid column names detected: {bad_col_names}"
    )
    assert_that_d(
      length(bad_col_names) == 0,
      data = tibble::tibble(
        error = err_msg,
        check = "check_col_names"
      ),
      msg = err_msg,
      quiet = quiet
    )
  }
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }
}
