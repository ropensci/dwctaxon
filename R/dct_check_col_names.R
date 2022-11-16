#' check_col_names: check that no invalid (non-DWC) columns are present
#'
#' Required columns: none
#' Required checks: none
#'
#' @inherit check_taxon_id_not_na
#' @autoglobal
#' @noRd
check_col_names_p <- function(tax_dat,
                              on_fail = "error",
                              on_success = "data",
                              run = TRUE) {
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
    assert_that_d(
      length(bad_col_names) == 0,
      data = tibble::tibble(
        error = glue::glue(
          "Invalid column names detected: {bad_col_names}"
        ),
        check = "check_col_names"
      )
    )
  }
  if (on_success == "data") {
    return(tax_dat)
  }
  if (on_success == "logical") {
    return(TRUE)
  }
}
