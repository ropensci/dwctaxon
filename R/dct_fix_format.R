#' Fix the format of a taxonomic database to conform with Darwin Core Taxon
#' standards
#'
#' @param tax_dat Dataframe; taxonomic database that is supposed to be in Darwin
#'   Core format
#'
#' @return Dataframe; taxonomic database in Darwin Core format
#' @autoglobal
#' @export
dct_fix_format <- function(tax_dat) {
  # Drop non-standard columns
  bad_col_names <- setdiff(colnames(tax_dat), dct_terms$term)
  if (length(bad_col_names) > 0) {
    message(
      glue::glue(
        "Dropping the following non-standard columns: \\
        {paste(bad_col_names, collapse = ', ')}"
      )
    )
    tax_dat <- dplyr::select(tax_dat, -dplyr::all_of(bad_col_names))
  }

  # Coerce column types
  bad_col_types <-
    purrr::map_df(tax_dat, class) |>
    tidyr::pivot_longer(
      names_to = "term",
      values_to = "input_type",
      cols = dplyr::everything()
    ) |>
    dplyr::left_join(dct_terms, by = "term") |>
    dplyr::filter(input_type != type)

  if (nrow(bad_col_types) > 0) {
    for (i in seq_len(bad_col_types)) {
      message(
        glue::glue(
          "Coercing column {bad_col_types$term[[i]]} from \\
          {bad_col_types$input_type[[i]]} to {bad_col_types$type[[i]]}"
        )
      )
      tax_dat[[bad_col_types$term[[i]]]] <- methods::as(
        tax_dat[[bad_col_types$term[[i]]]],
        bad_col_types$type[[i]]
      )
    }
  }

  tax_dat
}
