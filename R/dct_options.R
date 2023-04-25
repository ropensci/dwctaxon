#' Get and set function arguments via options
#'
#' Changes the default values of function arguments.
#'
#' Use this to change the default values of function arguments. That way, you
#' don't have to type the same thing each time you call a function.
#'
#' The arguments that can be set with this function are as follows:
#'
#' ### Validation arguments
#'
#' - `check_taxon_id`: `r param_check_taxon_id`
#' - `check_tax_status`: `r param_check_tax_status`
#' - `check_mapping_accepted`: `r param_check_mapping_accepted`
#' - `check_mapping_parent`: `r param_check_mapping_parent`
#' - `check_mapping_original`: `r param_check_mapping_original`
#' - `check_mapping_accepted_status`: `r param_check_mapping_accepted_status`
#' (See `dct_validate()`).
#' - `check_sci_name`: `r param_check_sci_name`
#' - `check_status_diff`: `r param_check_status_diff`
#' - `check_col_names`: `r param_check_col_names`
#' - `valid_tax_status`: `r param_valid_tax_status`
#' - `skip_missing_cols`: `r param_skip_missing_cols`
#' - `on_success`: `r param_on_success`
#' - `on_fail`: `r param_on_fail`
#'
#' ### Editing arguments
#'
#' - `fill_taxon_id`: `r param_fill_taxon_id`
#' - `fill_usage_id`: `r param_fill_usage_id`
#' - `taxon_id_length`: `r param_taxon_id_length`
#' - `clear_usage_id`: `r param_clear_usage_id`
#' - `clear_usage_name`: `r param_clear_usage_name`
#' - `fill_usage_name`: `r param_fill_usage_name`
#' - `remap_names`: `r param_remap_names`
#' - `remap_variant`: `r param_remap_variant`
#' - `stamp_modified`: `r param_stamp_modified`
#'
#' ### General arguments
#'
#' - `strict`: `r param_strict`
#' - `quiet`: `r param_quiet`
#'
#' @param reset Logical vector of length 1; if TRUE, reset all options to their
#' default values.
#' @param ... Any number of `argument = value` pairs, where the left side is the
#' name of the argument and the right side is its value. See Details and
#' Examples.
#'
#' @return Nothing; used for its side-effect.
#' @example inst/examples/dct_options.R
#' @autoglobal
#' @export
dct_options <- function(reset = FALSE,
                        ...) {
  assertthat::assert_that(assertthat::is.flag(reset))
  if (reset) {
    settings::reset(dct_opts)
  } else {
    dct_opts(...)
  }
}

dct_opts <- settings::options_manager(
  # validation
  check_taxon_id = TRUE,
  check_tax_status = TRUE,
  check_mapping_accepted = TRUE,
  check_mapping_parent = TRUE,
  check_mapping_original = TRUE,
  check_mapping_accepted_status = FALSE,
  check_sci_name = TRUE,
  check_status_diff = FALSE,
  check_col_names = TRUE,
  valid_tax_status = "accepted, synonym, variant, NA",
  skip_missing_cols = FALSE,
  on_success = "data",
  on_fail = "error",
  # editing
  fill_taxon_id = TRUE,
  fill_usage_id = TRUE,
  taxon_id_length = 32,
  clear_usage_id = TRUE,
  clear_usage_name = TRUE,
  fill_usage_name = TRUE,
  remap_names = TRUE,
  remap_variant = FALSE,
  stamp_modified = TRUE,
  strict = FALSE,
  # general
  quiet = FALSE,
  .allowed = list(
    check_taxon_id = settings::inlist(TRUE, FALSE),
    check_tax_status = settings::inlist(TRUE, FALSE),
    check_mapping_accepted = settings::inlist(TRUE, FALSE),
    check_mapping_parent = settings::inlist(TRUE, FALSE),
    check_mapping_original = settings::inlist(TRUE, FALSE),
    check_mapping_accepted_status = settings::inlist(TRUE, FALSE),
    check_sci_name = settings::inlist(TRUE, FALSE),
    check_status_diff = settings::inlist(TRUE, FALSE),
    check_col_names = settings::inlist(TRUE, FALSE),
    # valid_tax_status is character
    skip_missing_cols = settings::inlist(TRUE, FALSE),
    on_success = settings::inlist("data", "logical"),
    on_fail = settings::inlist("error", "summary"),
    fill_taxon_id = settings::inlist(TRUE, FALSE),
    fill_usage_id = settings::inlist(TRUE, FALSE),
    taxon_id_length = settings::inlist(1:32),
    clear_usage_id = settings::inlist(TRUE, FALSE),
    clear_usage_name = settings::inlist(TRUE, FALSE),
    fill_usage_name = settings::inlist(TRUE, FALSE),
    remap_names = settings::inlist(TRUE, FALSE),
    remap_variant = settings::inlist(TRUE, FALSE),
    stamp_modified = settings::inlist(TRUE, FALSE),
    strict = settings::inlist(TRUE, FALSE),
    quiet = settings::inlist(TRUE, FALSE)
  )
)

get_dct_opt <- function(opt) {
  tmp <- dct_options()
  assertthat::assert_that(
    opt %in% names(tmp),
    msg = "'opt' is not the name of an option."
  )
  tmp[[opt]]
}

print_default <- function(opt, quote = FALSE) {
  if (isTRUE(quote)) {
    return(
      glue::glue('Default `"{get_dct_opt(opt)}"`')
    )
  } else {
    return(
      glue::glue("Default `{get_dct_opt(opt)}`")
    )
  }
}
