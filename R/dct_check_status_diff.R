#' check_status_diff: check that each sci name has a single value for
#' taxonomic status
#'
#' Required columns:
#' - scientificName
#' - taxonomicStatus
#'
#' Required checks: none
#'
#' @inherit check_taxon_id_not_na
#' @autoglobal
#' @noRd
check_status_diff_p <- function(tax_dat,
                                on_fail,
                                on_success,
                                run = TRUE,
                                quiet) {
  # Set defaults ----
  if (missing(on_success)) {
    on_success <- get_dct_opt("on_success")
  }
  if (missing(on_fail)) {
    on_fail <- get_dct_opt("on_fail")
  }
  if (missing(quiet)) {
    quiet <- get_dct_opt("quiet")
  }

  if (run == FALSE) {
    return(NULL)
  }

  # Count number of taxonomicStatus per sci name
  tax_status_tally <-
    tax_dat |>
    dplyr::group_by(scientificName) |>
    dplyr::count(taxonomicStatus) |>
    dplyr::ungroup() |>
    dplyr::count(scientificName)

  bad_sci_name_uniq <- tax_status_tally$scientificName[
    tax_status_tally$n != 1
  ]

  bad_sci_name <- tax_dat$scientificName[
    tax_dat$scientificName %in% bad_sci_name_uniq
  ]
  bad_tax_status <- tax_dat$taxonomicStatus[
    tax_dat$scientificName %in% bad_sci_name_uniq
  ]
  bad_tax_id <- tax_dat$taxonID[
    tax_dat$scientificName %in% bad_sci_name_uniq
  ]

  # Format results
  if (on_fail == "error") {
    assertthat::assert_that(
      length(bad_sci_name_uniq) == 0,
      msg = glue::glue(
        "check_status_diff failed
         scientificName detected with multiple different values for \\
        taxonomicStatus.
         {make_msg('scientificName', bad_sci_name_uniq)}"
      )
    )
  }
  if (on_fail == "summary") {
    err_msg <- paste(
      "scientificName detected with multiple",
      "different values for taxonomicStatus"
    )
    assert_that_d(
      length(bad_sci_name_uniq) == 0,
      data = tibble::tibble(
        taxonID = bad_tax_id,
        scientificName = bad_sci_name,
        taxonomicStatus = bad_tax_status,
        error = err_msg,
        check = "check_status_diff"
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
