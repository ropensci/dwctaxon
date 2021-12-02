#' Darwin Core Taxon terms
#'
#' A table of valid Darwin Core terms. Only terms in the Taxon class or at the
#' record-level are included.
#'
#' @format Dataframe (tibble), including two columns:
#' - `group`: Darwin Core term group; either "taxon" (terms in the Taxon class)
#' or "record-level" (terms that are generic in that they might apply
#' to any type of record in a dataset.)
#' - `term`: Darwin Core term
#'
#' with two additional attributes:
#' - `retrieved`: Date the terms were obtained
#' - `url`: URL from which the terms were obtained
#' @source \url{https://dwc.tdwg.org/terms/#taxon}
"dct_terms"
