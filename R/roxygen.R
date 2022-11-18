# R code used for documentation

param_tax_dat <- "Dataframe; taxonomic database in DWC format."

param_check_taxon_id <- glue::glue(
  'Logical vector of length 1; should all instances of \\
   `taxonID` be required to be non-missing and unique? \\
  {print_default("check_taxon_id")}.'
)

param_check_tax_status <- glue::glue(
  'Logical vector of length 1; should all taxonomic \\
  names be required to have a valid value for taxonomic status (by \\
  default, "accepted", "synonym", or "variant")? \\
  {print_default("check_tax_status")}.'
)

param_check_mapping <- glue::glue(
  'Logical vector of length 1; should all values of \\
  `acceptedNameUsageID` be required to map to the `taxonID` of an existing \\
  name? \\
  {print_default("check_taxon_id")}.'
)

param_check_mapping_strict <- glue::glue(
  'Logical vector of length 1; should rules about \\
  mapping of variants and synonyms be enforced? \\
  {print_default("check_mapping_strict")}.'
)

param_check_sci_name <- glue::glue(
  'Logical vector of length 1; should all instances of \\
  `scientificName` be required to be non-missing and unique? \\
  {print_default("check_sci_name")}.'
)

param_check_status_diff <- glue::glue(
  'Logical vector of length 1; should each scientific \\
  name be allowed to have only one taxonomic status? \\
  {print_default("check_status_diff")}.'
)

param_check_col_names <- glue::glue(
  'Logical vector of length 1; should all column names be \\
  required to be a valid Darwin Core term? \\
  {print_default("check_col_names")}.'
)

param_valid_tax_status <- glue::glue(
  'Character vector of length 1; valid values for \\
  `taxonomicStatus`. Each value must be separated by a comma. \\
  {print_default("valid_tax_status")}. \\
  `"NA"` indicates that missing (NA) values are valid. Case-sensitive. \\
  Can also be set with `dct_options()`.'
)

param_on_fail <- glue::glue(
  'Character vector of length 1, either "error" or "summary". \\
  Describes what to do if the check fails. \\
  {print_default("on_fail", TRUE)}.'
)

param_on_success <- glue::glue(
  'Character vector of length 1, either "logical" or "data". \\
   Describes what to do if the check passes. \\
   {print_default("on_success", TRUE)}.'
)

param_strict <- glue::glue(
  'Logical vector of length 1; should taxonomic checks be run on \\
  the updated taxonomic database? \\
  {print_default("strict")}.'
)

param_quiet <- glue::glue(
  'Logical vector of length 1; should warnings be silenced? \\
  {print_default("quiet")}.'
)

param_fill_taxon_id <- glue::glue(
  'Logical vector of length 1; if `taxon_id` is not \\
  provided, should values in the taxonID column be filled in by looking \\
  them up from the scientificName? \\
  {print_default("fill_taxon_id")}.'
)

param_fill_usage_id <- glue::glue(
  'Logical vector of length 1; if `usage_id` is not \\
  provided, should values in the acceptedNameUsageID column be filled in by \\
  matching acceptedNameUsage to scientificName? \\
  {print_default("fill_usage_id")}.'
)

param_stamp_modified <- glue::glue(
  'Logical vector of length 1; should the `modified` \\
  column of any newly created or modified row include a timestamp with the \\
  date and time of its creation/modification? \\
  {print_default("stamp_modified")}.'
)

param_fill_usage_name <- glue::glue(
  'Logical vector of length 1; should the \\
  acceptedNameUsage of the selected row be set to the \\
  scientificName corresponding to its acceptedNameUsageID? \\
  {print_default("fill_usage_name")}.'
)

param_clear_usage_id <- glue::glue(
  'Logical vector of length 1; should \\
  acceptedNameUsageID of the selected row be set to `NA` if the word \\
  "accepted" is detected in tax_status (not case-sensitive)? \\
  {print_default("clear_usage_id")}.'
)

param_clear_usage_name <- glue::glue(
  'Logical vector of length 1; should \\
  acceptedNameUsage of the selected row be set to `NA` if the word \\
  "accepted" is detected in tax_status (not case-sensitive)? \\
  {print_default("clear_usage_name")}.'
)

param_remap_names <- glue::glue(
  'Logical vector of length 1; should the \\
  acceptedNameUsageID be updated (remapped) for rows with the same \\
  acceptedNameUsageID as the taxonID of the row to be modified? \\
  {print_default("remap_names")}.'
)

param_remap_variant <- glue::glue(
  'Same as `remap_names`, but applies specifically to \\
  rows with taxonomicStatus of "variant". \\
  {print_default("remap_variant")}.'
)

# Check that default values of both clear_usage_name and
# clear_usage_id are TRUE
check_fill_usage_id_name <- function() {
  invisible(
    assertthat::assert_that(
      get_dct_opt("clear_usage_name") && get_dct_opt("clear_usage_id"),
      msg = "clear_usage_name and clear_usage_id are both not TRUE"
    )
  )
}
