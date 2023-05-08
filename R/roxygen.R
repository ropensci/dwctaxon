# R code used for documentation

param_tax_dat <- "Dataframe; taxonomic database in DwC format."

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

param_check_mapping_accepted <- glue::glue(
  'Logical vector of length 1; should all values of \\
  `acceptedNameUsageID` be required to map to the `taxonID` of an existing \\
  name? \\
  {print_default("check_mapping_accepted")}.'
)

param_check_mapping_parent <- glue::glue(
  'Logical vector of length 1; should all values of \\
  `parentNameUsageID` be required to map to the `taxonID` of an existing \\
  name? \\
  {print_default("check_mapping_parent")}.'
)

param_check_mapping_original <- glue::glue(
  'Logical vector of length 1; should all values of \\
  `originalNameUsageID` be required to map to the `taxonID` of an existing \\
  name? \\
  {print_default("check_mapping_original")}.'
)

param_check_mapping_accepted_status <- glue::glue( # nolint
  'Logical vector of length 1; should rules about \\
  mapping of variants and synonyms be enforced? \\
  {print_default("check_mapping_accepted_status")}.'
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
  `"NA"` indicates that missing (NA) values are valid. Case-sensitive.'
)

# Need to manually specify default value for extra cols (NULL will disappear
# when glued), so check this assumption:
if (!is.null(get_dct_opt("extra_cols"))) {
  stop("Default value for extra_cols should be NULL")
}
param_extra_cols <- glue::glue(
  "Character vector; names of columns that should be allowed beyond
  those defined by the Darwin Core DwC taxon standard. \\
  Default NULL. \\
  Providing column name(s) that are valid DwC taxon column(s) has no effect."
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
  provided, should values in the taxonID column be filled in by \\
  generating them automatically from the scientificName? \\
  If the `taxonID` column does not yet exist it will be created. \\
  {print_default("fill_taxon_id")}.'
)

param_fill_usage_id <- glue::glue(
  'Logical vector of length 1; if `usage_id` is not \\
  provided, should values in the acceptedNameUsageID column be filled in by \\
  matching acceptedNameUsage to scientificName? \\
  If the `acceptedNameUsageID` column does not yet exist it will be created. \\
  {print_default("fill_usage_id")}.'
)

param_taxon_id_length <- glue::glue(
  'Numeric vector of length 1; how many characters should be included in \\
  automatically generated values of taxonID? Must be between 1 and 32, \\
  inclusive. \\
  {print_default("taxon_id_length")}.'
)

param_stamp_modified <- glue::glue(
  'Logical vector of length 1; should the `modified` \\
  column of any newly created or modified row include a timestamp with the \\
  date and time of its creation/modification? \\
  If the `modified` column does not yet exist it will be created. \\
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

param_skip_missing_cols <- glue::glue(
  'Logical vector of length 1; should checks be silently skipped if any of the
  columns they inspect are missing? \\
  {print_default("skip_missing_cols")}.'
)

# Check that default values of both clear_usage_name and
# clear_usage_id are TRUE
check_fill_usage_id_name <- function() {
  invisible(
    assertthat::assert_that(
      dct_options()$clear_usage_name && dct_options()$clear_usage_id,
      msg = "clear_usage_name and clear_usage_id are both not TRUE"
    )
  )
}
