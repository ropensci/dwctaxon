# Set up data for testing check_mapping_to_self()
bad_dat_dup_taxid <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~scientificName, ~taxonomicStatus,
  "1", NA, "Species foo", "Accepted",
  "2", "1", "Species bar", "Synonym",
  "3", "3", "Species bat", "Synonym"
)
bad_dat_dup_taxid_cols <- c(
  "acceptedNameUsageID", "parentNameUsageID", "originalNameUsageID"
)
bad_dat_dup_taxid <- rep(list(bad_dat_dup_taxid), 3)
bad_dat_dup_taxid[[2]] <- dplyr::rename(
  bad_dat_dup_taxid[[2]], !!bad_dat_dup_taxid_cols[2] := acceptedNameUsageID
)
bad_dat_dup_taxid[[3]] <- dplyr::rename(
  bad_dat_dup_taxid[[3]], !!bad_dat_dup_taxid_cols[3] := acceptedNameUsageID
)

# Set up data for testing check_mapping_exists()
bad_dat_missing_taxid <- tibble::tribble(
  ~taxonID, ~acceptedNameUsageID, ~scientificName,
  "1", NA, "Species foo",
  "2", "1", "Species bar",
  "3", "4", "Species bat"
)
bad_dat_missing_taxid <- rep(list(bad_dat_missing_taxid), 3)
bad_dat_missing_taxid[[2]] <- dplyr::rename(
  bad_dat_missing_taxid[[2]], !!bad_dat_dup_taxid_cols[2] := acceptedNameUsageID
)
bad_dat_missing_taxid[[3]] <- dplyr::rename(
  bad_dat_missing_taxid[[3]], !!bad_dat_dup_taxid_cols[3] := acceptedNameUsageID
)

usethis::use_data(
  bad_dat_dup_taxid_cols,
  bad_dat_dup_taxid,
  bad_dat_missing_taxid,
  internal = TRUE,
  overwrite = TRUE
)
