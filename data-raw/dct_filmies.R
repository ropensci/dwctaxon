## code to prepare `dct_filmies` dataset goes here

library(taxastand)
library(dplyr)
library(stringr)

dct_filmies <-
  filmy_taxonomy %>%
  filter(
    taxonomicStatus != "ambiguous synonym",
    taxonomicStatus != "provisionally accepted name"
  ) %>%
  filter(
    acceptedNameUsageID %in% .$taxonID |
      is.na(acceptedNameUsageID)
  ) %>%
  mutate(
    taxonomicStatus = str_remove_all(taxonomicStatus, " name")
  ) %>%
  select(
    taxonID, acceptedNameUsageID, taxonomicStatus,
    taxonRank, scientificName
  ) %>%
  mutate(
    across(everything(), as.character),
    scientificName = stringi::stri_trans_general(scientificName, "latin-ascii")
  ) %>%
  dct_validate()

usethis::use_data(dct_filmies, overwrite = TRUE)
