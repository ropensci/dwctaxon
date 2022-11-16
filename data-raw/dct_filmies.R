## code to prepare `dct_filmies` dataset goes here

library(taxastand)
library(tidyverse)

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
  dct_fix_format() %>%
  select(
    taxonID, acceptedNameUsageID, taxonomicStatus,
    taxonRank, scientificName
  ) %>%
  dct_validate()

usethis::use_data(dct_filmies, overwrite = TRUE)
