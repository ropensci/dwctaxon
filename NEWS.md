# dwctaxon 2.0.0

## Breaking Changes

- Allow for self-mapping between taxonID and acceptedNameUsageID (https://github.com/ropensci/dwctaxon/pull/60)

- Don't provide aliases for DwC terms (https://github.com/ropensci/dwctaxon/pull/63)

## New features

- Full MD5 hash used for generating values of taxonID, with option to use shorter lengths (https://github.com/ropensci/dwctaxon/pull/45)

- Add option to allow extra (non-DwC) columns (https://github.com/ropensci/dwctaxon/pull/61)

- Add function to drop rows (https://github.com/ropensci/dwctaxon/pull/71)

## Documentation

- Move repo to rOpenSci organization (https://github.com/ropensci/dwctaxon/pull/78)

- Add Statement of Need to README (https://github.com/ropensci/dwctaxon/pull/55)

- Add vignette showing how to fetch data (https://github.com/ropensci/dwctaxon/pull/62)

- Standardize capitalization of "DwC" in documentation (https://github.com/ropensci/dwctaxon/pull/64)

- Clarify when new columns are added in documentation (https://github.com/ropensci/dwctaxon/pull/69)

## Bug fixes

- Fix dct_fill_col() mistakenly allowing use of a missing column for fill_from (https://github.com/ropensci/dwctaxon/pull/54)

## Other

- Reduce cyclomatic complexity by splitting out subfunctions and removing `if()` calls when possible (https://github.com/ropensci/dwctaxon/issues/50)

# dwctaxon 1.0.0

- Working version of package with functionality for editing and validating
dataframes in Darwin Core (DwC) Taxon format
