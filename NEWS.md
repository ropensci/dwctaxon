# dwctaxon 2.0.3

## Bug fixes

- Don't require that all values of a reference column be unique when matching
values, instead only require that the matched values be unique. This means
it is now possible to use (for example) `dct_fill_col()` to fill 
`acceptedNameUsageID` based on `scientificName` even if `scientificName`
contains some duplicated values, as long as those don't match any
values in the `match_from` column
(https://github.com/ropensci/dwctaxon/pull/97)

## Documentation

- Add documentation of `extra_cols` option in `dct_options()` (https://github.com/ropensci/dwctaxon/pull/99)

## CRAN policies

- Add a check for an internet connection and valid URL when downloading files to
ensure compliance with CRAN policy (https://github.com/ropensci/dwctaxon/pull/94)

# dwctaxon 2.0.2

## Other

- Save options with `options()` and restore at end of each example (https://github.com/ropensci/dwctaxon/commit/c9a3c3b432854521d876c2492931ed6189a7abca)

# dwctaxon 2.0.1

## Other

- Use `/dontrun{}` to skip examples that intentionally cause errors (https://github.com/ropensci/dwctaxon/pull/89)

- Save user settings with `dct_options()` and restore at end of each example (https://github.com/ropensci/dwctaxon/pull/91)

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
