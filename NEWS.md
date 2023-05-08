# dwctaxon 1.0.0.9000

## Breaking Changes

- Allow for self-mapping between taxonID and acceptedNameUsageID (https://github.com/joelnitta/dwctaxon/pull/60)

- Don't provide aliases for DwC terms (https://github.com/joelnitta/dwctaxon/pull/63)

## New features

- Full MD5 hash used for generating values of taxonID, with option to use shorter lengths (https://github.com/joelnitta/dwctaxon/pull/45)

- Add option to allow extra (non-DwC) columns (https://github.com/joelnitta/dwctaxon/pull/61)

- Add function to drop rows (https://github.com/joelnitta/dwctaxon/pull/71)

## Documentation

- Add Statement of Need to README (https://github.com/joelnitta/dwctaxon/pull/55)

- Add vignette showing how to fetch data (https://github.com/joelnitta/dwctaxon/pull/62)

- Standardize capitalization of "DwC" in documentation (https://github.com/joelnitta/dwctaxon/pull/64)

- Clarify when new columns are added in documentation (https://github.com/joelnitta/dwctaxon/pull/69)

## Bug fixes

- Fix dct_fill_col() mistakenly allowing use of a missing column for fill_from (https://github.com/joelnitta/dwctaxon/pull/54)

## Other

- Reduce cyclomatic complexity  by splitting out subfunctions and removing `if()` calls when possible (https://github.com/joelnitta/dwctaxon/issues/50)

# dwctaxon 1.0.0

- Working version of package with functionality for editing and validating
dataframes in Darwin Core (DwC) Taxon format
