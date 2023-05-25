
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dwctaxon <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![DOI](https://zenodo.org/badge/434126221.svg)](https://zenodo.org/badge/latestdoi/434126221)
[![runiverse](https://ropensci.r-universe.dev/badges/dwctaxon)](https://ropensci.r-universe.dev/dwctaxon)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/dwctaxon/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/dwctaxon?branch=main)
[![pkgcheck](https://github.com/ropensci/dwctaxon/workflows/pkgcheck/badge.svg)](https://github.com/ropensci/dwctaxon/actions?query=workflow%3Apkgcheck)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/574_status.svg)](https://github.com/ropensci/software-review/issues/574)
<!-- badges: end -->

The goal of dwctaxon is to facilitate working with [Darwin Core Taxon
data](https://dwc.tdwg.org/terms/#taxon) in R.

## Statement of need

dwctaxon facilitates **editing** and **validating** Darwin Core Taxon
data. There are various reasons one might want to do this. Here is a
non-exhaustive list of use-cases for dwctaxon:

- To maintain an existing taxonomic database.
- To prepare a taxonomic database as a reference for taxonomic name
  resolution, for example with the
  [taxastand](https://github.com/joelnitta/taxastand) or
  [U.Taxonstand](https://doi.org/10.1016/j.pld.2022.09.001) R packages.
- To curate taxonomic data as part of a [Darwin Core
  Archive](https://en.wikipedia.org/wiki/Darwin_Core_Archive).

In theory, dwctaxon could be used to create taxonomic databases from
scratch, but it is more likely to be useful for updating and validating
existing databases (R in general is more suited to data wrangling and
analysis as opposed to data entry).

## Resources

For detailed usage examples, see the vignettes:

- [What is
  DwC?](https://docs.ropensci.org/dwctaxon/articles/what-is-dwc.html)
- [Editing DwC taxon
  data](https://docs.ropensci.org/dwctaxon/articles/editing.html)
- [Validating DwC taxon
  data](https://docs.ropensci.org/dwctaxon/articles/validation.html)
- [Real World
  Example](https://docs.ropensci.org/dwctaxon/articles/real-data.html)

For more information about dwctaxon, in particular for using it to
maintain a reference database for taxonomic name resolution, see
[taxastand and dwctaxon: A pair of R packages for standardizing species
names in Darwin Core format (BioDigiCon 2022
talk)](https://www.joelnitta.com/talks/2022-09-27_biodigi.html).

## Installation

`dwctaxon` can be installed from
[r-universe](https://ropensci.r-universe.dev/dwctaxon) or
[github](https://github.com/ropensci/dwctaxon).

``` r
options(repos = c(
  ropensci = "https://ropensci.r-universe.dev/", 
  CRAN = "https://cran.rstudio.com/"
))
install.packages("dwctaxon", dep = TRUE)
```

OR

``` r
# install.packages("remotes")
remotes::install_github("ropensci/dwctaxon")
```

## Usage

First, load packages and a dataset to work with:

``` r
library(tibble) # recommended for pretty printing of tibbles
library(dwctaxon)

dct_filmies
#> # A tibble: 2,451 × 5
#>    taxonID  acceptedNameUsageID taxonomicStatus taxonRank scientificName                            
#>    <chr>    <chr>               <chr>           <chr>     <chr>                                     
#>  1 54115096 <NA>                accepted        species   Cephalomanes atrovirens Presl             
#>  2 54133783 54115097            synonym         species   Trichomanes crassum Copel.                
#>  3 54115097 <NA>                accepted        species   Cephalomanes crassum (Copel.) M. G. Price 
#>  4 54133784 54115098            synonym         species   Trichomanes densinervium Copel.           
#>  5 54115098 <NA>                accepted        species   Cephalomanes densinervium (Copel.) Copel. 
#>  6 54133786 54115100            synonym         species   Cephalomanes curvatum (J. Sm.) V. D. Bosch
#>  7 54133787 54115100            synonym         species   Cephalomanes javanica (Bl.) V. D. Bosch   
#>  8 54133788 54115100            synonym         species   Cephalomanes oblongifolium Presl          
#>  9 54133789 54115100            synonym         species   Cephalomanes zollingeri V. D. Bosch       
#> 10 54133790 54115100            synonym         species   Lacostea javanica (Bl.) Prantl            
#> # ℹ 2,441 more rows
```

`dct_filmies` is a taxonomic dataset of filmy ferns included in
dwctaxon.

For demonstration purposes, we will just use the first five rows:

``` r
filmies_small <- head(dct_filmies, 5)
```

All functions in dwctaxon start with `dct_`.

### Edit data

`dct_add_row()` adds one or more rows, automatically providing values
for `taxonID`.

``` r
filmies_small |>
  dct_add_row(
    scientificName = "Hymenophyllum dwctaxonense Nitta",
    taxonomicStatus = "accepted"
  )
#> # A tibble: 6 × 6
#>   taxonID                          acceptedNameUsageID taxonomicStatus taxonRank scientificName                            modified         
#>   <chr>                            <chr>               <chr>           <chr>     <chr>                                     <chr>            
#> 1 54115096                         <NA>                accepted        species   Cephalomanes atrovirens Presl             <NA>             
#> 2 54133783                         54115097            synonym         species   Trichomanes crassum Copel.                <NA>             
#> 3 54115097                         <NA>                accepted        species   Cephalomanes crassum (Copel.) M. G. Price <NA>             
#> 4 54133784                         54115098            synonym         species   Trichomanes densinervium Copel.           <NA>             
#> 5 54115098                         <NA>                accepted        species   Cephalomanes densinervium (Copel.) Copel. <NA>             
#> 6 193e2011c8ace0ed138af91f41a335cc <NA>                accepted        <NA>      Hymenophyllum dwctaxonense Nitta          2023-05-25 18:37…
```

`dct_modify_row()` modifies a row, automatically re-mapping synonyms if
needed.

``` r
# Change C. densinervium to a synonym of C. crassum
filmies_small |>
  dct_modify_row(
    scientificName = "Cephalomanes densinervium (Copel.) Copel.",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Cephalomanes crassum (Copel.) M. G. Price"
  )
#> # A tibble: 5 × 6
#>   taxonID  acceptedNameUsageID taxonomicStatus taxonRank scientificName                            modified                  
#>   <chr>    <chr>               <chr>           <chr>     <chr>                                     <chr>                     
#> 1 54115096 <NA>                accepted        species   Cephalomanes atrovirens Presl             <NA>                      
#> 2 54133783 54115097            synonym         species   Trichomanes crassum Copel.                <NA>                      
#> 3 54115097 <NA>                accepted        species   Cephalomanes crassum (Copel.) M. G. Price <NA>                      
#> 4 54133784 54115097            synonym         species   Trichomanes densinervium Copel.           2023-05-25 18:37:24.426627
#> 5 54115098 54115097            synonym         species   Cephalomanes densinervium (Copel.) Copel. 2023-05-25 18:37:24.396852
```

`dct_fill_col()` fills in values for columns that have “term” - “termID”
pairs (e.g., `acceptedNameUsage` and `acceptedNameUsageID`).

``` r
# Fill-in the acceptedNameUsage column with scientific names
filmies_small |>
  dct_fill_col(
    fill_to = "acceptedNameUsage",
    fill_from = "scientificName",
    match_to = "taxonID",
    match_from = "acceptedNameUsageID"
  )
#> # A tibble: 5 × 7
#>   taxonID  acceptedNameUsageID taxonomicStatus taxonRank scientificName                            acceptedNameUsage                modified
#>   <chr>    <chr>               <chr>           <chr>     <chr>                                     <chr>                            <chr>   
#> 1 54115096 <NA>                accepted        species   Cephalomanes atrovirens Presl             <NA>                             2023-05…
#> 2 54133783 54115097            synonym         species   Trichomanes crassum Copel.                Cephalomanes crassum (Copel.) M… 2023-05…
#> 3 54115097 <NA>                accepted        species   Cephalomanes crassum (Copel.) M. G. Price <NA>                             2023-05…
#> 4 54133784 54115098            synonym         species   Trichomanes densinervium Copel.           Cephalomanes densinervium (Cope… 2023-05…
#> 5 54115098 <NA>                accepted        species   Cephalomanes densinervium (Copel.) Copel. <NA>                             2023-05…
```

### Validate data

`dct_validate()` is the main function for validation, and automatically
conducts a series of checks. The individual checks can be run with
`dct_check_*()` functions.

The `dct_filmies` dataset is already well-formatted, so it will pass
validation:

``` r
# Default behavior is to return the original dataset if checks pass
# For this example, return TRUE instead
dct_validate(dct_filmies, on_success = "logical")
#> [1] TRUE
```

For demonstration purposes, let’s mess up the data:

``` r
# Start by duplicating some data
filmies_dirty <- rbind(head(dct_filmies), head(dct_filmies, 2))
# Replace some values of `acceptedNameUsageID` with random letters
filmies_dirty$acceptedNameUsageID[sample(1:8, 5)] <- sample(letters, 5)
```

By default, `dct_validate()` will stop with an error on the first check
that fails:

``` r
dct_validate(filmies_dirty)
#> Error: check_taxon_id failed
#>    taxonID detected with duplicated value
#>    Bad taxonID: 54115096, 54133783
```

But it may be useful to get an overview of all the checks that failed.
This can be done by setting `on_fail` to `"summary"`:

``` r
dct_validate(filmies_dirty, on_fail = "summary")
```

    #> Warning: taxonID detected with duplicated value
    #> Warning: taxonID detected whose acceptedNameUsageID value does not map to taxonID of an existing name.
    #> Warning: scientificName detected with duplicated value
    #> # A tibble: 9 × 5
    #>   taxonID  acceptedNameUsageID scientificName                             error                                    check         
    #>   <chr>    <chr>               <chr>                                      <glue>                                   <chr>         
    #> 1 54115096 b                   Cephalomanes atrovirens Presl              taxonID detected whose acceptedNameUs... check_mapping 
    #> 2 54133783 k                   Trichomanes crassum Copel.                 taxonID detected whose acceptedNameUs... check_mapping 
    #> 3 54115097 s                   Cephalomanes crassum (Copel.) M. G. Price  taxonID detected whose acceptedNameUs... check_mapping 
    #> 4 54133786 n                   Cephalomanes curvatum (J. Sm.) V. D. Bosch taxonID detected whose acceptedNameUs... check_mapping 
    #> 5 54133783 p                   Trichomanes crassum Copel.                 taxonID detected whose acceptedNameUs... check_mapping 
    #> 6 54115096 <NA>                Cephalomanes atrovirens Presl              scientificName detected with duplicat... check_sci_name
    #> 7 54133783 <NA>                Trichomanes crassum Copel.                 scientificName detected with duplicat... check_sci_name
    #> 8 54115096 <NA>                <NA>                                       taxonID detected with duplicated value   check_taxon_id
    #> 9 54133783 <NA>                <NA>                                       taxonID detected with duplicated value   check_taxon_id

### Piping

All the functions in dwctaxon take a dataframe as their first argument
and return a dataframe by default, so they are “pipe-friendly” and can
be chained together:

``` r
dct_filmies |>
  dct_modify_row(
    taxonID = "54133783",
    taxonomicStatus = "accepted"
  ) |>
  dct_add_row(
    scientificName = "Hymenophyllum dwctaxonense Nitta",
    taxonomicStatus = "accepted"
  ) |>
  dct_validate()
#> # A tibble: 2,452 × 6
#>    taxonID  acceptedNameUsageID taxonomicStatus taxonRank scientificName                             modified                  
#>    <chr>    <chr>               <chr>           <chr>     <chr>                                      <chr>                     
#>  1 54115096 <NA>                accepted        species   Cephalomanes atrovirens Presl              <NA>                      
#>  2 54133783 <NA>                accepted        species   Trichomanes crassum Copel.                 2023-05-25 18:37:24.552694
#>  3 54115097 <NA>                accepted        species   Cephalomanes crassum (Copel.) M. G. Price  <NA>                      
#>  4 54133784 54115098            synonym         species   Trichomanes densinervium Copel.            <NA>                      
#>  5 54115098 <NA>                accepted        species   Cephalomanes densinervium (Copel.) Copel.  <NA>                      
#>  6 54133786 54115100            synonym         species   Cephalomanes curvatum (J. Sm.) V. D. Bosch <NA>                      
#>  7 54133787 54115100            synonym         species   Cephalomanes javanica (Bl.) V. D. Bosch    <NA>                      
#>  8 54133788 54115100            synonym         species   Cephalomanes oblongifolium Presl           <NA>                      
#>  9 54133789 54115100            synonym         species   Cephalomanes zollingeri V. D. Bosch        <NA>                      
#> 10 54133790 54115100            synonym         species   Lacostea javanica (Bl.) Prantl             <NA>                      
#> # ℹ 2,442 more rows
```

It’s often a good idea to include `dct_validate()` at the end of a chain
to make sure the modified taxonomic database is still correctly
formatted.

## Citing this package

If you use this package, please cite it! Here is an example:

    Nitta, JH and Iwasaki, W (2023) dwctaxon: Tools for working with Darwin Core Taxon data in R. https://doi.org/10.5281/zenodo.6388271

The example DOI above is for the overall package.

Here is the latest DOI, which you should use if you are using the latest
version of the package:

[![DOI](https://zenodo.org/badge/434126221.svg)](https://zenodo.org/badge/latestdoi/434126221)

You can find DOIs for older versions by viewing the “Releases” menu on
the right.

## Contributing

Contributions to this package are welcome! Please see the [Contribution
Guide](https://github.com/ropensci/dwctaxon/blob/main/.github/CONTRIBUTING.md)
and [Code of Conduct](https://ropensci.org/code-of-conduct/).

## Note to developers

[roxyglobals](https://github.com/anthonynorth/roxyglobals) is used to
maintain [`R/globals.R`](R/globals.R), but is not available on CRAN. You
will need to install this package from github and use the `@autoglobal`
or `@global` roxygen tags to develop functions with globals.

## License

[MIT License](https://github.com/ropensci/dwctaxon/blob/main/LICENSE.md)
