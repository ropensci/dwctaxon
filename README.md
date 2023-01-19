
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dwctaxon <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![DOI](https://zenodo.org/badge/434126221.svg)](https://zenodo.org/badge/latestdoi/434126221)
[![runiverse](https://joelnitta.r-universe.dev/badges/dwctaxon)](https://joelnitta.r-universe.dev/ui#package:dwctaxon)
<!-- badges: end -->

The goal of dwctaxon is to facilitate working with [Darwin Core Taxon
data](https://dwc.tdwg.org/terms/#taxon) in R.

The typical use-case of dwctaxon is not create Darwin Core Taxon
datasets from scratch (although you could do that), but rather to enable
easy modification and validation of existing datasets.

The primary motivation for validation is so that the dataset can be used
for taxonomic name resolution, for example with the
[taxastand](https://github.com/joelnitta/taxastand) R package.

For detailed usage examples, see the vignettes: - [What is
DWC?](https://joelnitta.github.io/dwctaxon/articles/what-is-dwc.html) -
[Editing DWC taxon
data](https://joelnitta.github.io/dwctaxon/articles/editing.html) -
[Validating DWC taxon
data](https://joelnitta.github.io/dwctaxon/articles/validation.html)

## Installation

`dwctaxon` can be installed from
[r-universe](https://joelnitta.r-universe.dev) or
[github](https://github.com/joelnitta).

``` r
install.packages("dwctaxon", repos = 'https://joelnitta.r-universe.dev')
```

OR

``` r
# install.packages("remotes")
remotes::install_github("joelnitta/dwctaxon")
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
#> # … with 2,441 more rows
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
    sci_name = "Hymenophyllum dwctaxonense Nitta",
    taxonomicStatus = "accepted"
  )
#> # A tibble: 6 × 6
#>   taxonID  acceptedNameUsageID taxonomicStatus taxonRank scientificName                            modified           
#>   <chr>    <chr>               <chr>           <chr>     <chr>                                     <chr>              
#> 1 54115096 <NA>                accepted        species   Cephalomanes atrovirens Presl             <NA>               
#> 2 54133783 54115097            synonym         species   Trichomanes crassum Copel.                <NA>               
#> 3 54115097 <NA>                accepted        species   Cephalomanes crassum (Copel.) M. G. Price <NA>               
#> 4 54133784 54115098            synonym         species   Trichomanes densinervium Copel.           <NA>               
#> 5 54115098 <NA>                accepted        species   Cephalomanes densinervium (Copel.) Copel. <NA>               
#> 6 193e2011 <NA>                accepted        <NA>      Hymenophyllum dwctaxonense Nitta          2023-01-19 16:15:08
```

`dct_modify_row()` modifies a row, automatically re-mapping synonyms if
needed.

``` r
# Change C. densinervium to a synonym of C. crassum
filmies_small |>
  dct_modify_row(
    sci_name = "Cephalomanes densinervium (Copel.) Copel.",
    tax_status = "synonym",
    usage_name = "Cephalomanes crassum (Copel.) M. G. Price"
  )
#> # A tibble: 5 × 6
#>   taxonID  acceptedNameUsageID taxonomicStatus taxonRank scientificName                            modified           
#>   <chr>    <chr>               <chr>           <chr>     <chr>                                     <chr>              
#> 1 54115096 <NA>                accepted        species   Cephalomanes atrovirens Presl             <NA>               
#> 2 54133783 54115097            synonym         species   Trichomanes crassum Copel.                <NA>               
#> 3 54115097 <NA>                accepted        species   Cephalomanes crassum (Copel.) M. G. Price <NA>               
#> 4 54133784 54115097            synonym         species   Trichomanes densinervium Copel.           2023-01-19 16:15:08
#> 5 54115098 54115097            synonym         species   Cephalomanes densinervium (Copel.) Copel. 2023-01-19 16:15:08
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
#> # A tibble: 5 × 6
#>   taxonID  acceptedNameUsageID taxonomicStatus taxonRank scientificName                            acceptedNameUsage                        
#>   <chr>    <chr>               <chr>           <chr>     <chr>                                     <chr>                                    
#> 1 54115096 <NA>                accepted        species   Cephalomanes atrovirens Presl             <NA>                                     
#> 2 54133783 54115097            synonym         species   Trichomanes crassum Copel.                Cephalomanes crassum (Copel.) M. G. Price
#> 3 54115097 <NA>                accepted        species   Cephalomanes crassum (Copel.) M. G. Price <NA>                                     
#> 4 54133784 54115098            synonym         species   Trichomanes densinervium Copel.           Cephalomanes densinervium (Copel.) Copel.
#> 5 54115098 <NA>                accepted        species   Cephalomanes densinervium (Copel.) Copel. <NA>
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

    #> Warning in assert_that_d(length(duplicated_tax_id) == 0, data = tibble::tibble(taxonID = duplicated_tax_id, : isTRUE(x = condition) is not
    #> TRUE
    #> Warning in assert_that_d(sum(map_id_is_bad) == 0, data = tibble::tibble(taxonID = bad_taxon_id, : taxonID detected whose acceptedNameUsageID
    #> value does not map to taxonID of an existing name.
    #> Warning in assert_that_d(length(duplicated_sci_name) == 0, data = tibble::tibble(scientificName = duplicated_sci_name, : scientificName
    #> detected with duplicated value
    #> # A tibble: 9 × 5
    #>   taxonID  acceptedNameUsageID scientificName                             error                                    check         
    #>   <chr>    <chr>               <chr>                                      <glue>                                   <chr>         
    #> 1 54115096 b                   Cephalomanes atrovirens Presl              taxonID detected whose acceptedNameUs... check_mapping 
    #> 2 54133783 k                   Trichomanes crassum Copel.                 taxonID detected whose acceptedNameUs... check_mapping 
    #> 3 54115097 s                   Cephalomanes crassum (Copel.) M. G. Price  taxonID detected whose acceptedNameUs... check_mapping 
    #> 4 54133786 n                   Cephalomanes curvatum (J. Sm.) V. D. Bosch taxonID detected whose acceptedNameUs... check_mapping 
    #> 5 54133783 p                   Trichomanes crassum Copel.                 taxonID detected whose acceptedNameUs... check_mapping 
    #> 6 <NA>     <NA>                Cephalomanes atrovirens Presl              scientificName detected with duplicat... check_sci_name
    #> 7 <NA>     <NA>                Trichomanes crassum Copel.                 scientificName detected with duplicat... check_sci_name
    #> 8 54115096 <NA>                <NA>                                       taxonID detected with duplicated value   check_taxon_id
    #> 9 54133783 <NA>                <NA>                                       taxonID detected with duplicated value   check_taxon_id

### Piping

All the functions in dwctaxon take a dataframe as their first argument
and return a dataframe by default, so they are “pipe-friendly” and can
be chained together:

``` r
dct_filmies |>
  dct_modify_row(
    taxon_id = "54133783",
    tax_status = "accepted"
  ) |>
  dct_add_row(
    sci_name = "Hymenophyllum dwctaxonense Nitta",
    taxonomicStatus = "accepted"
  ) |>
  dct_validate()
#> # A tibble: 2,452 × 6
#>    taxonID  acceptedNameUsageID taxonomicStatus taxonRank scientificName                             modified           
#>    <chr>    <chr>               <chr>           <chr>     <chr>                                      <chr>              
#>  1 54115096 <NA>                accepted        species   Cephalomanes atrovirens Presl              <NA>               
#>  2 54133783 <NA>                accepted        species   Trichomanes crassum Copel.                 2023-01-19 16:15:08
#>  3 54115097 <NA>                accepted        species   Cephalomanes crassum (Copel.) M. G. Price  <NA>               
#>  4 54133784 54115098            synonym         species   Trichomanes densinervium Copel.            <NA>               
#>  5 54115098 <NA>                accepted        species   Cephalomanes densinervium (Copel.) Copel.  <NA>               
#>  6 54133786 54115100            synonym         species   Cephalomanes curvatum (J. Sm.) V. D. Bosch <NA>               
#>  7 54133787 54115100            synonym         species   Cephalomanes javanica (Bl.) V. D. Bosch    <NA>               
#>  8 54133788 54115100            synonym         species   Cephalomanes oblongifolium Presl           <NA>               
#>  9 54133789 54115100            synonym         species   Cephalomanes zollingeri V. D. Bosch        <NA>               
#> 10 54133790 54115100            synonym         species   Lacostea javanica (Bl.) Prantl             <NA>               
#> # … with 2,442 more rows
```

It’s often a good idea to include `dct_validate()` at the end of a chain
to make sure the modified taxonomic database is still correctly
formatted.

## Citing this package

If you use this package, please cite it! Here is an example:

    Nitta, JH (2022) dwctaxon: Tools for working with Darwin Core Taxon data in R. https://doi.org/10.5281/zenodo.6388271

The example DOI above is for the overall package.

Here is the latest DOI, which you should use if you are using the latest
version of the package:

[![DOI](https://zenodo.org/badge/434126221.svg)](https://zenodo.org/badge/latestdoi/434126221)

You can find DOIs for older versions by viewing the “Releases” menu on
the right.

## License

[MIT License](LICENSE.md)
