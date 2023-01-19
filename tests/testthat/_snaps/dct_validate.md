# check_mapping_* works for dup taxid bad_dat=1          , 2          , 3          , NA         , 1          , 3          , Species foo, Species bar, Species bat

    Code
      (expect_error(dct_validate(bad_dat)))
    Output
      <assertError: check_mapping failed.
      taxonID detected with identical acceptedNameUsageID.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad acceptedNameUsageID: 3>

---

    Code
      (expect_warning(dct_validate(bad_dat, on_fail = "summary")))
    Output
      <simpleWarning in assert_that_d(sum(map_id_is_bad) == 0, data = tibble::tibble(taxonID = bad_taxon_id,     scientificName = bad_sci_name, `:=`({        {            col_select        }    }, bad_acc_id), error = error_msg, check = "check_mapping"),     msg = error_msg, quiet = quiet): taxonID detected with identical acceptedNameUsageID>

---

    Code
      dct_validate(bad_dat, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID acceptedNameUsageID scientificName error                         check
        <chr>   <chr>               <chr>          <glue>                        <chr>
      1 3       3                   Species bat    taxonID detected with identi~ chec~

---

    Code
      (expect_error(dct_validate(bad_dat)))
    Output
      <assertError: check_mapping failed.
      taxonID detected with identical parentNameUsageID.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad parentNameUsageID: 3>

---

    Code
      (expect_warning(dct_validate(bad_dat, on_fail = "summary")))
    Output
      <simpleWarning in assert_that_d(sum(map_id_is_bad) == 0, data = tibble::tibble(taxonID = bad_taxon_id,     scientificName = bad_sci_name, `:=`({        {            col_select        }    }, bad_acc_id), error = error_msg, check = "check_mapping"),     msg = error_msg, quiet = quiet): taxonID detected with identical parentNameUsageID>

---

    Code
      dct_validate(bad_dat, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID parentNameUsageID scientificName error                           check
        <chr>   <chr>             <chr>          <glue>                          <chr>
      1 3       3                 Species bat    taxonID detected with identica~ chec~

---

    Code
      (expect_error(dct_validate(bad_dat)))
    Output
      <assertError: check_mapping failed.
      taxonID detected with identical originalNameUsageID.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad originalNameUsageID: 3>

---

    Code
      (expect_warning(dct_validate(bad_dat, on_fail = "summary")))
    Output
      <simpleWarning in assert_that_d(sum(map_id_is_bad) == 0, data = tibble::tibble(taxonID = bad_taxon_id,     scientificName = bad_sci_name, `:=`({        {            col_select        }    }, bad_acc_id), error = error_msg, check = "check_mapping"),     msg = error_msg, quiet = quiet): taxonID detected with identical originalNameUsageID>

---

    Code
      dct_validate(bad_dat, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID originalNameUsageID scientificName error                         check
        <chr>   <chr>               <chr>          <glue>                        <chr>
      1 3       3                   Species bat    taxonID detected with identi~ chec~

# check_mapping_* works for missing taxid bad_dat=1          , 2          , 3          , NA         , 1          , 4          , Species foo, Species bar, Species bat

    Code
      (expect_error(dct_validate(bad_dat)))
    Output
      <assertError: check_mapping failed.
      taxonID detected whose acceptedNameUsageID value does not map to taxonID of an existing name.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad acceptedNameUsageID: 4>

---

    Code
      (expect_warning(dct_validate(bad_dat, on_fail = "summary")))
    Output
      <simpleWarning in assert_that_d(sum(map_id_is_bad) == 0, data = tibble::tibble(taxonID = bad_taxon_id,     scientificName = bad_sci_name, `:=`({        {            col_select        }    }, bad_acc_id), error = error_msg, check = "check_mapping"),     msg = error_msg, quiet = quiet): taxonID detected whose acceptedNameUsageID value does not map to taxonID of an existing name.>

---

    Code
      dct_validate(bad_dat, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID acceptedNameUsageID scientificName error                         check
        <chr>   <chr>               <chr>          <glue>                        <chr>
      1 3       4                   Species bat    taxonID detected whose accep~ chec~

---

    Code
      (expect_error(dct_validate(bad_dat)))
    Output
      <assertError: check_mapping failed.
      taxonID detected whose parentNameUsageID value does not map to taxonID of an existing name.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad parentNameUsageID: 4>

---

    Code
      (expect_warning(dct_validate(bad_dat, on_fail = "summary")))
    Output
      <simpleWarning in assert_that_d(sum(map_id_is_bad) == 0, data = tibble::tibble(taxonID = bad_taxon_id,     scientificName = bad_sci_name, `:=`({        {            col_select        }    }, bad_acc_id), error = error_msg, check = "check_mapping"),     msg = error_msg, quiet = quiet): taxonID detected whose parentNameUsageID value does not map to taxonID of an existing name.>

---

    Code
      dct_validate(bad_dat, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID parentNameUsageID scientificName error                           check
        <chr>   <chr>             <chr>          <glue>                          <chr>
      1 3       4                 Species bat    taxonID detected whose parentN~ chec~

---

    Code
      (expect_error(dct_validate(bad_dat)))
    Output
      <assertError: check_mapping failed.
      taxonID detected whose originalNameUsageID value does not map to taxonID of an existing name.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad originalNameUsageID: 4>

---

    Code
      (expect_warning(dct_validate(bad_dat, on_fail = "summary")))
    Output
      <simpleWarning in assert_that_d(sum(map_id_is_bad) == 0, data = tibble::tibble(taxonID = bad_taxon_id,     scientificName = bad_sci_name, `:=`({        {            col_select        }    }, bad_acc_id), error = error_msg, check = "check_mapping"),     msg = error_msg, quiet = quiet): taxonID detected whose originalNameUsageID value does not map to taxonID of an existing name.>

---

    Code
      dct_validate(bad_dat, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID originalNameUsageID scientificName error                         check
        <chr>   <chr>               <chr>          <glue>                        <chr>
      1 3       4                   Species bat    taxonID detected whose origi~ chec~
