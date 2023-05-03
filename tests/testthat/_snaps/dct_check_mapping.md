# check_mapping_to_self works bad_dat=1          , 2          , 3          , NA         , 1          , 3          , Species foo, Species bar, Species bat, Accepted   , Synonym    , Synonym    , bad_col=acceptedNameUsageID

    Code
      (expect_error(check_mapping_to_self(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected with identical acceptedNameUsageID.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad acceptedNameUsageID: 3>

---

    Code
      check_mapping_to_self(bad_dat, col_select = bad_col, on_fail = "summary",
        quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID scientificName acceptedNameUsageID error                         check
        <chr>   <chr>          <chr>               <glue>                        <chr>
      1 3       Species bat    3                   taxonID detected with identi~ chec~

---

    Code
      (expect_error(dct_check_mapping(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected with identical acceptedNameUsageID.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad acceptedNameUsageID: 3>

---

    Code
      dct_check_mapping(bad_dat, col_select = bad_col, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID acceptedNameUsageID scientificName error                         check
        <chr>   <chr>               <chr>          <glue>                        <chr>
      1 3       3                   Species bat    taxonID detected with identi~ chec~

# check_mapping_to_self works bad_dat=1          , 2          , 3          , NA         , 1          , 3          , Species foo, Species bar, Species bat, Accepted   , Synonym    , Synonym    , bad_col=parentNameUsageID

    Code
      (expect_error(check_mapping_to_self(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected with identical parentNameUsageID.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad parentNameUsageID: 3>

---

    Code
      check_mapping_to_self(bad_dat, col_select = bad_col, on_fail = "summary",
        quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID scientificName parentNameUsageID error                           check
        <chr>   <chr>          <chr>             <glue>                          <chr>
      1 3       Species bat    3                 taxonID detected with identica~ chec~

---

    Code
      (expect_error(dct_check_mapping(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected with identical parentNameUsageID.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad parentNameUsageID: 3>

---

    Code
      dct_check_mapping(bad_dat, col_select = bad_col, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID parentNameUsageID scientificName error                           check
        <chr>   <chr>             <chr>          <glue>                          <chr>
      1 3       3                 Species bat    taxonID detected with identica~ chec~

# check_mapping_to_self works bad_dat=1          , 2          , 3          , NA         , 1          , 3          , Species foo, Species bar, Species bat, Accepted   , Synonym    , Synonym    , bad_col=originalNameUsageID

    Code
      (expect_error(check_mapping_to_self(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected with identical originalNameUsageID.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad originalNameUsageID: 3>

---

    Code
      check_mapping_to_self(bad_dat, col_select = bad_col, on_fail = "summary",
        quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID scientificName originalNameUsageID error                         check
        <chr>   <chr>          <chr>               <glue>                        <chr>
      1 3       Species bat    3                   taxonID detected with identi~ chec~

---

    Code
      (expect_error(dct_check_mapping(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected with identical originalNameUsageID.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad originalNameUsageID: 3>

---

    Code
      dct_check_mapping(bad_dat, col_select = bad_col, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID originalNameUsageID scientificName error                         check
        <chr>   <chr>               <chr>          <glue>                        <chr>
      1 3       3                   Species bat    taxonID detected with identi~ chec~

# check_mapping_exists works bad_dat=1          , 2          , 3          , NA         , 1          , 4          , Species foo, Species bar, Species bat, bad_col=acceptedNameUsageID

    Code
      (expect_error(check_mapping_exists(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected whose acceptedNameUsageID value does not map to taxonID of an existing name.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad acceptedNameUsageID: 4>

---

    Code
      check_mapping_exists(bad_dat, col_select = bad_col, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID scientificName acceptedNameUsageID error                         check
        <chr>   <chr>          <chr>               <glue>                        <chr>
      1 3       Species bat    4                   taxonID detected whose accep~ chec~

---

    Code
      (expect_error(dct_check_mapping(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected whose acceptedNameUsageID value does not map to taxonID of an existing name.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad acceptedNameUsageID: 4>

---

    Code
      dct_check_mapping(bad_dat, col_select = bad_col, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID acceptedNameUsageID scientificName error                         check
        <chr>   <chr>               <chr>          <glue>                        <chr>
      1 3       4                   Species bat    taxonID detected whose accep~ chec~

# check_mapping_exists works bad_dat=1          , 2          , 3          , NA         , 1          , 4          , Species foo, Species bar, Species bat, bad_col=parentNameUsageID

    Code
      (expect_error(check_mapping_exists(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected whose parentNameUsageID value does not map to taxonID of an existing name.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad parentNameUsageID: 4>

---

    Code
      check_mapping_exists(bad_dat, col_select = bad_col, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID scientificName parentNameUsageID error                           check
        <chr>   <chr>          <chr>             <glue>                          <chr>
      1 3       Species bat    4                 taxonID detected whose parentN~ chec~

---

    Code
      (expect_error(dct_check_mapping(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected whose parentNameUsageID value does not map to taxonID of an existing name.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad parentNameUsageID: 4>

---

    Code
      dct_check_mapping(bad_dat, col_select = bad_col, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID parentNameUsageID scientificName error                           check
        <chr>   <chr>             <chr>          <glue>                          <chr>
      1 3       4                 Species bat    taxonID detected whose parentN~ chec~

# check_mapping_exists works bad_dat=1          , 2          , 3          , NA         , 1          , 4          , Species foo, Species bar, Species bat, bad_col=originalNameUsageID

    Code
      (expect_error(check_mapping_exists(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected whose originalNameUsageID value does not map to taxonID of an existing name.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad originalNameUsageID: 4>

---

    Code
      check_mapping_exists(bad_dat, col_select = bad_col, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID scientificName originalNameUsageID error                         check
        <chr>   <chr>          <chr>               <glue>                        <chr>
      1 3       Species bat    4                   taxonID detected whose origi~ chec~

---

    Code
      (expect_error(dct_check_mapping(bad_dat, col_select = bad_col)))
    Output
      <assertError: check_mapping failed.
      taxonID detected whose originalNameUsageID value does not map to taxonID of an existing name.
      Bad taxonID: 3
      Bad scientificName: Species bat
      Bad originalNameUsageID: 4>

---

    Code
      dct_check_mapping(bad_dat, col_select = bad_col, on_fail = "summary", quiet = TRUE)
    Output
      # A tibble: 1 x 5
        taxonID originalNameUsageID scientificName error                         check
        <chr>   <chr>               <chr>          <glue>                        <chr>
      1 3       4                   Species bat    taxonID detected whose origi~ chec~

