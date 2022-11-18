# fill_usage_id works

    Code
      dct_add_row(dct_add_row(tibble::tibble(taxonID = "123", scientificName = "Foogenus barspecies",
        taxonomicStatus = "accepted"), sci_name = "Bargenus foosp", tax_status = "accepted",
      usage_id = NA, stamp_modified = FALSE), sci_name = c("Foogenus boospecies",
        "Bargenus bkaspecies"), usage_name = c("Foogenus barspecies",
        "Bargenus foosp"), tax_status = "synonym", stamp_modified = FALSE)
    Output
      # A tibble: 4 x 5
        taxonID  scientificName      taxonomicStatus acceptedNameUsageID acceptedNam~1
        <chr>    <chr>               <chr>           <chr>               <chr>        
      1 123      Foogenus barspecies accepted        <NA>                <NA>         
      2 22aad276 Bargenus foosp      accepted        <NA>                <NA>         
      3 442c4d2d Foogenus boospecies synonym         123                 Foogenus bar~
      4 42e72bd3 Bargenus bkaspecies synonym         22aad276            Bargenus foo~
      # ... with abbreviated variable name 1: acceptedNameUsage

# fill_usage_id doesn't create acceptedUsageID column

    Code
      dct_add_row(dct_add_row(tibble::tibble(taxonID = "123", scientificName = "Foogenus barspecies",
        taxonomicStatus = "accepted"), sci_name = "Bargenus foosp", tax_status = "accepted",
      stamp_modified = FALSE), sci_name = c("Foogenus boospecies",
        "Bargenus bkaspecies"), usage_name = c("Foogenus barspecies",
        "Bargenus foosp"), tax_status = "synonym", stamp_modified = FALSE)
    Output
      # A tibble: 4 x 4
        taxonID  scientificName      taxonomicStatus acceptedNameUsage  
        <chr>    <chr>               <chr>           <chr>              
      1 123      Foogenus barspecies accepted        <NA>               
      2 22aad276 Bargenus foosp      accepted        <NA>               
      3 442c4d2d Foogenus boospecies synonym         Foogenus barspecies
      4 42e72bd3 Bargenus bkaspecies synonym         Bargenus foosp     

# setting validation args via options works

    Code
      (expect_no_error(dct_add_row(base_dat, new_dat = add_dat, strict = TRUE)))
    Output
      # A tibble: 2 x 5
        taxonID  scientificName acceptedNameUsageID acceptedNameUsage taxonomicStatus
        <chr>    <chr>          <chr>               <chr>             <chr>          
      1 1        foo            <NA>                <NA>              <NA>           
      2 bd40ef6d foo            <NA>                <NA>              <NA>           

