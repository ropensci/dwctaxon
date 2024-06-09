# fill_usage_id works

    Code
      dct_add_row(dct_add_row(tibble::tibble(taxonID = "123", scientificName = "Foogenus barspecies",
        taxonomicStatus = "accepted"), scientificName = "Bargenus foosp",
      taxonomicStatus = "accepted", acceptedNameUsageID = NA, stamp_modified = FALSE),
      scientificName = c("Foogenus boospecies", "Bargenus bkaspecies"),
      acceptedNameUsage = c("Foogenus barspecies", "Bargenus foosp"),
      taxonomicStatus = "synonym", stamp_modified = FALSE)
    Output
      # A tibble: 4 x 5
        taxonID   scientificName taxonomicStatus acceptedNameUsageID acceptedNameUsage
        <chr>     <chr>          <chr>           <chr>               <chr>            
      1 123       Foogenus bars~ accepted        <NA>                <NA>             
      2 22aad276~ Bargenus foosp accepted        <NA>                <NA>             
      3 442c4d2d~ Foogenus boos~ synonym         123                 Foogenus barspec~
      4 42e72bd3~ Bargenus bkas~ synonym         22aad27626cf8f945d~ Bargenus foosp   

# fill_usage_id only cares about uniqueness of matching names

    Code
      dct_add_row(base_dat, scientificName = "Bargenus foosp", acceptedNameUsage = "Bargenus bkaspecies",
        taxonomicStatus = "synonym", stamp_modified = FALSE)
    Output
      # A tibble: 4 x 5
        taxonID   scientificName taxonomicStatus acceptedNameUsage acceptedNameUsageID
        <chr>     <chr>          <chr>           <chr>             <chr>              
      1 1         Foogenus bars~ accepted        <NA>              <NA>               
      2 2         Foogenus bars~ accepted        <NA>              <NA>               
      3 3         Bargenus bkas~ accepted        <NA>              <NA>               
      4 22aad276~ Bargenus foosp synonym         Bargenus bkaspec~ 3                  

# fill_usage_id doesn't create acceptedUsageID column when FALSE

    Code
      dct_add_row(dct_add_row(tibble::tibble(taxonID = "123", scientificName = "Foogenus barspecies",
        taxonomicStatus = "accepted"), scientificName = "Bargenus foosp",
      taxonomicStatus = "accepted", stamp_modified = FALSE), scientificName = c(
        "Foogenus boospecies", "Bargenus bkaspecies"), acceptedNameUsage = c(
        "Foogenus barspecies", "Bargenus foosp"), taxonomicStatus = "synonym",
      stamp_modified = FALSE, fill_usage_id = FALSE)
    Output
      # A tibble: 4 x 4
        taxonID                       scientificName taxonomicStatus acceptedNameUsage
        <chr>                         <chr>          <chr>           <chr>            
      1 123                           Foogenus bars~ accepted        <NA>             
      2 22aad27626cf8f945d4792dfceaa~ Bargenus foosp accepted        <NA>             
      3 442c4d2d5df6c3995a8d6a13d10c~ Foogenus boos~ synonym         Foogenus barspec~
      4 42e72bd3217967151e11da9d181a~ Bargenus bkas~ synonym         Bargenus foosp   

# setting validation args via options works

    Code
      (expect_no_error(dct_add_row(base_dat, new_dat = add_dat, strict = TRUE)))
    Output
      # A tibble: 2 x 5
        taxonID   scientificName acceptedNameUsageID acceptedNameUsage taxonomicStatus
        <chr>     <chr>          <chr>               <chr>             <chr>          
      1 1         foo            <NA>                <NA>              <NA>           
      2 bd40ef6d~ foo            <NA>                <NA>              <NA>           

# Adding modifiedBy works

    Code
      (expect_no_error(dct_add_row(base_dat, scientificName = "foo",
        stamp_modified_by_name = TRUE)))
    Output
      # A tibble: 2 x 3
        taxonID                          scientificName modifiedBy
        <chr>                            <chr>          <chr>     
      1 1                                foo            <NA>      
      2 bd40ef6d4a9413de9c1318a65cbae5d7 foo            me        

# Adding modifiedByID works

    Code
      (expect_no_error(dct_add_row(base_dat, scientificName = "foo",
        stamp_modified_by_id = TRUE)))
    Output
      # A tibble: 2 x 3
        taxonID                          scientificName modifiedByID
        <chr>                            <chr>          <chr>       
      1 1                                foo            <NA>        
      2 bd40ef6d4a9413de9c1318a65cbae5d7 foo            me          

