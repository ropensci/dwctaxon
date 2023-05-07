# dct_fill_col() input checks give meaningful errors

    Code
      (expect_error(dct_fill_col(test_dat, fill_to = "a")))
    Output
      <assertError: fill_to must be a valid DwC term; see `dct_terms`>
    Code
      (expect_error(dct_fill_col(test_dat, fill_from = "a")))
    Output
      <assertError: fill_from must be a valid DwC term; see `dct_terms`>
    Code
      (expect_error(dct_fill_col(test_dat, match_to = "a")))
    Output
      <assertError: match_to must be a valid DwC term; see `dct_terms`>
    Code
      (expect_error(dct_fill_col(test_dat, match_from = "a")))
    Output
      <assertError: match_from must be a valid DwC term; see `dct_terms`>

