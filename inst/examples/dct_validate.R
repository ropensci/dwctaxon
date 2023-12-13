# The example dataset dct_filmies is already correctly formatted and passes
# validation
dct_validate(dct_filmies)

# So make some bad data on purpose with a duplicated scientific name
bad_dat <- dct_filmies
bad_dat$scientificName[1] <- bad_dat$scientificName[2]

# The incorrectly formatted data won't pass
try(
  dct_validate(bad_dat)
)

# It will pass if we allow duplicated scientific names though
dct_validate(bad_dat, check_sci_name = FALSE)

# Individual checks can also be turned or off with dct_options()

# First save the current settings before making any changes
old_settings <- dct_options()

# Let's allow duplicated scientific names by default
dct_options(check_sci_name = FALSE)

# The data passes validation as before, but we don't have to specify
# `check_sci_name = FALSE` in the function call
dct_validate(bad_dat)

# Reset options to those before this example was run
do.call(dct_options, old_settings)
