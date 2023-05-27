# Show all options
dct_options()

# Store existing settings, including any changes made by the user
old_settings <- dct_options()

# View one option
dct_options()$valid_tax_status

# Change one option
dct_options(valid_tax_status = "accepted, weird, whatever")
dct_options()$valid_tax_status

# Reset to default values
dct_options(reset = TRUE)
dct_options()$valid_tax_status

# Multiple options may also be set at once
dct_options(check_taxon_id = FALSE, check_status_diff = TRUE)

# Reset options to those before this example was run
do.call(dct_options, old_settings)
