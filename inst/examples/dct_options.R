# Show all options
dct_options()

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

dct_options(reset = TRUE)
