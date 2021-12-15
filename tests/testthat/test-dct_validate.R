test_that("validation of mapping works", {
	bad_dat <- tibble::tribble(
			~taxonID, ~acceptedNameUsageID, ~taxonomicStatus, ~scientificName,
			"1", NA, "accepted", "Species foo",
			"2", "1", "synonym", "Species bar",
			"3", "2", "synonym", "Species bat")
	expect_error(
		dct_validate(bad_dat),
		"`check_mapping` failed. At least one `acceptedNameUsageID` value does not map to `taxonID` of an existing name"
	)
})
