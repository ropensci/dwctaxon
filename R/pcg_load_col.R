#' Load data from the Catalog of Life
#'
#' Catalog of Life data can be downloaded from here:
#' https://download.catalogueoflife.org/col/monthly/. The "_dwca" format should
#' be selected when downloading (files ending in "_dwca.zip").
#'
#' If `col_data_path` ends in "zip", the function will try to automatically
#' extract the taxonomic names data, which is typically named "Taxon.tsv".
#'
#' @param col_data_path Path to data downloaded from the Catalog of Life; either
#' the original zip file, or the tsv file containing the taxonomic data extracted
#' from the zip file (usually named "Taxon.tsv").
#'
#' @return Dataframe (tibble)
#' @export
pcg_load_col <- function(col_data_path) {
	assertthat::assert_that(fs::file_exists(col_data_path))
	file_ending <- fs::path_ext(col_data_path)

	# Try to unzip Taxon.tsv
	temp_dir <- tempdir()
	temp_tsv <- ""
	if (file_ending == "zip") {
		tryCatch(
		  expr = unzip(col_data_path, files = "Taxon.tsv", overwrite = TRUE, exdir = temp_dir),
		  finally = "Could not find 'Taxon.tsv' in zip file. Try manually unzipping."
		)
		temp_tsv <- fs::path(temp_dir, "Taxon.tsv")
		col_data_path <- temp_tsv
	}

	# Use data.table as it handles quotation marks in data better than read_*() functions
	res <- data.table::fread(file = col_data_path, sep = "\t", stringsAsFactors = FALSE) |>
		tibble::as_tibble()

	# Clean up
	if (file_ending == "zip" && fs::file_exists(temp_tsv)) fs::file_delete(temp_tsv)

	res

}
