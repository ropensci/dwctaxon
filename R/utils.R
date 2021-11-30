#' paste3
#'
#' Paste while removing NAs
#'
#' Removes NAs from pasted elements, but if ALL elements are NA, the result is NA.
#'
#' Shamelessly copied from
#' \url{https://stackoverflow.com/questions/13673894/suppress-nas-in-paste}
#' @param ... Strings to paste
#' @param sep Character used to separate pasted strings
paste3 <- function(..., sep=" ") {
	L <- list(...)
	L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
	ret <- gsub(paste0("(^",sep,"|",sep,"$)"),"",
						 gsub(paste0(sep,sep),sep,
						 		 do.call(paste, c(L, list(sep = sep)))))
	is.na(ret) <- ret == ""
	ret
}

