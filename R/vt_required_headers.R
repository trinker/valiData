#' Validate that a CSV's Required Headers Are There
#'
#' \code{vt_required_headers} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} required headers are there (differes from
#' \code{vt_required_columns} in that no values in cells are required just the
#' header).
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param ignore.case logical.  Should case be ignored?
#' @param ignore.space logical.  Should whitespace be ignored?
#' @param file_name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_required_headers
#' @export
vt_required_headers <- function(data, map, ignore.case = FALSE, ignore.space = FALSE, file_name = NULL){

    if (is.null(file_name)) file_name <- "The file"

    map_cols <- map[['table_level']][['required_headers']][[file_name]]

	act_nms <- actual_names <- colnames(data)
	exp_nms <- expected_names <- map_cols

	if (ignore.case) {
		actual_names <- tolower(actual_names)
		expected_names <- tolower(expected_names)
	}

	if (ignore.space) {
		actual_names <- gsub("\\s+", "", actual_names)
		expected_names <- gsub("\\s+", "", expected_names)
	}

	not_found <- setdiff(expected_names, actual_names)
	not_found <- exp_nms[expected_names %in% not_found]

	nullify <- function(x) {if(length(x) == 0) NULL else x}

	colnms <- list(
		valid = length(not_found) == 0,
		locations =  list(                                            ## location of those not validating
			missing_headers = nullify(not_found)
		),
		proportion =  1 - length(not_found)/length(expected_names),   ## proportion of those vaidating
		call = "vt_required_headers",                                         ## function name that was called
		file_name = file_name,
		ignore_case = ignore.case,
		ignore_space = ignore.space
	)
	class(colnms) <- 'vt_required_headers'
	colnms
}


#' Prints a vt_required_headers Object
#'
#' Prints a vt_required_headers object
#'
#' @param x A vt_required_headers object.
#' @param \ldots ignored.
#' @method print vt_required_headers
#' @export
print.vt_required_headers <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

	    if (!is.null(x[["locations"]][["missing_headers"]])) {
	        present <- sprintf("The following column headers are NOT present in '%s' but required:\n\n%s\n\n\n\n",
                  x[["file_name"]],
	            paste(paste0("\t- ", x[["locations"]][["missing_headers"]]), collapse = "\n")
              )
	    } else {
	        present <- ""
	    }

		message <- sprintf(
			paste0(header("Required Header Test"),
				"'%s' is missing column headers that are required (header must be present; cell values not required).\n",
				present
			),
			x[["file_name"]]
		)

		class(message) <- c('invalid_report', "character")
		print(message)
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		print(message)
	}

}
