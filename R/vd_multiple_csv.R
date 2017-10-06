#' Validate Which Folders Contain Multiple CSVs
#'
#' Validate Which Folders Contain Multiple CSVs
#'
#' @param path path to project directory
#' @param \dots ignored.
#' @return Returns a list of validation results.
#' @rdname vd_multiple_csv
#' @export
#' @examples
#' dir_name <- file.path(tempdir(), "delete_me")
#' dir.create(dir_name)
#' dir(dir_name)
#' lapply(1:2, function(i) {
#'     write.csv(mtcars, file = file.path(dir_name, sprintf('file_%s.csv', i)))
#' })
#' vd_multiple_csv(dir_name)
vd_multiple_csv <- function(path, ...){

    . <- NULL
	paths <- lapply(dir(path, pattern = "\\.(csv|CSV)$",recursive = TRUE), function(x) file.path(path,x)) %>%
		sapply(., "[[",1) %>%
		sapply(.,function(x) gsub("//","/", x), USE.NAMES = FALSE) %>%
		sapply(.,function(x) gsub("~", path.expand("~"), x), USE.NAMES = FALSE)

	if (length(paths) > 0) {

	    grpd <- split(paths, dirname(paths))

        multis <- names(grpd)[unlist(lapply(grpd, length)) > 1]

        are_valid <- length(multis) == 0

	} else {

	    are_valid <- TRUE
	    multis <- FALSE
	}

	if (!are_valid){
		message <-  paste0(
		    header("Multiple .csv Files Per Folder Notice", char="~"),
		    "The following folders contained multiple CSVs to test:\n\n",
		    paste(paste0("\t -", multis) , collapse = "\n" ),
		    "\n\n",
		    "Only the first file in each folder was used.\nPlease remove or combine files.",
		    "\n\n\n\n"
		)
	} else {
	    message <- NULL
	}


	report <- list(
		valid = are_valid,                    ## logical did enough (proportion) elements validate
		locations = multis,    ## location of those not validating,
	    message = message,
		call = "vd_multiple_csv"          ## function name that was
	)
	class(report) <- "vd_multiple_csv"
	report
}


#' Prints a vd_multiple_csv Object
#'
#' Prints a vd_multiple_csv object
#'
#' @param x A vd_multiple_csv object.
#' @param \ldots ignored.
#' @method print vd_multiple_csv
#' @export
print.vd_multiple_csv <- function (x, ...)	{

	if (!isTRUE(x[["valid"]])) {
		cat(x[['message']])
	}
}
