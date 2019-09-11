#' Validate that a CSV's has No Embedded Nulls
#'
#' \code{vf_embedded_nul} - Validates that a .csv file has bno embedded nuls.  If
#' the file has embedded nuls it is likely due to not being in UTF-8 format (contains
#' multi-byte character sequences).
#'
#' @param path Path to a .csv file.
#' @param \dots ignored.
#' @export
#' @rdname vf_embedded_nul
#' @examples
#' loc <- file.path(tempdir(), 'temp.csv')
#' cat(
#'     paste(c(",x,y", "1, the dog, wen,1", "2,door,2"), collapse="\n"),
#'     file=loc
#' )
#' vf_embedded_nul(loc)
vf_embedded_nul <- function(path, ...){

    data <- suppressWarnings(readr::read_csv(path, col_names = FALSE))
    problem_cases <- readr::problems(data)

    ## The embeded null csv check works under the assumption that readr's `problem`
    ## function outputs a check with an expected and actual columns.  Broken
    ## csv files have a readr `problems` output that looks like this:

        ## > m <- readr::read_csv('Book1.csv')
        ## Warning: 2 parsing failures.
        ##     row col   expected           actual
        ## 1     1 X1    ""                 embedded null
        ## 2     1 X2    1/0/T/F/TRUE/FALSE ""
        ## 3     2 X1    ""                 embedded null

    ## This depends on readr always reporting embedded nul csv files in this way.
    if (
            ## the readr problems has an 'expected' column
            nrow(problem_cases) > 0 &&

            ## the readr problems's expected column contains the word `embedded nul` column
            grepl("embedded null", problem_cases[["actual"]])
        ) {

            valid <- FALSE
	        message <- sprintf(
    	            paste0(header("Embedded Null CSV Test"),
                   "The file '%s' appears to have a embedded nuls.\n",
                   "This issue can affect the reliability of later tests.\n",
                   "This is issue likely caused by an encoding that is not in UTF-8 format (though this is not guaranteed to be the issue).\n",
                   "You can try opening '%s' in Notepad++ (or other text editor) and manually changing the file encoding to UTF-8.'\n\n\n\n"
                ),
                basename(path),
                basename(path)
            )

    } else {


        valid <- TRUE
        message <- ""
    }

	nul <- list(
		valid = valid,                   ## logical did file  validate (not have nuls)
		call = "vf_embedded_nul",           ## function name that was called
		file_name = basename(path),       ## file name
	    message = message
	)

	class(nul) <- 'vf_embedded_nul'
	nul

}



#' Prints a vf_embedded_nul Object
#'
#' Prints a vf_embedded_nul object
#'
#' @param x A vf_embedded_nul object.
#' @param \ldots ignored.
#' @method print vf_embedded_nul
#' @export
print.vf_embedded_nul <- function (x, ...)	{
	if (!isTRUE(x[["valid"]])) {
		cat(x[['message']])
	}
}

