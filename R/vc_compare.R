#' Validates If Comparison Met
#'
#' Validates If Comparison Met
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param y Column name from \code{data} (character string) to be compared to.
#' @param comparison logical operator for the comparison
#' @param date logical.  If \code{TRUE} x and y are converted to dates via
#' \code{parsedate::parse_iso_8601}.
#' @param \ldots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     a = c(NA, 1:10),
#'     b = c(0, 1, 2:10+1),
#'     d1 = c(NA, sprintf("2016-01-%sT09", 11:20)),
#'     d2 = c("2016-01-01T09", "2016-01-11T09", sprintf("2016-05-%sT09", 12:20)),
#'     stringsAsFactors = FALSE
#' )
#'
#' vc_compare(dat, x = 'a', y = 'b', '<')
#' vc_compare(dat, x = 'a', y = 'b', '<=')
#' vc_compare(dat, x = 'd1', y = 'd2', '<', date=TRUE)
#' vc_compare(dat, x = 'd1', y = 'd2', '<=', date=TRUE)
vc_compare <- function(data, x, y, comparison, date = FALSE, ...){
# browser()
    # if(missing(x)) (return())
    # if(missing(y)) (return())

### may ventually need to specify col type her because > makes no sense on character cols

    ## select the column & replace missing with NA
    colx <- sub_out_missing(data[[x]])
    coly <- sub_out_missing(data[[y]])
browser()
    if (!(is.null(data[[x]]) | is.null(data[[y]]))) {



        ## record missing (NA)
        is_na <- c(is.na(colx))|c(is.na(coly))
    # if (x == 'EmailAddress2') browser()
        if (isTRUE(date)) {
            ## enable mm/dd/yyyy format

            slasher_locs <- grep('(\\d{1,2})/(\\d{1,2})/(\\d{4})', colx)
            zero_added <- gsub('(?<=^|/)(\\d)(?=/)', '0\\1', colx[slasher_locs], perl=TRUE)
            colx[slasher_locs] <- gsub('(\\d{1,2})/(\\d{1,2})/(\\d{4})', '\\3-\\1-\\2', zero_added, perl = TRUE)

            slasher_locs <- grep('(\\d{1,2})/(\\d{1,2})/(\\d{4})', coly)
            zero_added <- gsub('(?<=^|/)(\\d)(?=/)', '0\\1', coly[slasher_locs], perl=TRUE)
            coly[slasher_locs] <- gsub('(\\d{1,2})/(\\d{1,2})/(\\d{4})', '\\3-\\1-\\2', zero_added, perl = TRUE)

            colx2 <- as.POSIXct(rep(NA, length(colx)))
            coly2 <- as.POSIXct(rep(NA, length(coly)))

            colx2[!is.na(colx)] <- parsedate::parse_iso_8601(trimws(colx[!is.na(colx)]))
            coly2[!is.na(coly)] <- parsedate::parse_iso_8601(trimws(coly[!is.na(coly)]))

            colx <- as.Date(colx2)
            coly <- as.Date(coly2)

        }

        if (all(!is_na & is.na(colx))|all(!is_na & is.na(coly))) {
            message <- sprintf("All of the date formats used in either %s or %s or both do not follow the ISO 8601 required.\n\n\n\n", x, y)
            is_valid <- rep(FALSE, length(colx))
            are_valid <- FALSE
        } else {
    # browser()
            ## expression to validate against (elementwise)
        	is_valid <- compare(colx, coly, comparison)

        	## valid columnwise: Are all elelemnts either valid or NA?
        	are_valid <- all(is_valid|is_na)

        # 	## generate the comment
        # 	if (!are_valid){
        #         message <- sprintf(
        #             "The following rows of %s are not valid \nbecause they are %s %s:\n\n%s\n\n\n\n",
        #                 sQuote(x),
        #                 switch(comparison,
        #                     "==" = "not equal to",
        #                     "!=" = "equal to",
        #                     ">"  = "not greater than",
        #                     "<"  = "not less than",
        #                     ">=" = "not greater than or equal to",
        #                     "<=" = "not less than or equal to",
        #                     "~=" = "not almost equal (enough)",
        #                     "invalid `compare` argument"
        #                 ),
        #                 sQuote(y),
        #                 output_truncate(which(!(is_valid|is_na)))
        #         )
        # 	} else {
        # 	    message <- NULL
        # 	}

        	if (is.na(are_valid)){

        	  message <- sprintf(
        	    "The following rows of %s and %s are not valid \nbecause they are all missing values:\n\n%s\n\n\n\n",
        	    sQuote(x),
        	    sQuote(y),
        	    output_truncate(which(!(is_na))) )
        	} else if (!are_valid) {
        	  message <- sprintf(
        	    "The following rows of %s are not valid \nbecause they are %s %s:\n\n%s\n\n\n\n",
        	    sQuote(x),
        	    switch(comparison,
        	           "==" = "not equal to",
        	           "!=" = "equal to",
        	           "==@" = "not equal to (ignoring case)",
        	           "!=@" = "equal to (ignoring case)",
        	           ">"  = "not greater than",
        	           "<"  = "not less than",
        	           ">=" = "not greater than or equal to",
        	           "<=" = "not less than or equal to",
        	           "~=" = "not almost equal (enough)",
        	           "invalid `compare` argument"
        	    ),
        	    sQuote(y),
        	    output_truncate(which(!(is_valid|is_na))) )
        	} else {
        	  message <- NULL
        	}
        }
    } else {
        are_valid <- TRUE
        message <- NULL
        is_valid <- TRUE
        is_na <- FALSE

    }
    ## construct vc list & class
    vc_output <- list(
        column_name = x,
        valid = are_valid,
        message = message,
        passing = is_valid,
        missing = is_na,
        call = 'vc_compare'
    )

    class(vc_output) <- 'vc'
    vc_output

}

#' Nearly Equal
#'
#' Helper Function for vc_compare to implement all.equal as new comparison operator
#'
#' @param x number one
#' @param y number two
#' @export
`~=` <- function(x, y){

    unlist(Map(function(a, b) {isTRUE(all.equal(a, b))}, x, y))

}


#' Equal Ignoring Case
#'
#' Helper Function for vc_compare to implement equals with case ignored
#'
#' @param x number one
#' @param y number two
#' @export
#' @name equal-ignore-case
`==@` <- function(x, y){

    unlist(Map(function(a, b) {isTRUE(tolower(a) == tolower(b))}, x, y))

}


#' Not Equal Ignoring Case
#'
#' Helper Function for vc_compare to implement not equal with case ignored
#'
#' @param x number one
#' @param y number two
#' @export
#' @name not-equal
`!=@` <- function(x, y){

    unlist(Map(function(a, b) {isTRUE(tolower(a) != tolower(b))}, x, y))

}



#' Main Helper Function for vc_compare
#'
#' Helper Function for vc_compare
#'
#' @param xx character vector to be coerced
#' @param yy character vector to be coerced and compared
#' @param fun logical operator
#' @export
compare <- function(xx, yy, fun){
    match.fun(fun)(xx, yy)
}
