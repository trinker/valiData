#' Validates If Email
#'
#' Validates If Email
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     email =c('cookie@cookiemonster.com', 'joe@hometown.uni'
#'     , 'meet@seven', '@Jim', 'joe@gmail.com', NA),
#'     stringsAsFactors = FALSE
#' )
#' vc_email(dat, 'email')
vc_email <- function(data, x, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise) RFC 5321 compliant
    #regex <- "^[a-z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-z0-9.-]+\\.[a-z]{2,}$"
    ## updated to be less strict via: http://stackoverflow.com/a/38137215/1000343
    regex <- "^(([^<>()\\[\\]\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\.,;:\\s@\"]+)*)|(\".+\"))@(([^<>()\\.,;\\s@\"]+\\.{0,1})+[^<>()\\.,;:\\s@\"]{2,})$"

	is_valid <- grepl(regex, col, ignore.case = TRUE, perl = TRUE)
	is_valid[is_na] <- NA

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s do not follow the format of allowable emails:\n\n%s\n\n\n\n",
			sQuote(x),
		    output_truncate(which(!(is_valid|is_na)))
		)
	} else {
	    message <- NULL
	}

    ## construct vc list & class
    vc_output <- list(
        column_name = x,
        valid = are_valid,
        message = message,
        passing = is_valid,
        missing = is_na,
        call = 'vc_email'
    )

    class(vc_output) <- 'vc'
    vc_output
}


# regex <- "^(([^<>()\\[\\]\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\.,;:\\s@\"]+)*)|(\".+\"))@(([^<>()\\.,;\\s@\"]+\\.{0,1})+[^<>()\\.,;:\\s@\"]{2,})$"
# email_test_data$is_valid <- grepl(regex, email_test_data$email, ignore.case = TRUE, perl = TRUE)

## email_test_data <- structure(list(email = c("Sean.O'Conner@anyuniv.edu", "prettyandsimple@example.com", "very.common@example.com",
## "disposable.style.email.with+symbol@example.com", "other.email-with-dash@example.com",
## "x@example.com", "\"much.more unusual\"@example.com", "\"very.unusual.@.unusual.com\"@example.com",
## "\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com",
## "example-indeed@strange-example.com", "admin@mailserver1", "#!$%&'*+-/=?^_`{}|~@example.org",
## "\"()<>[]:,;@\\\\\\\"!#$%&'-/=?^_`{}| ~.a\"@example.org", "\" \"@example.org",
## "example@localhost", "example@s.solutions", "user@com", "user@localserver",
## "user@[IPv6:2001:db8::1]", "user@[1:2:3:4:5::6]", "Abc.example.com",
## "A@b@c@example.com", "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com",
## "just\"not\"right@example.com", "this is\"not\\allowed@example.com",
## "this\\ still\\\"not\\\\allowed@example.com", "john..doe@example.com",
## "john.doe@example..com"), valid = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
## TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
## TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
## FALSE), is_valid = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
## NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
## )), .Names = c("email", "valid", "is_valid"), row.names = c(NA,
## -28L), class = "data.frame")

