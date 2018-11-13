#' Validates If Date UTC
#'
#' Validates If Date UTC
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     start = c("2016-01-01T09", "R2D2-3CPO", sprintf("2016-04-%sT09", 12:19), NA),
#'     end = c(NA, sprintf("2016-01-%sT09", 11:20)),
#'     stringsAsFactors = FALSE
#' )
#'
#' vc_utc_date(dat, 'start')
#' vc_utc_date(dat, 'end')
vc_utc_date <- function(data, x, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## enable mm/dd/yyyy format
    slasher_locs <- grep('(\\d{1,2})/(\\d{1,2})/(\\d{4})', col)
    zero_added <- gsub('(?<=^|/)(\\d)(?=/)', '0\\1', col[slasher_locs], perl=TRUE)
    col[slasher_locs] <- gsub('(\\d{1,2})/(\\d{1,2})/(\\d{4})', '\\3-\\1-\\2', zero_added, perl = TRUE)

    ## Fix for AM/PM if system allows it (delicate, regex solution)
    regex_12_hr <- '(^.+[ T])(\\d+)(.+)([AP]M)(\\s*)'
    is_12_hr <- grepl(regex_12_hr, col[!is_na])
    if (any(is_12_hr)) {
        vals_12_hr <- as.data.frame(
            stringi::stri_match(col[!is_na][is_12_hr], regex = regex_12_hr)[, 2:6],
            stringsAsFactors = FALSE
        )

        names(vals_12_hr) <- c('S', 'H', 'MS', 'AMPM', 'E')

        vals_12_hr['HI'] <- as.integer(vals_12_hr[['H']])
        vals_12_hr['H'] <- ifelse(vals_12_hr[['AMPM']] == 'PM', as.character(vals_12_hr[['HI']] + 12L), vals_12_hr[['H']])
        vals_12_hr['MS'] <- gsub('\\s+$', '', vals_12_hr[['MS']])

        col[!is_na][is_12_hr] <- unlist(apply(vals_12_hr[c('S', 'H', 'MS', 'E')], 1, paste, collapse = ''))
    }


    ## expression to validate against (elementwise)
    col[!is_na] <- parsedate::parse_iso_8601(trimws(col[!is_na]))
    is_valid <- !c(is.na(col) & !is_na)

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"%s contains %s rows that do not follow the UTC date format:\n\n%s\n\n\n\n",
			sQuote(x),
		    length(!(is_valid|is_na)),
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
        call = 'vc_utc_data'
    )

    class(vc_output) <- 'vc'
    vc_output
}

