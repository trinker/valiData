## This file is for internal use
compare_column <- function(path, parent.column, child.column = parent.column,
    parent, child, ignore.case = TRUE, ignore.element.case = FALSE, ...) {

    . <- NULL

    ## Directory Level ##
    ## check that path points to a directory
    is_directory <- vd_dir(path)
    if (!is_directory[['valid']]) {
        out <- list(path=path, is_directory = is_directory)
        class(out) <- 'valiData_dir_level'
        return(out)
    }

    ## check that directory has stuff in it
    non_empty_directory <- vd_non_empty(path)
    if (!is_directory[['valid']]) {
        out <- list(path=path, empty_directory = non_empty_directory)
        class(out) <- 'valiData_dir_level'
        return(out)
    }

    # csv_subpaths is only for subfolders that containing csv files
    csv_subpaths <- get_paths_to_csvs(path)
    parsed <- strsplit(csv_subpaths, "(\\\\|/)+")
    lens <- sapply(parsed, length) - 1
    folder <- unlist(Map(function(x, y){x[y]}, parsed, lens))

    if(length(stats::na.omit(match(parent, folder))) == 0) return(invisible(NULL))
    if(length(stats::na.omit(match(child, folder))) == 0) return(invisible(NULL))

    parent_file <- csv_subpaths[match(parent, folder)]
    child_file <- csv_subpaths[match(child, folder)]

    #parent_table <- suppressWarnings(readr::read_csv(parent_file, col_types = readr::cols(.default = "c")))
    parent_table <- read_csv_check(parent_file)
    if (!is.data.frame(parent_table)) (return(NULL))


    if (isTRUE(ignore.case)){
        colnames(parent_table) <- stringi::stri_trans_tolower(colnames(parent_table))
    }

    if (isTRUE(ignore.element.case)){
        parent_table[sapply(parent_table, is.character)] <- lapply(parent_table[sapply(parent_table, is.character)], stringi::stri_trans_tolower)
    }

    ## check for duplicate rows minus the personal identifier
    dupes <- vt_duplicated_rows(parent_table[, !parent_table %in% c(parent.column)], parent)
    if (!dupes[['valid']]) {return(dupes)}

    validated <- lapply(stats::na.omit(child_file), function(x){

        child_table <- read_csv_check(x)
        if (!is.data.frame(child_table)) (return(NULL))

        if (isTRUE(ignore.case)){
            colnames(child_table) <- tolower(colnames(child_table))
            child.column <- stringi::stri_trans_tolower(child.column)
            parent.column <- stringi::stri_trans_tolower(parent.column)
        }

        parent_table <- parent_table[,colnames(parent_table) %in% parent.column, drop = FALSE]

        if (sum(colnames(child_table) %in% child.column) == 0) return(NULL)
        child_table <- child_table[,colnames(child_table) %in% child.column, drop = FALSE]


        if (isTRUE(ignore.element.case)){
            child_table[sapply(child_table, is.character)] <- lapply(child_table[sapply(child_table, is.character)], stringi::stri_trans_tolower)
        }

        vc_id_found(data=child_table, x=parent.column, y=child.column,
            data2 = parent_table, ignore.case=ignore.case, parent=parent, child=basename(dirname(x)))

    })
# browser()
    if (sum(!unlist(lapply(validated, is.null))) == 0) return(invisible(NULL))

    dir_info <- invisible(lapply(stats::na.omit(child_file), function(x){
        file_name <- basename(dirname(x))

        ## Print csv folder name and path
        header_file(
            basename(dirname(x)),
            gsub("/+|\\\\+", "/", gsub(path, "~/",  x, fixed=TRUE) )
        )

    }))

    if (sum(unlist(lapply(validated, is.null))) > 0) {
        dir_info <- dir_info[!unlist(lapply(validated, is.null))]
        validated <- validated[!unlist(lapply(validated, is.null))]
    }


# browser()
    out <- list(dir_info = dir_info, validated = validated)

    class(out) <- 'compare_column'
    out

}

read_csv_check <- function(path, ...){

    data <- try(suppressWarnings(readr::read_csv(path, col_types = readr::cols(.default = "c"))))

    if (inherits(data, 'try-error')){
        data2 <- try(suppressWarnings(readr::read_csv(path, col_names = FALSE,
            col_types = readr::cols(.default = "c"))))

        if (!inherits(data2, 'try-error')){
           return('bad header')
        } else {
           return('bad not header')
        }
    } else {
        data
    }
}

vc_id_found <- function(data, x, data2, y = x, ignore.case, parent = 'the parent data', child = '', ...) {

    xc <- ifelse(ignore.case, stringi::stri_trans_tolower(x), x)
    yc <- ifelse(ignore.case, stringi::stri_trans_tolower(y), y)

    col <- sub_out_missing(data[[yc]])
    is_na <- is.na(col)

    if (!all(is_na)) {

        is_valid <- data[[yc]] %in% data2[[xc]]

        is_valid[is_na] <- NA
        are_valid <- all(is_valid, na.rm = TRUE)

        if (!are_valid) {
            message <- sprintf("The following rows of '%s' (in the %s column) contain elements not found in '%s' (in the %s column):\n\n%s\n\n\n\n",
                child, y, parent, x,  output_truncate(which(!is_valid)))
        } else {
            message <- NULL
        }
    } else {
        message <- NULL
        are_valid <- TRUE
        is_valid <- TRUE
    }

    vc_output <- list(column_name = x, valid = are_valid, message = message,
        passing = is_valid, missing = is_na, call = "vc_id_found", column_name.y = y)
    class(vc_output) <- "vc"
    vc_output
}



print.compare_column <- function(x, ...){
    invisible(Map(function(x, y){
        cat(x)
        if (y[['valid']]) {
            print(report_all_is_well())
        } else {
            print(y)
        }
    }, x[['dir_info']], x[['validated']]))
}


