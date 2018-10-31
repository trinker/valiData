#' Apply Column Map to Dataframe
#'
#' Apply a column map from \code{read_column_map_dir} to a
#' \code{\link[base]{data.frame}}.
#'
#' @param data A data frame to run column-wise tests on.
#' @param colmap A column map from \code{read_column_map_dir}.
vc_column_apply <- function(data, colmap){

    nms_lookup <- dplyr::data_frame(
        nms = tolower(names(colmap)),
        colmapnams =names(colmap)
    )
# browser()
    locs <- match(tolower(colnames(data)), nms_lookup[['nms']])
    nms <- nms_lookup[['colmapnams']][locs]
    colnames(data)[!is.na(nms)] <- nms[!is.na(nms)]

    # colnames(data) <- tolower(colnames(data))
    # names(colmap) <- tolower(names(colmap))

    ## only check the headers that exist in both map and data
    map <- colmap[colnames(data)[colnames(data) %in% names(colmap)]]
    data <- data[names(map)]
# browser()
    Map(function(x, y, z){

# if (z == "EmailAddress1IsPreferred" ) browser()
        #y <-
        #gsub("\\)$", paste0("data, ", z, "\")"), y)
# if (tolower(z) == 'deliverymode') browser()
        replacement <- paste0("\\1", paste0("data, ", shQuote(z), ", \\2"))
        y <- gsub(",\\s*\\)", ")", gsub("(^[^\\(]+\\()(.+$)", replacement, y))

        invisible(lapply(y, function(w) {
            eval(parse(text=w))
            }))

    }, data, map, colnames(data))

}

