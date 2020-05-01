#' Convert Directory of Predictives Files to Core Folder Structure
#'
#' Convert a single directory of .csv files to a directory of core data folders
#' and files.
#'
#' @param path A path to the directory where the predictives files reside.
#' @param institution A string representing the institution's name.
#' @param first.import logical. If `TRUE` then StudentImportPrior.csv file is not
#' required, otherwise a hard stop occurs if this file is missing.
#' @param exdir The directory to extract files to. NOTE that this folder will be
#' deleted and then recreated.  You will lose any data cotnained within.
#' Defaults to creating a folder named the same as `institution` in the system's
#' temp directory.
#' @param \ldots ignored.
#' @return Returns the path to exdir for easy piping.
#' @export
#' @examples
#' \dontrun{
#' temp_loc <- as_core_structure(
#'     path = 'data/tyler_fake',
#'     institution = 'HomeTown',
#'     first.import = TRUE
#' )
#'
#' cl::opener(temp_loc)
#' }
convert_to_core_structure <- function(path, institution = 'Institution', first.import = FALSE,
    exdir = file.path(tempdir(), institution), ...){

    stopifnot(dir.exists(path))

    ## Ensure no additional directories are found inside of the path folder
    if (!isTRUE(all.equal(dir(path), dir(path, include.dirs = TRUE)))){
        stop(sprintf('There are additional folders in `%s`...Please remove and try again.', basename(path)))
    }

    ## Ensure only .csv files are found inside of the path folder
    if (!all(tools::file_ext(dir(path)) %in% 'csv')){
        stop(sprintf('There are non .csv files in `%s`...Please remove and try again.', basename(path)))
    }

    unmatched <- dir(path)[!tolower(dir(path)) %in% tolower(dir_to_filename_map$filename)]
    if (length(unmatched) > 0){
        stop(sprintf(
            "The following files do not match a an expected name:\n\n%s\n\nPlease remove/rename them and try again.",
            paste(paste0('  -', unmatched), collapse = '\n')
        ))
    }


    ## Remove prior creation of the parent directory then recreate it
    if (dir.exists(exdir)) unlink(exdir, recursive = TRUE, force = TRUE)
    dir.create(exdir, recursive = TRUE)
    stopifnot(dir.exists(exdir))

    ## Folders from core data to re-create
    fldrs <- file.path(exdir,  dir_to_filename_map[['dir']])

    ## Create the possible core data folder structure
    for (i in fldrs) dir.create(i, recursive = TRUE)

    ## Contents of the datafiles
    fls <- dir(path, pattern = '.csv', full.names = TRUE)

    ## Loop through and move files that match filename in map to appropriate folder
    for(i in seq_len(nrow(dir_to_filename_map))){

        r <- dir_to_filename_map[i, ]

        destination <- file.path(exdir, r[['dir']])
        match_name <- tolower(r[['filename']])

        potential_names <- tolower(basename(fls))
        is_hit <- potential_names %in% match_name
        hit <- potential_names[is_hit]

        ## Error handling-------------------------------------------------------
        if (length(hit) > 1) {
            stop(sprintf('There are more than one `%s` files detected', match_name))
        }

        if (length(hit) < 1) {

            if (r[['required']]){

                if (match_name == 'studentimportprior.csv') {

                    if (!first.import) {

                        stop(sprintf(paste0(
                                '`%s` file required but not detected\n\nIf this is the first time',
                                ' importing for this institution, then set: first.import = TRUE'
                            ), match_name)
                        )

                    } else {

                        message(sprintf('`%s` file not required on first import but will be needed next import', match_name))

                    }

                } else {

                    stop(sprintf('`%s` file required but not detected', match_name))
                }

            } else {
                    message(sprintf('`%s` file not required and not detected', match_name))
            }

        } else {
        ## End of Error handling-------------------------------------------------------

            file.copy(fls[is_hit], destination)
            if (file.exists(file.path(destination, basename(fls[is_hit])))){

                message(sprintf('`%s` file added to `%s` folder', match_name, institution))

            } else {

                stop(sprintf('`%s` file was not added to `%s` folder', match_name, institution))

            }

        }

    }


    return(exdir)

}

dir_to_filename_map <- tibble::tribble(
                                 ~dir,                ~filename,    ~required,
            "Courses/AcademicProgram",    "AcademicProgram.csv",        FALSE,
               "Courses/AcademicTerm",       "AcademicTerm.csv",         TRUE,
            "Accounts/AccountImports",           "Accounts.csv",         TRUE,
                     "Courses/Course",             "Course.csv",        FALSE,
               "Courses/CrossListing",       "CrossListing.csv",        FALSE,
                 "Courses/Enrollment",         "Enrollment.csv",         TRUE,
         "Demographics/FacultyImport",      "FacultyImport.csv",        FALSE,
                 "Courses/Instructor",         "Instructor.csv",        FALSE,
                    "Courses/OrgUnit",            "OrgUnit.csv",        FALSE,
           "Courses/RemoveInstructor",   "RemoveInstructor.csv",        FALSE,
                    "Courses/Section",            "Section.csv",         TRUE,
           "Courses/SectionAttribute",   "SectionAttribute.csv",        FALSE,
         "Demographics/StudentImport",      "StudentImport.csv",         TRUE,
    "Demographics/StudentImportPrior", "StudentImportPrior.csv",         TRUE
)
