pacman::p_install_gh("trinker/valiData")
library(tidyverse)
clDev::source_R_folder()
# clDev::source_R_folder()



inst <- 'Messiah'
file <- sprintf("C:\\Users\\trinker\\Desktop\\TestCore\\%s/Demographics/StudentImport/11.  Student Demographics.csv", inst)
loc <- sprintf("C:\\Users\\trinker\\Desktop\\TestCore/%s", inst)
map_loc <- 'C:\\Users\\trinker\\Desktop\\TestCore\\bin/Core_Data_Dictionary_DS_longforms.xlsx'
map <- import_map(map_loc)

valiData(loc, map)
validate_file(file, "studentimport" , map)

path <- loc


pp7 <- compare_column(
        path = path,
        parent.column='PersonIdentifier',
        parent='AccountImports',
        child = c('Enrollment', 'FacultyRemoval', 'Instructor', 'FacultyImport', 'StudentImport'),
        ignore.case = TRUE

    )
