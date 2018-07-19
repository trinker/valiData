pacman::p_install_gh("trinker/valiData")
library(tidyverse)
clDev::source_R_folder()
# clDev::source_R_folder()





inst <- 'college'
file <- sprintf("C:\\Users\\trinker\\Desktop\\TestCore\\%s/Accounts/AccountImports/00_ACCOUNTS.csv", inst)
loc <- sprintf("C:\\Users\\trinker\\Desktop\\TestCore/%s", inst)
map_loc <- 'C:\\Users\\trinker\\Desktop\\TestCore\\bin/Core_Data_Dictionary_DS_longforms.xlsx'
map <- import_map(map_loc)

valiData(loc, map)
validate_file(file, "accounts" , map)

path <- loc

compare_column(
        path = path,
        parent.column='PersonIdentifier',
        parent='AccountImports',
        child = c('Enrollment', 'FacultyRemoval', 'Instructor', 'FacultyImport', 'StudentImport'),
        ignore.case = TRUE
        )
