pacman::p_install_gh("trinker/valiData")
library(tidyverse)
clDev::source_R_folder()
# clDev::source_R_folder()





inst <- 'poo'
file <- sprintf("C:\\Users\\trinker\\OneDrive - Campus Labs LLC\\Desktop\\Data_Validation\\TestCore\\%s/Accounts/AccountImports/Account.csv", inst)
file.exists(file)
loc <- sprintf("C:\\Users\\trinker\\OneDrive - Campus Labs LLC\\Desktop\\Data_Validation\\TestCore/%s", inst)
map_loc <- 'C:\\Users\\trinker\\OneDrive - Campus Labs LLC\\Desktop\\Data_Validation\\TestCore\\bin/Core_Data_Dictionary_DS_longforms.xlsx'
map <- import_map(map_loc)

valiData(loc, map)
validate_file(file, "accountimports" , map)

path <- loc


pp7 <- compare_column(
        path = path,
        parent.column='PersonIdentifier',
        parent='AccountImports',
        child = c('Enrollment', 'FacultyRemoval', 'Instructor', 'FacultyImport', 'StudentImport'),
        ignore.case = TRUE

    )
