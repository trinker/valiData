pacman::p_install_gh("trinker/valiData")

clDev::source_R_folder()
# clDev::source_R_folder()

wkd <- cl::l_drive_go("fdu")

file <- "C:\\Users\\trinker\\Desktop\\TestCore\\JCSU/Accounts/AccountImports/00_ACCOUNTS.csv"
loc <- "C:\\Users\\trinker\\Desktop\\TestCore/JCSU"
map_loc <- 'C:\\Users\\trinker\\Desktop\\TestCore\\bin/Core_Data_Dictionary_DS_longforms.xlsx'
map <- import_map(map_loc)

valiData(loc, map)
validate_file(file, "accounts" , map)

path <- "C:/Users/trinker/Desktop/TestCore/Daemen"


compare_column(
            path = path,
            parent.column='OrgUnitIdentifier',
            parent='OrgUnit',
            child = c('Course', 'Section'),
            ignore.case = TRUE,
            ignore.element.case = TRUE
        )
