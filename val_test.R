pacman::p_install_gh("trinker/valiData")

clDev::source_R_folder()
# clDev::source_R_folder()

wkd <- cl::l_drive_go("fdu")

file <- "C:\\Users\\trinker\\Desktop\\TestCore/uab/Accounts/AccountImports/accounts_University of Alabama at Birmingham_5_24_2017 10_14_07 AM.csv"

map_loc <- 'C:\\Users\\trinker\\Desktop\\TestCore\\bin/Core_Data_Dictionary_DS_longforms.xlsx'
map <- import_map(map_loc)


validate_file(file, "accountimports" , map)



