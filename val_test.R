pacman::p_install_gh("trinker/valiData")

clDev::source_R_folder()
# clDev::source_R_folder()

wkd <- cl::l_drive_go("fdu")

file <- "C:\\Users\\trinker\\Desktop\\TestCore\\Clarion\\Courses\\Course/Course.csv"
loc <- "C:\\Users\\trinker\\Desktop\\TestCore/IW Demographics"
map_loc <- 'C:\\Users\\trinker\\Desktop\\TestCore\\bin/Core_Data_Dictionary_DS_longforms.xlsx'
map <- import_map(map_loc)

valiData(loc, map)
validate_file(file, "course" , map)



