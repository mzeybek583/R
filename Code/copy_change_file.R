

## File operations

dirIn <- "E:/Selcuk/DERSLER/2021-2022_sinav/but/foto/questions/"

subdirName2 <- "E:/Selcuk/DERSLER/2022-2023_sinav/but/foto_but/questions/"

fl = list.files(dirIn, full.names = TRUE, pattern="*.rmd")
fl_id = list.files(dirIn, full.names = FALSE, pattern="*.rmd")

new_file_names <- paste0(subdirName2,"2021_but_",fl_id)

file.copy(from = fl, to = new_file_names, recursive = TRUE)
