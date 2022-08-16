
library(magick)

setwd("set/your/image/path/as/working/directory")
lst.file <- grep(list.files(path="set/your/image/path/"), 
                 pattern="*.HEIC", invert = FALSE, value = TRUE)

if (!dir.exists("JPG")) {dir.create("JPG")} # create new folder to export JPEG files

for (ls in 1:length(lst.file)) {
   path <- lst.file[ls]
  f.name <- tools::file_path_sans_ext(path)
  img <- image_read(path = path)
  img_jpg <- image_convert(img, "jpeg") # you can change jpeg to various format. Please see help file of Magick package
  #print(img_jpg)
  image_write(img_jpg, path = paste0("JPG/",f.name,".jpeg"))
}
