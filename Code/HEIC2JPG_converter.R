
library(magick)

setwd("E:/DR_Sonrasi_Projeler/TurkLiDAR/iphone_road/drive-download-20220816T180142Z-001/")
lst.file <- grep(list.files(path="E:/DR_Sonrasi_Projeler/TurkLiDAR/iphone_road/drive-download-20220816T180142Z-001/"), 
                 pattern="*.HEIC", invert = FALSE, value = TRUE)

if (!dir.exists("JPG")) {dir.create("JPG")}

for (ls in 1:length(lst.file)) {
  #ls <- 1
  path <- lst.file[ls]
  f.name <- tools::file_path_sans_ext(path)
  img <- image_read(path = path)
  img_jpg <- image_convert(img, "jpeg")
  #print(img_jpg)
  image_write(img_jpg, path = paste0("JPG/",f.name,".jpeg"))
}
