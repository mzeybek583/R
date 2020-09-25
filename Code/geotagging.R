
# Code: Assist. Prof. Dr. Mustafa Zeybek
# Acu
# 21.09.2020
# Geotag GPS coordinates into EXIF image data

#install.packages("exifr")
#install.packages("dplyr")

# Load libs Do not CHANGE!

library(exifr)
library(dplyr)
#library(exiftoolr)
###################################

#Change
files <- list.files(path = "C:/Users/LENOVO-7/Desktop/geotag/", pattern = "*.JPG", full.names = TRUE)
#


dat <- read_exif(files)
# Change
koord <- read.csv("resim_koor.TXT",header = FALSE)

#dat2 <- select(dat,
#               FileName,
#               GPSLongitude,
#               GPSLatitude,
#               GPSAltitude,
#               Roll,
#               Pitch,
#               Yaw,
#               RtkStdLon,
#               RtkStdLat,
#               RtkStdHgt)


###set coordinates for jpg


for (i in 1:nrow(koord)) {
  Long.arg = paste("-GPSLongitude=",koord[i,3] ,sep = "")
  Lat.arg = paste("-GPSLatitude=",koord[i,2], sep = "")
  Alt.arg = paste("-GPSAltitude=",koord[i,4], sep = "")

exiftool_call(args = Lat.arg, files[i]) #set latitude
exiftool_call(args = Long.arg, files[i]) #set longitude
exiftool_call(args = Alt.arg, files[i]) #set longitude

}

#  for (i in 1:nrow(koord)) {
# 
# exif_call(path = files, args = c("-csv=resim_koor.txt", "-gpslatituderef=N", "-gpslongituderef=E"))
# 
#  }
# #for (i in 1:nrow(koord)) {
#   exiftool_cmd <- paste("exiftool(-k) -GPSLongitude=",koord[i,2],"-GPSLatitude=",koord[i,3], "-GPSAltitude=",koord[i,4], " C:/Users/LENOVO-7/Desktop/geotag/", koord[i,1], sep ="" )
#   system(exiftool_cmd)
# }
# 
# 
# files <- list.files(path = "C:/Users/LENOVO-7/Desktop/geotag/", pattern = "*.JPG", full.names = TRUE)
# dat2 <- read_exif(files)
