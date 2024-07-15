
# Code: Assist. Prof. Dr. Mustafa Zeybek
# Acu
# v1 21.09.2020
# v2 16/07/2024
# Updated Handheld RTK trajectory file from SWMAPS (HRTK)
# Geotag GPS coordinates into EXIF image data

# Load libs Do not CHANGE!

library(exifr)
library(dplyr)
library(dplyr)
library(lubridate)
#library(exiftoolr)
###################################

#Change
files <- list.files(path = "C:/Users/ASUS/Desktop/geotag-handheld/img", pattern = "*.jpg", full.names = TRUE)
#

dat <- read_exif(files)

img <- data.frame(img = dat$FileName, time=as.POSIXct(dat$CreateDate, format =  "%Y:%d:%m %H:%M:%S"))

# Change
koord <- read.csv("coordinates.csv",header = TRUE, sep = ";")
#koord <- koord[-79,]

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

# add timestamp to both
img <- img %>% mutate(time = ymd_hms(time))

#img$timestamp <- as.POSIXct(img$timestamp, "%Y-%m-%d %H:%M:%S")

koord <- koord %>% mutate(time = ymd_hms(time))

# join the two tables

str(img)
str(koord)

img2 <- img %>%  left_join(koord, by = c("time"))


na_rows <- img2[!complete.cases(img2), ]
ind <- data.frame(which(is.na(img2), arr.ind = TRUE))
indR <- unique(ind$row)

if ( nrow(na_rows) > 0) {
  for (i in c(1:length(indR))) {
    period_object <- difftime(na_rows$time[i], koord$time)
    period_object <- data.frame(t=as.numeric(abs(period_object), units = "secs"))
    na_values <- koord[which.min(period_object$t),]
    na_rows[i,c(3:9)] <- na_values[,c(2:8)] 
    img2[indR[i],] <- na_rows[i,]
    print(i)
  }
}

###set coordinates for jpg

for (i in 1:nrow(img2)) {
  Long.arg = paste("-GPSLongitude=",img2[i,4] ,sep = "")
  Lat.arg = paste("-GPSLatitude=",img2[i,5], sep = "")
  Alt.arg = paste("-GPSAltitude=",img2[i,6], sep = "")
  Horizontal.arg = paste("-GPSHPositioningError=",img2[i,7], sep = "")
  Vertical.arg = paste("-GPSVPositioningError=",img2[i,9], sep = "")
  
  exiftool_call(args = Long.arg, files[i]) #set longitude 
  exiftool_call(args = Lat.arg, files[i]) #set latitude
  exiftool_call(args = Alt.arg, files[i]) #set vertical
  exiftool_call(args = Horizontal.arg, files[i]) #set hor accuracy
  exiftool_call(args = Vertical.arg, files[i]) #set vert accuracy
}
