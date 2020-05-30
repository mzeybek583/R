
## Convert GPS points and attiributes with projected coordinate system 
# Coder : Assist Prof. Dr. Mustafa Zeybek

library(stringr)
library(sp)
library(rgdal)
# Set working directory

#change the path
#setwd("E:/R/")

# Read raw data
gps <- read.csv(file = "../GPS/ACU-AGAC.txt", header = FALSE, sep = "")
gps

# Desriptions
desc <- as.character(gps$V5)

tur2 <- str_split(desc, "-")

# initialize
i=1
tur <- data.frame()

for (i in 1:length(tur2)) {
  #print(i)
  out <- tur2[[i]][1]
  out2 <- tur2[[i]][2]
  out3 <- tur2[[i]][3]
  tur[i,1] <- out
  tur[i,2] <- out2
  tur[i,3] <- out3
}

colnames(tur) <- c("Tur", "DBH", "Height")

x <- gps$V2; y <- gps$V3

export <- data.frame(cbind(x, y, tur))

#Create spatial object
coordinates(export) <- ~x + y

proj4string(export) <- CRS("+init=EPSG:5258") # Projection system

# Export
writeOGR(export, dsn="gps" ,layer = "map", driver = "ESRI Shapefile" )
