
## Convert Focus 2 Total station points and attiributes with projected coordinate system 
# Coder : Assist Prof. Dr. Mustafa Zeybek

library(stringr)
library(sp)
library(rgdal)
# Set working directory

#change the path
#setwd("E:/R/")

# Read raw data
veri <- read.csv(file = "../total_agac/coord_agac_total-15_2.txt", header = FALSE, sep = ",")
head(veri)

# Desriptions
desc <- as.character(veri$V5)

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

x <- veri$V2; y <- veri$V3; z <- rep(915, nrow(veri));

export <- data.frame(cbind(veri$V1, x, y, z, tur))

#Create spatial object
coordinates(export) <- ~x + y

proj4string(export) <- CRS("+init=EPSG:5258") # Projection system

# Export
writeOGR(export, dsn="coord" ,overwrite_layer = TRUE,layer = "total", driver = "ESRI Shapefile" )
write.csv(export, file = "coord_processed.txt")
