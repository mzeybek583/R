

## Code's for Landscape Engineering Surveying Course 
## Coder' Assist Prof. Dr. Mustafa Zeybek

library(raster)
library(sp)
library(rasterVis)

# Working directory
setwd("C:/Users/LENOVO-7/Desktop/")

# #Generate random data
# set.seed(4208)
# 
# z <- rnorm(10, mean=100, sd=40)
# 
# x <- rnorm(10, mean = 455100, sd= 50)
# 
# y <- rnorm(10, mean = 4452200, sd = 50)

veri <- read.csv(file = "veri.dat", header = TRUE, sep = ",")
  x <- veri$V1
  y <- veri$V2
  z <- veri$V3
plot(x, y)

text(x, y, format(round(z,2), nsmall=2), cex=1.5, pos=3,col="red")

data <- data.frame(cbind(x, y, z))

class(data)

coordinates(data) <- cbind(x,y)

grd <- expand.grid(x = seq(from = min(data$x),
                           to = max(data$x),
                           by = 10),
                   y = seq(from = min(data$y),
                           to = max(data$y), 
                           by = 10))  # expand points to grid
class(grd)
# Convert grd object to a spatial
# points object
coordinates(grd) <- ~x + y

# turn into a spatial pixels object
gridded(grd) <- TRUE
#### view grid with points overlayed
plot(grd, cex = 1.5, col = "grey")
plot(data, pch = 15, col = "red", cex = 1,add = TRUE)

library(gstat) # IDW fonksiyonu
# Veriyi enterpole et
# 2. mertebeden
P.idw <- gstat::idw(z~1, data, newdata=grd, idp=2.0)

r <- raster(P.idw)
crs(r) <- "+init=EPSG:5258"


plot(r, col= terrain.colors(100),
     legend.args=list(text='Elevation (m)',
                      side=3, font=2, line=1.0, cex=0.8),
     xlim = c(min(grd@coords[,1]),max(grd@coords[,1])),
     ylim = c(min(grd@coords[,2]),
              max(grd@coords[,2])))
contour(r, add=TRUE)
plot(data, add=TRUE)
text(x, y, format(round(z,2), nsmall=2), cex=0.75, pos=3,col="red")


## Plot 2    
# levelplot(r, col.regions = terrain.colors(100), margin=FALSE, contour=TRUE, region=TRUE,
#           labels=TRUE, pretty=TRUE)
# 
# contourplot(r, add =TRUE, margin=FALSE, pretty=TRUE)
# 

writeRaster(r,"elev.tif")

# write.csv(cbind(format(round(x,3),nsmall=3),
#                 format(round(y,3),nsmall=3),
#                 format(round(z,3),nsmall=3)), quote = FALSE,
#           "veri.dat")

