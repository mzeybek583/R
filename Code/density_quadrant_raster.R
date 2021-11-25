#reference: https://www.youtube.com/watch?v=ed-2Wu6z_vc&list=PL4aUQR9L9RFrP7tNSM3m8-xXfGKTbRIND&index=4

setwd("/Your/Working/Directory/pointpatterns/")

library(rgdal)
library(raster)
library(spatstat)
library(maptools)

# Load DEM as pixel image
dem = raster("DEM.tif")
elev = as.im(dem)

# Load spatial points
pts = readOGR(dsn = ".", layer = "points")

# convert to planar point pattern
pts.ppp = as.ppp(pts)
unitname(pts.ppp)= c("degree", "degrees")

# observation window from shapefile
mask = readOGR(dsn = ".", layer="mask")
win = as.owin(mask, unitname = "degrees")

# observation window from extent
win = owin(xrange=c(dem@extent[1:2]), 
           yrange = c(dem@extent[3:4]))
Window(pts.ppp)= win
pts.ppp = unmark(pts.ppp)

plot(layered(elev, pts.ppp, win), main = "Point locations on DEM")

# kernel smoothed intensity
den = density.ppp(pts.ppp, sigma = 0.01)
plot(den)

# export
r = raster(den)
writeRaster(r, "intensity.tif", overwrite = T)

# quadrat counting in spatstat
qq = quadratcount(pts.ppp, nx = 10, ny = 10)
plot(qq)

# quadrat counting using rasterize
r = raster(ext = dem@extent, resolution = 0.02)
qq = rasterize(coordinates(pts), r , fun = "count",
               background = 0)
plot(qq)
writeRaster(qq, "qq.tif", overwrite = T)
