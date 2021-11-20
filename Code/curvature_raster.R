# NOT RUN {
library(raster)
library(spatialEco)
data(elev)
elev <- projectRaster(elev, crs="+proj=robin +datum=WGS84", 
                      res=1000, method='bilinear')
curvature(elev, type="planform")
mcnab.crv <- curvature(elev, type="mcnab")
plot(mcnab.crv, main="McNab's curvature") 
# }
# NOT RUN {

# }
