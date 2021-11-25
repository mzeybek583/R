# Reference: https://www.youtube.com/watch?v=WBfcR0zN0xk&list=PL4aUQR9L9RFrP7tNSM3m8-xXfGKTbRIND&index=11

setwd("/Your/Working/Directory")

library(rgdal)
library(raster)

# get bounding box of a single polygon 
shp = readOGR("./", "heart")
plot(shp)
bbox(shp)
bbox = spPolygons(extent(shp), crs = crs(shp))
plot(bbox)
lines(shp)
bbox$ID = 1
writeOGR(bbox, "./", "bbox", driver = "ESRI Shapefile")

# get bounding boxes of multiple polygons stored in a single shapefile
polys = readOGR("./", "flowers")
plot(polys)
bbox = spPolygons(extent(polys), crs = crs(polys))
lines(bbox)
boxes = SpatialPolygons(list())
for(i in polys$FID){
  poly = polys[polys$FID ==i,]
  bbox = spPolygons(extent(poly))
  boxes = bind(boxes, bbox)
}
crs(boxes)=crs(polys)
boxes$ID = seq(24)
plot(polys)
lines(boxes)
writeOGR(boxes, "./", "boxes", driver = "ESRI Shapefile")
