


## Concave hull around manual found TRee area

library(concaveman)
library(rgdal)
library(raster)

points <- read.csv("centers.txt")[,2:3]

points <- as.matrix(points)


tree.pts <- data.frame(x = points[,1], y = points[,2])


### Get long and lat from your data.frame. Make sure that the order is in lon/lat.

xy <- tree.pts[,1:2]

spdf <- SpatialPointsDataFrame(coords = xy,data=tree.pts, proj4string = CRS("+init=epsg:5254"))


# Write data to file ------------------------------------------------------


shapefile(spdf,"tree_centers",overwrite=TRUE )



# Working area concave hull -----------------------------------------------


polygon <- concaveman(points)

#plot(points)
#plot(polygon, add = TRUE, type = "l")

prj <- CRS("+init=epsg:5254")

sp <- SpatialPolygons(list(Polygons(list(Polygon(polygon)), ID=1)))

proj4string(sp)<- prj

class(sp)

# Write data to file ------------------------------------------------------


shapefile(sp,"B7-boundary",overwrite=TRUE )

