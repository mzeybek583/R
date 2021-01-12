

## Coordinate Transformation from WGS84 to TUREF Projection


# Load Libs ---------------------------------------------------------------


library(rgdal)
#library(raster)
library(sp)

p <-  data.frame(x= 41.84932405683, y= 41.199137172149)

# Enter data --------------------------------------------------------------


p.xy = p[c("x", "y")]

coordinates(p.xy) < ~x + y

# Determine current Coord. Syst. ------------------------------------------


p <- SpatialPoints(p, proj4string = CRS("EPSG:4326"))
p
class(p)

plot(p, axes=T)

# Transform Data ----------------------------------------------------------


p2 <- spTransform(p, "EPSG:5258")

#Print
sprintf("%.3f",p2@coords)
