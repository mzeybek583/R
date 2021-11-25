#Reference: https://www.youtube.com/watch?v=_bzqAuBnOag&list=PL4aUQR9L9RFrP7tNSM3m8-xXfGKTbRIND&index=10

library(raster)
library(plyr)
library(ggplot2)
library(viridis)

set.seed(123)

# create empty raster
r = raster(xmn=800000,xmx =810000, ymn=7200000, 
           ymx=7210000, res = 1000)
crs(r) = CRS("+init=epsg:32719")

# generate random points
p = as(r@extent, 'SpatialPolygons')  
pts = spsample(p, n = 500, "random")
pts$value = runif(500, min=0, max=10)

# points to raster using rasterize()
meanr = rasterize(pts, r, "value", fun = mean)
plot(meanr)

# points to raster from scratch
df = as.data.frame(pts)
df$cutX = cut(df$x, breaks = seq(800000, 810000, 1000))
df$cutY = cut(df$y, breaks = seq(7200000, 7210000, 1000))

mVal = ddply(df, .(cutX, cutY), summarize, meanvalue = mean(value))
mVal$cutX = as.character(mVal$cutX)
mVal$cutX = unlist(strsplit(mVal$cutX, ","))[c(T,F)]
mVal$cutX = as.numeric(sub("\\(", "", mVal$cutX))+500
mVal$cutY = as.character(mVal$cutY)
mVal$cutY = unlist(strsplit(mVal$cutY, ","))[c(T,F)]
mVal$cutY = as.numeric(sub("\\(", "", mVal$cutY))+500

coordinates(mVal) = ~cutX +cutY
gridded(mVal) = T
meanr = raster(mVal)
crs(meanr) = CRS("+init=epsg:32719")
plot(meanr)
