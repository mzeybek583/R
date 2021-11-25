Reference: https://www.youtube.com/watch?v=2lB2c_FTxpI&list=PL4aUQR9L9RFrP7tNSM3m8-xXfGKTbRIND&index=9
setwd("/Your/Working/Directory/ExtractProfile/")

library(raster)
library(ggplot2)
library(rgdal)
library(spatstat)
library(maptools)

# load raster and convert to dataframe
r = raster("dem.tif")
df = as.data.frame(r, xy = T)

# create a subset along a single y coordinate
df = df[df$y == median(df$y),]

# plot
ggplot(data = df, aes(x = x-min(x), y = dem, color = dem))+
  geom_point()+
  theme_light()+
  scale_color_gradientn(colors = terrain.colors(10))+
  labs(x = "Distance along profile [m]", y = "Elevation [m]",
       color = "Elevation [m]")

# import a profile line
line = readOGR("./", "profile")
plot(r)
lines(line)

# convert to point segment pattern and get pointsalong line
psp = as.psp.SpatialLinesDataFrame(line)
pts = pointsOnLines(psp, eps = 10)
points(pts)

# convert to spatial points
pts = SpatialPoints(coords = cbind(pts$x, pts$y), 
                    proj4string = CRS("+init=epsg:32719"))
pdf = as.data.frame(pts)

# extract elevation
pdf$dem = extract(r, pts)
# add distance along profile
pdf$dist = seq(0, (nrow(pdf)-1)*10, 10)

# plot
ggplot(data = pdf, aes(dist, dem, color = dem))+
  geom_point()+
  theme_light()+
  scale_color_gradientn(colors = terrain.colors(10))+
  labs(x = "Distance along profile [m]", y = "Elevation [m]",
       color = "Elevation [m]")
