# Reference : https://www.r-spatial.org/r/2017/12/21/geoms.html
library(sf)

## Loading required package: methods

## Linking to GEOS 3.5.1, GDAL 2.1.2, proj.4 4.9.3

pol = st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
b = st_sfc(pol, pol + c(.9, .2), pol + c(.2, .9))
#par(mar = rep(0, 4))
plot(b,col = sf.colors(3, categorical = TRUE), axes = TRUE)

i = st_intersection(st_sf(b))
#par(mar = rep(0, 9))
cl = sf.colors(3, categorical = TRUE)
plot(b)
plot(i[i$n.overlaps == 2,], col = cl[1], add = TRUE, axes=TRUE)
plot(i[i$n.overlaps == 3,], col = cl[2], add = TRUE, axes=TRUE)

(i = st_intersection(b))

d = st_difference(b)
plot(d, col = cl, axes=TRUE)
st_overlaps(i)
st_overlaps(d)
