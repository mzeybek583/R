

# Yuzey Nivelmani R Kodlari

# Kutuphane ---------------------------------------------------------------

library(raster)
library(rasterVis)

# kotlar uretiliyor
set.seed(4208)
z <- rnorm(36, mean = 100, sd =0.1)
hist(z)

xy <- raster(ncol=6, nrow=6, xmn=100, xmx=125, ymn=100, ymx=125)
ncell(xy)
values(xy) <- z


# Kotlar ekrana tiklayinca yazdirilir. ------------------------------------
myClick <- function(x, n = Inf, id = FALSE, xy = FALSE, cell = FALSE, 
                    type = "n", show = TRUE, ...) {
  i <- 0
  n <- max(n, 1)
  while (i < n) {
    i <- i + 1
    loc <- locator(1, type, ...)
    xyCoords <- cbind(x = loc$x, y = loc$y)
    cells <- na.omit(cellFromXY(x, xyCoords))
    if (length(cells) == 0)
      break
    value <- extract(x, cells)
    text(xyCoords, labels = round(value, digits = 3))
  }
}

#myClick(xy, n=1)

plot(xy)


## Contour
xgrid <-  seq(xy$layer@extent[1], xy$layer@extent[2], (xy$layer@extent[2] - xy$layer@extent[1])/5)
ygrid <-  seq(xy$layer@extent[3], xy$layer@extent[4], (xy$layer@extent[4] - xy$layer@extent[3])/5)
mtrx3d <- matrix(z, ncol = 6)
contour(x = xgrid, y = ygrid, z = mtrx3d,add=T)

contour(xy)



# Contour plot ------------------------------------------------------------

mainCP <- contourplot(xy, pretty=TRUE, region =FALSE,colorkey = list(space='right'))
mainCP
levelplot(xy, col.regions = terrain.colors(50),
          contour = TRUE, region = TRUE, labels = TRUE,
          pretty = FALSE)
