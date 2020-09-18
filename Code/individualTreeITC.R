
# CHM based individual tree segmentation

# Import libraries
library(itcSegment)
library(scales)

data(imgData) #  import data
plot(imgData) # Plot image (Raster)
se<-itcIMG(imgData,epsg=32632) # Crown
summary(se) # Summary
plot(se,axes=T, add=TRUE, col=alpha("#99FF33",0.5)) # Overlay the crown polygons onto raster

