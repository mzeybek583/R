

### Read SWMaps TRACK CSV file and convert to TUREF

# Load required library
library(terra)
library(tidyr)

# Read the CSV file
  raw <- read.csv("Rtk 2_TRACKPOINTS.csv", sep = ",", header = TRUE)

# Parse and convert latitude, longitude, and elevation to numeric
ID <- c(1:nrow(raw))
time <- data.frame(time=substr(raw$Time, 2,19))
time <- as.POSIXct(time$time, format =  "%d/%m/%Y %H:%M:%S")

Lat <- as.numeric(gsub(",", ".", gsub("\\.", "", raw$Lat)))
Long <- as.numeric(gsub(",", ".", gsub("\\.", "", raw$Lon)))
Z <- as.numeric(gsub(",", ".", gsub("\\.", "", raw$Elevation)))
Antenna<- 1.8
Z <- Z - Antenna
Lat_acc <- as.numeric(gsub(",", ".", gsub("\\.", "", raw$Horizontal.Accuracy)))
Lon_acc <- as.numeric(gsub(",", ".", gsub("\\.", "", raw$Horizontal.Accuracy)))
Z_acc <- as.numeric(gsub(",", ".", gsub("\\.", "", raw$Vertical.Accuracy)))

# Combine longitude and latitude into a matrix
lonlat <- cbind(Long, Lat)

# Create a spatial points vector with WGS84 coordinate reference system
crdref <- "+proj=longlat +datum=WGS84"
pts <- vect(lonlat, crs = crdref)

# Check the coordinate reference system of the points
print(crs(pts))

# Create a data frame with ID and Elevation
att <- data.frame(time=time,ID = ID, Z = Z, X_Acc= Lon_acc, Y_Acc= Lat_acc, Z_Acc= Z_acc)

# Add attributes to the points vector
pts <- vect(lonlat, atts = att, crs = crdref)

# Define the new coordinate reference system (TUREF)
newcrs <- "epsg:5255"

# Project the points to the new coordinate reference system
p_projected <- terra::project(pts, newcrs)

output <- data.frame(geom(p_projected))
export <- cbind(time,output[,c(1,3,4)],Z, Lat_acc, Lon_acc, Z_acc)

# Plot the projected points
plot(p_projected)

# Print the geometry of the projected points
print(geom(p_projected))

# Write the projected points to a shapefile
outfile <- "shp_out.shp"
writeVector(p_projected, outfile, overwrite = TRUE)
write.table(export,"coordinates.csv", sep = ";", row.names = FALSE)
