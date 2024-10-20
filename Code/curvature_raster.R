# Load necessary libraries
if (!require("terra")) install.packages("terra", dependencies = TRUE)
if (!require("spatialEco")) install.packages("spatialEco", dependencies = TRUE)
library(terra)
library(spatialEco)

# Sample elevation data (you can replace this with your own dataset)
elev <- rast(system.file("ex/elev.tif", package = "terra"))

# Project the raster to Robinson projection with specified resolution
elev_proj <- project(elev, "+proj=robin +datum=WGS84", res = 1000, method = "bilinear")

# Calculate curvature (planform and McNab)
planform_curvature <- curvature(elev_proj, type = "planform")
mcnab_curvature <- curvature(elev_proj, type = "mcnab")

# Plot the results
plot(mcnab_curvature, main = "McNab's Curvature (Projected)")

# Optional: Save the raster
writeRaster(mcnab_curvature, "mcnab_curvature.tif", overwrite = TRUE)
