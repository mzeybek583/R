# Load necessary libraries
if (!require("lidR")) install.packages("lidR", dependencies = TRUE)
if (!require("sf")) install.packages("sf", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("viridis")) install.packages("viridis", dependencies = TRUE)
library(lidR)
library(sf)
library(ggplot2)
library(viridis)

# Load sample LAS file provided by lidR package
las_file <- system.file("extdata", "MixedConifer.laz", package = "lidR")
las <- readLAS(las_file)

# Check if the LAS file was loaded successfully
if (is.empty(las)) {
  stop("Error: LAS file could not be loaded. Please check the file path and data.")
}

# Step 1: Detect individual trees in the point cloud
tree_tops <- locate_trees(las, lmf(ws = 5, hmin = 2))

# Step 2: Perform tree segmentation using Dalponte's method
chm <- rasterize_canopy(las, res = 1, algorithm = pitfree())
las <- segment_trees(las, dalponte2016(chm, tree_tops))

# Step 3: Generate Canopy Height Model (CHM) for visualization
chm <- rasterize_canopy(las, res = 1, algorithm = pitfree())

# Step 4: Convert the tree segmentation into spatial features
tree_crowns <- crown_metrics(las, func = .stdmetrics)
tree_crowns_sf <- st_as_sf(tree_crowns, coords = c("X", "Y"), crs = 4326)

# Function to plot CHM using ggplot2
plot_chm <- function(chm) {
  chm_df <- as.data.frame(raster::as.data.frame(chm, xy = TRUE, na.rm = TRUE))
  colnames(chm_df) <- c("x", "y", "height")  # Explicitly set column names
  ggplot() +
    geom_raster(data = chm_df, aes(x = x, y = y, fill = height)) +
    scale_fill_viridis_c(option = "viridis") +
    theme_minimal() +
    labs(title = "Canopy Height Model (CHM)", x = "Longitude", y = "Latitude")
}

# Plot the CHM data
plot_chm(chm)

# Function to overlay tree crowns on CHM
plot_trees_on_chm <- function(chm, tree_crowns_sf) {
  chm_df <- as.data.frame(raster::as.data.frame(chm, xy = TRUE, na.rm = TRUE))
  colnames(chm_df) <- c("x", "y", "height")  # Ensure consistent column names
  ggplot() +
    geom_raster(data = chm_df, aes(x = x, y = y, fill = height)) +
    geom_sf(data = tree_crowns_sf, fill = NA, color = "green", size = 0.5) +
    scale_fill_viridis_c(option = "viridis") +
    theme_minimal() +
    labs(title = "CHM with Tree Crowns Segmentation", x = "Longitude", y = "Latitude")
}

# Show summary of the segmented trees
summary(tree_crowns_sf)

# Plot tree crowns over CHM
plot_trees_on_chm(chm, tree_crowns_sf)
