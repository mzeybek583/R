# Load the necessary library
if (!require("lidR")) install.packages("lidR", dependencies = TRUE)
library(lidR)

# Define the file paths
input_file <- "table_scene_lms400.las"
output_file <- "table_scene_lms400_downsampled.las"

# Check if the input LAS file exists
if (!file.exists(input_file)) {
  stop("Input file not found. Please check the file path and ensure the file exists.")
}

# Read the point cloud data
cloud <- readLAS(input_file)

# Check if the point cloud was read successfully
if (is.null(cloud) || is.empty(cloud)) {
  stop("The point cloud data could not be read or is empty. Please check the file path and format.")
}

# Display the number of points before filtering
cat("PointCloud before filtering:", npoints(cloud), "data points.\n")

# Apply voxel grid filtering to downsample the point cloud
# Set voxel size to 0.1 meters for random downsampling
res <- 0.1
n <- 1
cloud_filtered <- decimate_points(cloud, random_per_voxel(res = res, n = n))

# Display the number of points after filtering
cat("PointCloud after filtering:", npoints(cloud_filtered), "data points.\n")

# Check if the filtering operation succeeded
if (is.null(cloud_filtered) || is.empty(cloud_filtered)) {
  stop("Filtering failed. The resulting point cloud is empty.")
}

# Write the filtered point cloud to a new LAS file
writeLAS(cloud_filtered, output_file)

# Confirm successful save operation
if (file.exists(output_file)) {
  cat("Filtered LAS file saved successfully as:", output_file, "\n")
} else {
  cat("Failed to save the filtered LAS file. Please check your file permissions and path.\n")
}
