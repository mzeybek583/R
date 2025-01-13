# Load necessary libraries
library(akima)    # For interpolation
library(ggplot2)  # For visualization
library(reshape2) # For reshaping data

# Step 1: Generate random point clouds for two surveys
set.seed(123)
n_points <- 100

# Survey 1: Generate random points
point_cloud1 <- data.frame(
  x = runif(n_points, 0, 10),
  y = runif(n_points, 0, 10),
  z = runif(n_points, 100, 200)  # Elevation at T1
)

# Survey 2: Simulate deformation (add noise)
point_cloud2 <- data.frame(
  x = point_cloud1$x,
  y = point_cloud1$y,
  z = point_cloud1$z + rnorm(n_points, mean = 2, sd = 5)  # Add deformation
)

# Step 2: Define grid and interpolate
grid_spacing <- 1
x_range <- seq(0, 10, by = grid_spacing)
y_range <- seq(0, 10, by = grid_spacing)

# Function to interpolate point cloud to grid
interpolate_to_grid <- function(point_cloud, x_range, y_range) {
  interp_result <- interp(
    x = point_cloud$x,
    y = point_cloud$y,
    z = point_cloud$z,
    xo = x_range,
    yo = y_range,
    linear = TRUE
  )
  grid <- interp_result$z
  grid[is.na(grid)] <- mean(grid, na.rm = TRUE)  # Fill missing values
  return(grid)
}

# Interpolate point clouds into grids
grid1 <- interpolate_to_grid(point_cloud1, x_range, y_range)
grid2 <- interpolate_to_grid(point_cloud2, x_range, y_range)

# Step 3: Compute tilt for each survey
calculate_tilts <- function(elevation, NS) {
  nrows <- nrow(elevation)
  ncols <- ncol(elevation)
  
  tn <- matrix(NA, nrow = nrows - 1, ncol = ncols)
  te <- matrix(NA, nrow = nrows, ncol = ncols - 1)
  
  for (i in 1:(nrows - 1)) {
    for (j in 1:ncols) {
      tn[i, j] <- (elevation[i + 1, j] - elevation[i, j]) / NS
    }
  }
  
  for (i in 1:nrows) {
    for (j in 1:(ncols - 1)) {
      te[i, j] <- (elevation[i, j + 1] - elevation[i, j]) / NS
    }
  }
  
  return(list(northward_tilt = tn, eastward_tilt = te))
}

# Compute tilts for both surveys
tilts1 <- calculate_tilts(grid1, grid_spacing)
tilts2 <- calculate_tilts(grid2, grid_spacing)

# Step 4: Calculate tilt differences
tilt_diff_n <- tilts2$northward_tilt - tilts1$northward_tilt
tilt_diff_e <- tilts2$eastward_tilt - tilts1$eastward_tilt

# Step 5: Statistical Testing for Each Grid Cell
# Initialize matrices for significance results
significance_n <- matrix(NA, nrow = nrow(tilt_diff_n), ncol = ncol(tilt_diff_n))
significance_e <- matrix(NA, nrow = nrow(tilt_diff_e), ncol = ncol(tilt_diff_e))

# Perform t-test for each cell
for (i in 1:nrow(tilt_diff_n)) {
  for (j in 1:ncol(tilt_diff_n)) {
    t_test_result <- t.test(c(tilts1$northward_tilt[i, j], tilts2$northward_tilt[i, j]))
    significance_n[i, j] <- ifelse(t_test_result$p.value < 0.05, 1, 0)  # 1 if significant, 0 otherwise
  }
}

for (i in 1:nrow(tilt_diff_e)) {
  for (j in 1:ncol(tilt_diff_e)) {
    t_test_result <- t.test(c(tilts1$eastward_tilt[i, j], tilts2$eastward_tilt[i, j]))
    significance_e[i, j] <- ifelse(t_test_result$p.value < 0.05, 1, 0)  # 1 if significant, 0 otherwise
  }
}

# Step 6: Visualize Significance Results
# Convert significance matrices to data frames for plotting
north_sig_df <- melt(significance_n, varnames = c("Row", "Column"), value.name = "Significant")
east_sig_df <- melt(significance_e, varnames = c("Row", "Column"), value.name = "Significant")

# Create significance heatmaps
ggplot(north_sig_df, aes(x = Column, y = -Row, fill = as.factor(Significant))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("Not Significant", "Significant")) +
  ggtitle("Significance of Northward Tilt Differences") +
  theme_minimal()

ggplot(east_sig_df, aes(x = Column, y = -Row, fill = as.factor(Significant))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("Not Significant", "Significant")) +
  ggtitle("Significance of Eastward Tilt Differences") +
  theme_minimal()
