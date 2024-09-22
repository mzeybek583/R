# Load necessary libraries
library(rgl)
library(MASS)   # For robust regression (rlm) and robust covariance (cov.rob)
library(rrcov)  # For robust PCA

# Simulate 3D point cloud (for demonstration)
set.seed(123)
n <- 1000

# Parameters for the point cloud (e.g., simulating a beam-like structure with random noise)
length_beam <- 20
width_beam <- 2
height_beam <- 2

x <- runif(n, -length_beam / 2, length_beam / 2)
y <- rnorm(n, mean = 0, sd = width_beam / 5)
z <- rnorm(n, mean = 0, sd = height_beam / 5)

# Add some noise and a few outliers to the points to simulate a real-world dataset
point_cloud <- cbind(x, y, z)

# Adding outliers
outliers <- matrix(rnorm(30, mean = 10), ncol = 3)
point_cloud <- rbind(point_cloud, outliers)

# Visualize the 3D point cloud with outliers
plot3d(point_cloud[, 1], point_cloud[, 2], point_cloud[, 3], col = "blue", size = 5)

# Perform Robust PCA using MCD (Minimum Covariance Determinant)
robust_pca <- PcaHubert(point_cloud, k = 3)  # k is the number of principal components

# Get the robust neutral axis (first robust principal component)
robust_neutral_axis <- robust_pca@loadings[, 1]  # First robust principal component

# Print the robust neutral axis
cat("Robust neutral axis (first robust principal component):\n", robust_neutral_axis, "\n")

# Step 2: Robust Regression to Refine the Neutral Axis
# Project the points onto the neutral axis
projected_points <- point_cloud %*% robust_neutral_axis

# Perform robust regression on the projected points
# We aim to fit a line (y = m * x + b) along the neutral axis direction
robust_fit <- rlm(projected_points ~ point_cloud[, 1])  # Robust linear regression

# Extract the coefficients for the fitted line (this refines the neutral axis)
m <- coef(robust_fit)[2]  # Slope
b <- coef(robust_fit)[1]  # Intercept

# Display robust fit results
cat("Refined neutral axis fit (robust regression):\n")
cat("Slope (m):", m, "\n")
cat("Intercept (b):", b, "\n")

# Visualization of the robust neutral axis and the refined fit
center_of_cloud <- colMeans(point_cloud)  # Mean of the point cloud (centroid)

# Create points along the robust neutral axis for visualization
line_length <- 20  # Length of the line for visualizing the neutral axis
neutral_line <- rbind(center_of_cloud - line_length * robust_neutral_axis, 
                      center_of_cloud + line_length * robust_neutral_axis)

# Add the robust neutral axis to the 3D plot
lines3d(neutral_line[, 1], neutral_line[, 2], neutral_line[, 3], col = "red", lwd = 3)

# Add the refined robust fit line (based on robust regression)
fitted_line <- cbind(point_cloud[, 1], m * point_cloud[, 1] + b, 0)  # Only consider 2D for now
lines3d(fitted_line[, 1], fitted_line[, 2], fitted_line[, 3], col = "green", lwd = 3)
