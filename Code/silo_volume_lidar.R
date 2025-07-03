# ---- Silo 3D Surface Volume Analysis Script ----
# This script simulates a silo with a heaped surface, computes volumetric stats,
# and visualizes: 1) the top surface, 2) silo walls/base, and 3) a reverse cone.

# --- Load Required Packages ---
if (!require("geometry")) install.packages("geometry")
if (!require("interp")) install.packages("interp")
if (!require("rgl")) install.packages("rgl")
library(geometry)
library(interp)
library(rgl)

# --- Parameters ---
set.seed(1)
n <- 200                     # Number of random points for sample surface
radius <- 4                  # Silo radius (m)
silo_height <- 6             # Silo total height (m) from base to roof

# --- Simulate Surface Points (LIDAR/Point Cloud) ---
# Generate n points uniformly on a disk (the silo base), then assign heights z
theta <- runif(n, 0, 2*pi)
r <- sqrt(runif(n, 0, 1)) * radius
x <- r * cos(theta)
y <- r * sin(theta)
# Heaped surface: z = 5 - (x^2 + y^2) / (radius^2) + noise
z <- 5 - (x^2 + y^2) / (radius^2) + rnorm(n, 0, 0.1)
points_df <- data.frame(x, y, z)

# --- Reverse Cone Parameters ---
reverse_cone_radius <- 4      # meters, at base of cone
reverse_cone_depth  <- 1      # meters, cone apex below the base (z=0)

# --- Grid Interpolation for Surface Visualization ---
grid_res <- 0.03
xg <- seq(-radius, radius, by = grid_res)
yg <- seq(-radius, radius, by = grid_res)
grd <- interp::interp(points_df$x, points_df$y, points_df$z, xo = xg, yo = yg)
height_mat <- grd$z

# --- Mask Grid to Silo Circle (mathematical background: area integration) ---
grid_x <- matrix(rep(grd$x, each=length(grd$y)), nrow=length(grd$y))
grid_y <- matrix(rep(grd$y, length(grd$x)), nrow=length(grd$y), byrow=FALSE)
mask <- (grid_x^2 + grid_y^2) <= radius^2
valid_heights <- height_mat[mask]
cell_area <- grid_res^2

# --- Surface Statistics ---
surface_min <- min(valid_heights, na.rm = TRUE)      # Lowest surface point
surface_max <- max(valid_heights, na.rm = TRUE)      # Highest surface point
surface_mean <- mean(valid_heights, na.rm = TRUE)    # Average surface height
surface_sd <- sd(valid_heights, na.rm = TRUE)        # Surface roughness (stddev)

# --- Mathematical background: Surface Volume Calculation ---
# Approximates integral over the surface:
# V_surface = sum_over_grid( cell_area * height )
surface_volume <- sum(valid_heights, na.rm = TRUE) * cell_area

# --- Silo Total Cylinder Volume (for reference) ---
# V_cylinder = pi * r^2 * h
silo_cylinder_volume <- pi * radius^2 * silo_height

# --- Reverse Cone Volume (mathematical background) ---
# V_cone = (1/3) * pi * r^2 * h
reverse_cone_vol <- (1/3) * pi * reverse_cone_radius^2 * reverse_cone_depth

# --- Volume below surface (if min(z) > 0, material from base to min surface) ---
# Usually zero for realistic heap, but reported for completeness
# V_bottom_slice = pi * r^2 * max(0, min(surface))
bottom_slice_volume <- pi * radius^2 * max(0, surface_min)

# --- Total Filled Volume Calculation ---
# Surface volume + reverse cone volume
total_filled_volume <- surface_volume + reverse_cone_vol

# --- Empty Space (Air) Above the Material ---
empty_space_volume <- silo_cylinder_volume - total_filled_volume

# --- Prepare 3D Color Map for Surface ---
height_mat_plot <- height_mat
height_mat_plot[is.na(height_mat_plot)] <- min(height_mat, na.rm = TRUE)
ncol <- 100
colmap <- terrain.colors(ncol)
zcol <- colmap[cut(height_mat_plot, breaks = ncol, include.lowest = TRUE)]

# === 3D Visualization Section ===
open3d()
bg3d(color = "white")

# 1. Plot the interpolated surface (with RGB/terrain colors)
persp3d(grd$x, grd$y, height_mat_plot,
        col = zcol, xlab = "X (m)", ylab = "Y (m)", zlab = "Height (m)",
        main = "Silo with Cylinder and Reverse Cone", aspect = c(1, 1, 0.5), alpha = 0.9, add = FALSE)

# 2. Plot the silo wall (cylinder) and base
cylinder_segments <- 100
cylinder_theta <- seq(0, 2*pi, length.out = cylinder_segments)
for(i in 1:(length(cylinder_theta)-1)) {
  x_cyl <- c(radius*cos(cylinder_theta[i]), radius*cos(cylinder_theta[i+1]),
             radius*cos(cylinder_theta[i+1]), radius*cos(cylinder_theta[i]))
  y_cyl <- c(radius*sin(cylinder_theta[i]), radius*sin(cylinder_theta[i+1]),
             radius*sin(cylinder_theta[i+1]), radius*sin(cylinder_theta[i]))
  z_cyl <- c(0, 0, silo_height, silo_height)
  quads3d(x_cyl, y_cyl, z_cyl, col = "grey80", alpha = 0.2, front="lines")
}
x_base <- radius * cos(cylinder_theta)
y_base <- radius * sin(cylinder_theta)
z_base <- rep(0, length(x_base))
polygon3d(x_base, y_base, z_base, col="grey90", alpha=0.4)

# 3. Plot the reverse cone (downwards at center)
cone_segments <- 100
cone_theta <- seq(0, 2*pi, length.out=cone_segments)
x_cone <- reverse_cone_radius * cos(cone_theta)
y_cone <- reverse_cone_radius * sin(cone_theta)
z_cone <- rep(0, length(x_cone))
x_tip <- 0
y_tip <- 0
z_tip <- -reverse_cone_depth
for(i in 1:(length(cone_theta)-1)) {
  triangles3d(
    c(x_cone[i], x_cone[i+1], x_tip),
    c(y_cone[i], y_cone[i+1], y_tip),
    c(z_cone[i], z_cone[i+1], z_tip),
    col="royalblue", alpha=0.3
  )
}

# Axes and labels
axes3d()
title3d(xlab = "X (m)", ylab = "Y (m)", zlab = "Height (m)")

# === Print All Statistics and Mathematical Formulas ===
cat("==== Silo & Surface Statistics ====\n")
cat(sprintf("Silo radius (r): %.2f m\n", radius))
cat(sprintf("Silo height (h): %.2f m\n", silo_height))
cat(sprintf("Silo total capacity (V_cylinder = pi * r^2 * h): %.2f m3\n", silo_cylinder_volume))
cat(sprintf("Reverse cone radius (r): %.2f m\n", reverse_cone_radius))
cat(sprintf("Reverse cone depth (h): %.2f m\n", reverse_cone_depth))
cat(sprintf("Reverse cone volume (V_cone = (1/3) * pi * r^2 * h): %.2f m3\n", reverse_cone_vol))
cat("\n--- Surface (top of material) ---\n")
cat(sprintf("Min surface height: %.2f m\n", surface_min))
cat(sprintf("Max surface height: %.2f m\n", surface_max))
cat(sprintf("Mean surface height: %.2f m\n", surface_mean))
cat(sprintf("StdDev surface height: %.2f m\n", surface_sd))
cat(sprintf("Surface volume above z=0 (V_surface = sum grid cell_area * h): %.2f m3\n", surface_volume))
cat(sprintf("Volume below min surface (bottom slice, V = pi * r^2 * z_min): %.2f m3\n", bottom_slice_volume))
cat("\n--- Totals ---\n")
cat(sprintf("Total filled volume (surface + cone): %.2f m3\n", total_filled_volume))
cat(sprintf("Empty space (to full): %.2f m3\n", empty_space_volume))
cat(sprintf("Fill percentage: %.2f %%\n", 100 * total_filled_volume / silo_cylinder_volume))
cat("===================================\n")
