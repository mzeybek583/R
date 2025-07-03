# ---- Silo 3D Surface Volume Analysis Script ----
# Simulates a silo with a heaped surface, computes volumetric stats,
# visualizes the 3D surface, cylinder, base, reverse cone, 
# and marks min/max surface points (on original data).
# Now includes engineering warnings for nearly full and below-base conditions.

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
theta <- runif(n, 0, 2*pi)
r <- sqrt(runif(n, 0, 1)) * radius
x <- r * cos(theta)
y <- r * sin(theta)
z <- 3 - (x^2 + y^2) / (radius^2) + rnorm(n, 0, 0.1)
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

# --- Mask Grid to Silo Circle ---
grid_x <- matrix(rep(grd$x, each=length(grd$y)), nrow=length(grd$y))
grid_y <- matrix(rep(grd$y, length(grd$x)), nrow=length(grd$y), byrow=FALSE)
mask <- (grid_x^2 + grid_y^2) <= radius^2
valid_heights <- height_mat[mask]
cell_area <- grid_res^2

# --- Surface Statistics (from grid) ---
surface_min <- min(valid_heights, na.rm = TRUE)
surface_max <- max(valid_heights, na.rm = TRUE)
surface_mean <- mean(valid_heights, na.rm = TRUE)
surface_sd <- sd(valid_heights, na.rm = TRUE)
surface_volume <- sum(valid_heights, na.rm = TRUE) * cell_area

# --- Silo volumes ---
silo_cylinder_volume <- pi * radius^2 * silo_height
reverse_cone_vol <- (1/3) * pi * reverse_cone_radius^2 * reverse_cone_depth
bottom_slice_volume <- pi * radius^2 * max(0, surface_min)
total_filled_volume <- surface_volume + reverse_cone_vol
empty_space_volume <- silo_cylinder_volume - surface_volume
full_capacity <- silo_cylinder_volume + reverse_cone_vol

# --- Prepare 3D Color Map for Surface ---
height_mat_plot <- height_mat
height_mat_plot[is.na(height_mat_plot)] <- min(height_mat, na.rm = TRUE)
ncol <- 100
colmap <- rainbow(ncol) # or terrain.colors(ncol)
zcol <- colmap[cut(height_mat_plot, breaks = ncol, include.lowest = TRUE)]

# === 3D Visualization Section ===
open3d()
bg3d(color = "white")

# 1. Plot the interpolated colored surface
persp3d(grd$x, grd$y, height_mat_plot,
        col = zcol, xlab = "X (m)", ylab = "Y (m)", zlab = "Height (m)",
        main = "Silo with Cylinder and Reverse Cone", aspect = c(1, 1, 0.5), alpha = 0.9, add = FALSE)

# --- Min/max markers from original point cloud (recommended for reporting) ---
ix_min <- which.min(points_df$z)
ix_max <- which.max(points_df$z)
spheres3d(points_df$x[ix_min], points_df$y[ix_min], points_df$z[ix_min], radius=0.18, color="navy")
text3d(points_df$x[ix_min], points_df$y[ix_min], points_df$z[ix_min], 
       texts = sprintf("Min: %.2f m", points_df$z[ix_min]), adj = c(1,1), col="navy", cex=1.4)
spheres3d(points_df$x[ix_max], points_df$y[ix_max], points_df$z[ix_max], radius=0.18, color="darkred")
text3d(points_df$x[ix_max], points_df$y[ix_max], points_df$z[ix_max], 
       texts = sprintf("Max: %.2f m", points_df$z[ix_max]), adj = c(1,1), col="darkred", cex=1.4)

# 2. Plot the silo wall and base
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

# --- ADD COLOR LEGEND FOR SURFACE HEIGHT ---
n_legend <- 8
breaks_legend <- seq(surface_min, surface_max, length.out = n_legend)
legend_colors <- colmap[cut(breaks_legend, breaks = seq(surface_min, surface_max, length.out = ncol+1), labels = FALSE, include.lowest = TRUE)]
legend_labels <- sprintf("%.2f", breaks_legend)
legend3d("topright", legend = legend_labels, fill = legend_colors, cex = 1.0, inset = c(0.02), title = "Surface Height (m)", bty = "n")

# === Print All Statistics and Mathematical Formulas ===
cat("==== Silo & Surface Statistics ====\n")
cat(sprintf("Silo radius (r): %.2f m\n", radius))
cat(sprintf("Silo height (h): %.2f m\n", silo_height))
cat(sprintf("Silo total capacity: %.2f m\n ", full_capacity))
cat(sprintf("Reverse cone radius (r): %.2f m\n", reverse_cone_radius))
cat(sprintf("Reverse cone depth (h): %.2f m\n", reverse_cone_depth))
cat(sprintf("Reverse cone volume (V_cone = (1/3) * pi * r^2 * h): %.2f m3\n", reverse_cone_vol))
cat("\n--- Surface (top of material) ---\n")
cat(sprintf("Min surface height (grid): %.2f m\n", surface_min))
cat(sprintf("Max surface height (grid): %.2f m\n", surface_max))
cat(sprintf("Min height (raw): %.2f m\n", points_df$z[ix_min]))
cat(sprintf("Max height (raw): %.2f m\n", points_df$z[ix_max]))
cat(sprintf("Mean surface height: %.2f m\n", surface_mean))
cat(sprintf("StdDev surface height: %.2f m\n", surface_sd))
cat(sprintf("Surface volume above z=0 (V_surface = sum grid cell_area * h): %.2f m3\n", surface_volume))
cat(sprintf("Volume below min surface (bottom slice, V = pi * r^2 * z_min): %.2f m3\n", bottom_slice_volume))
cat("\n--- Totals ---\n")
cat(sprintf("Total filled volume (surface + cone): %.2f m3\n", total_filled_volume))
cat(sprintf("Empty space (to full): %.2f m3\n", empty_space_volume))
cat(sprintf("Fill percentage: %.2f %%\n", 100 * total_filled_volume / full_capacity))
cat("===================================\n")

# --- ENGINEERING WARNINGS BASED ON SURFACE HEIGHTS ---
# 1. Max surface height close to top of silo
distance_to_roof <- silo_height - surface_max
if (distance_to_roof < 0.5) {
  cat(sprintf("\n*** WARNING: Maximum surface is within %.2f m of silo roof (nearly full: max surface = %.2f m, silo height = %.2f m) ***\n",
              distance_to_roof, surface_max, silo_height))
  warning(sprintf("Maximum surface is within %.2f m of silo roof (max=%.2f, silo height=%.2f)",
                  distance_to_roof, surface_max, silo_height))
}
# 2. Any surface point below base
if (surface_min < 0) {
  cat(sprintf("\n*** WARNING: Minimum surface height is BELOW THE BASE! (min surface = %.2f m) ***\n", surface_min))
  warning(sprintf("Minimum surface height is BELOW THE BASE! (min surface = %.2f m)", surface_min))
}
