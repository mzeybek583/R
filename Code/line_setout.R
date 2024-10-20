# Hat Aplikasyonu
# Coder: Assist. Prof. Dr. Mustafa Zeybek

# Load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Set working directory (change to your path)
# setwd("Desktop/")  # Update to your actual working directory

# Function to calculate azimuth
calculate_azimuth <- function(pts) {
  delta_x <- pts$X[2] - pts$X[1]
  delta_y <- pts$Y[2] - pts$Y[1]
  
  azimuth <- atan(delta_x / delta_y) * 200 / pi
  if (delta_y == 0 && delta_x > 0) {
    azimuth <- 100
  } else if (delta_y == 0 && delta_x < 0) {
    azimuth <- 200
  } else if (delta_x > 0 && delta_y > 0) {
    azimuth <- azimuth
  } else if (delta_x > 0 && delta_y < 0) {
    azimuth <- azimuth + 200
  } else if (delta_x < 0 && delta_y < 0) {
    azimuth <- azimuth + 200
  } else {
    azimuth <- azimuth + 400
  }
  
  return(azimuth)
}

# Function to generate setting out points
generate_setting_out_points <- function(pts, interval = 10) {
  azimuth <- calculate_azimuth(pts)
  
  # Calculate total distance
  distance <- sqrt((pts$X[2] - pts$X[1])^2 + (pts$Y[2] - pts$Y[1])^2)
  
  # Generate points at intervals
  steps <- seq(interval, distance - 0.1, by = interval)
  apk <- data.frame(X = numeric(), Y = numeric())
  
  for (step in steps) {
    new_y <- pts$Y[1] + step * cos(azimuth * pi / 200)
    new_x <- pts$X[1] + step * sin(azimuth * pi / 200)
    apk <- rbind(apk, data.frame(X = new_x, Y = new_y))
  }
  
  return(apk)
}

# Function to plot setting out
plot_setting_out <- function(pts, apk) {
  ggplot() +
    geom_point(data = pts, aes(x = X, y = Y), color = "green", size = 3) +
    geom_line(data = pts, aes(x = X, y = Y), color = "blue", linetype = "dashed") +
    geom_point(data = apk, aes(x = X, y = Y), color = "red", size = 2) +
    labs(title = "Hat Aplikasyonu", x = "X Coordinate", y = "Y Coordinate") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dotted"),
          panel.grid.minor = element_line(color = "gray", linetype = "dotted"))
}

# Main execution
# Define input points
p1 <- c(455450.00, 4435670.00)
p2 <- c(455500.00, 4435676.00)
pts <- data.frame(rbind(p1, p2))
colnames(pts) <- c("X", "Y")

# Generate setting out points at 10 m intervals
apk <- generate_setting_out_points(pts, interval = 10)

# Plot the results
plot_setting_out(pts, apk)

# Export the coordinates
write.csv(format(round(apk, 3), nsmall = 3), "hat.dat", quote = FALSE, row.names = FALSE)
