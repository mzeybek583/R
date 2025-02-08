# Load required libraries
library(rdist)
library(plotly)

# Generate 3D random points
set.seed(42)
df <- matrix(runif(300), ncol = 3)  # 100 points in 3D space

# Compute pairwise distance matrix
dist_mat <- pdist(df)

# Function for Farthest Point Sampling in 3D
farthest_point_sampling <- function(distance_matrix, num_samples = 10) {
  num_points <- nrow(distance_matrix)
  selected <- sample(1:num_points, 1)  # Start with a random point
  distances <- rep(Inf, num_points)    # Initialize distances to infinity
  
  for (i in 2:num_samples) {
    # Update distances based on the newly selected point
    distances <- pmin(distances, distance_matrix[selected[length(selected)], ])
    # Select the farthest point
    next_point <- which.max(distances)
    selected <- c(selected, next_point)
  }
  
  return(selected)
}

# Apply FPS
fps_indices <- farthest_point_sampling(as.matrix(dist_mat), num_samples = 10)

# Create a data frame for visualization
df_plot <- data.frame(
  x = df[, 1],
  y = df[, 2],
  z = df[, 3],
  type = "Original"
)
df_plot[fps_indices, "type"] <- "Sampled"

# Define colors
df_plot$color <- ifelse(df_plot$type == "Sampled", "red", "gray")

# 3D Scatter Plot with plotly
plot_ly(df_plot, x = ~x, y = ~y, z = ~z, color = ~color, colors = c("gray", "red"),
        type = "scatter3d", mode = "markers", marker = list(size = 5)) %>%
  layout(title = "Farthest Point Sampling (FPS) in 3D",
         scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z")))
