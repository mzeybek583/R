# Load necessary libraries
if (!require("maps")) install.packages("maps", dependencies = TRUE)
if (!require("sf")) install.packages("sf", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
library(maps)
library(sf)
library(ggplot2)
library(dplyr)

# Convert map data of Turkey to sf object
tr <- st_as_sf(map("world", "Turkey", plot = FALSE, fill = TRUE))

# Define Lambert equal area projection (laea)
laea <- st_crs("+proj=laea +lat_0=30 +lon_0=20")  # Lambert equal area

# Transform Turkey map to Lambert equal area
tr <- st_transform(tr, crs = laea)

# Create a graticule for the map
g <- st_graticule(tr, lon = seq(24, 48, 3), lat = seq(34, 44, 2))

# Plot the transformed map with graticules using base R plot
plot(st_geometry(tr), axes = TRUE, main = "Transformed Map of Turkey with Graticule")
plot(st_geometry(g), add = TRUE, col = "gray", lty = "dotted")

# Define points to plot (e.g., study area points)
d_points <- data.frame(long = c(41), lat = c(41)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs = laea)

# Use ggplot2 to visualize the transformed map and points
ggplot(data = tr) + 
  geom_sf(fill = "lightblue", color = "black") +
  geom_sf(data = d_points, color = "red", size = 3) +
  theme_bw(base_size = 20) +
  labs(title = "Transformed Map of Turkey with Study Points")
