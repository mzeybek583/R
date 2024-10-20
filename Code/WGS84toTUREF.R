# Load required library
if (!require("sf")) install.packages("sf", dependencies = TRUE)
library(sf)

# Define coordinates in WGS84 (EPSG:4326)
p <- data.frame(x = 41.84932405683, y = 41.199137172149)

# Convert data to an sf object with WGS84 CRS
p_sf <- st_as_sf(p, coords = c("x", "y"), crs = 4326)

# Print the original coordinates
print(p_sf)

# Transform coordinates to TUREF (EPSG:5258)
p_turef <- st_transform(p_sf, crs = 5258)

# Print the transformed coordinates
print(st_coordinates(p_turef))
