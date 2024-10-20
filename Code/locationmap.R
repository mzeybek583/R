# Study Area Map of Turkey
# Coder: Assist. Prof. Dr. Mustafa Zeybek

# Load required libraries
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("sf")) install.packages("sf", dependencies = TRUE)
if (!require("rnaturalearth")) install.packages("rnaturalearth", dependencies = TRUE)
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata", dependencies = TRUE)
if (!require("ggspatial")) install.packages("ggspatial", dependencies = TRUE)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# Set theme for plots
theme_set(theme_bw())

# Load world data and extract Turkey
world <- ne_countries(scale = "medium", returnclass = "sf")
turkey <- world[world$name == "Turkey", ]

# Define the study area site (e.g., Artvin Province)
sites <- data.frame(longitude = c(41.8470), latitude = c(41.1979))

# Extract centroid for Turkey to add as a label
world_points <- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
turkey_point <- world_points[world_points$name == "Turkey", ]

# Create the study area map
map_turkey <- ggplot(data = world) + 
  geom_sf(fill = "antiquewhite") +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 2, 
             shape = 22, fill = "darkred") +
  geom_text(data = turkey_point, aes(x = X, y = Y, label = name), 
            color = "darkblue", fontface = "bold", check_overlap = TRUE, size = 3) +
  annotation_scale(location = "bl", width_hint = 0.2,
                   text_cex = 0.6, text_face = 2) +
  annotation_north_arrow(location = "bl", which_north = "true",  
                         height = unit(0.5, "cm"),
                         width = unit(0.5, "cm"),
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(20, 51), ylim = c(30, 48), expand = FALSE) + 
  xlab("Longitude") + ylab("Latitude") + 
  theme(
    panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.3), 
    panel.background = element_rect(fill = "aliceblue")
  )

# Save the map to files
ggsave("turkey_map.pdf", map_turkey, width = 6, height = 6)
ggsave("turkey_map.png", map_turkey, width = 6, height = 6, dpi = 300)
