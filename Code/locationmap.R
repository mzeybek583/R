
## Study area Map of Turkey
# Coder: Assist. Prof. Dr. Mustafa Zeybek
# Reference https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

setwd("E:/R/") #change
library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")
library(tikzDevice)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
# ggplot(data = world) +
#   geom_sf() +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))
# 
# ggplot(data = world) + 
#   geom_sf(color = "black", fill = "lightgreen")
# 
# ggplot(data = world) +
#   geom_sf(aes(fill = pop_est)) +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt")
# 
# ggplot(data = world) +
#   geom_sf() +
#   coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
# 
# ggplot(data = world) +
#   geom_sf() +
#   coord_sf(crs = "+init=epsg:3035")
# 
# ggplot(data = world) +
#   geom_sf() +
#   coord_sf(crs = st_crs(3035))
# 
# ggplot(data = world) +
#   geom_sf() +
#   coord_sf(xlim = c(20, 51), ylim = c(30, 48), expand = FALSE)
# 
# library("ggspatial")
# ggplot(data = world) +
#   geom_sf() +
#   annotation_scale(location = "bl", width_hint = 0.5) +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   coord_sf(xlim = c(20, 51), ylim = c(30, 48))

# ggplot(data = world) +
#   geom_sf() +
#   geom_text(data= world_points,aes(x=X, y=Y, label=name),
#             color = "darkblue", fontface = "bold", check_overlap = FALSE) +
#   annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
#            fontface = "italic", color = "grey22", size = 6) +
#   coord_sf(xlim = c(20, 51), ylim = c(30, 48), expand = FALSE)

## Scale on map varies by more than 10%, scale bar may be inaccurate

##

tikz('plot1.tex',width=3.5, height=3.5)

library("sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

world_points <- world_points[world$name=="Turkey",]

sites <- data.frame(longitude = c(41.8470), latitude = c(41.1979))

ggplot(data = world) + 
  geom_sf(fill= "antiquewhite")+
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 2, 
             shape = 22, fill = "darkred")+
  geom_text(data= world_points,aes(x=X, y=Y, label=name), 
            color = "darkblue", fontface = "bold", check_overlap = TRUE,  size = 3) +
  #annotate(geom = "text", x = -90, y = 26,
   #        label = "Gulf of Mexico", fontface = "italic", color = "grey22", size = 6) +
  annotation_scale(location = "bl", width_hint = 0.2,
                   text_cex = 0.6,  text_face = 2) +
  annotation_north_arrow(location = "bl", which_north = "true",  
                         height = unit(0.5, "cm"),
                         width = unit(0.5, "cm"),
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(20, 51), ylim = c(30, 48), expand = FALSE) + 
  xlab("Longitude") + ylab("Latitude") + 
  #ggtitle("Map of Turkey and Study Area (Artvin Province)") + 
  theme_bw(base_size = 9)+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.3), 
        panel.background = element_rect(fill = "aliceblue")
#+
 # theme_bw(base_size = 20))
)
ggsave("map.pdf")
ggsave("map_web.png", width = 6, height = 6, dpi = 300)
dev.off()
