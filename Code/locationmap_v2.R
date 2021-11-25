library(maps)
map("world", "Turkey")
tr = st_as_sf(map("world", "Turkey", plot = TRUE, fill = TRUE))
laea = st_crs("+proj=laea +lat_0=30 +lon_0=20") # Lambert equal area
tr <- st_transform(tr, laea)
g = st_graticule(tr,lon = seq(24,48,3),lat = seq(34,44,2))
plot(st_geometry(g), axes = TRUE, add=T)
plot(tr, graticule = g, key.pos = NULL, axes = TRUE, main="")


library(ggplot2)
d_points <- data.frame(long = c(41),lat  = c(41)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs = laea)

ggplot(data = tr) + 
  geom_sf()+
  geom_sf(data = d_points, 
          color = "red", size = 5)+
  theme_bw(base_size = 20)
  
