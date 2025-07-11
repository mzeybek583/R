library(readr)
library(sf)
library(dplyr)

# CSV dosyas�n� oku (ondal�k nokta ayar� ile)
data <- read_csv("Helen_Layer1.csv", locale = locale(decimal_mark = "."))

# Latitude, Longitude ve Elevation de�erlerinden sf objesi olu�tur
data_coords <- data %>%
  select(Latitude, Longitude, Elevation) %>%
  rename(lat = Latitude, lon = Longitude, elev = Elevation)

# Noktalar� EPSG:4326 koordinat sistemi ile sf objesine �evir
points_sf <- st_as_sf(data_coords, coords = c("lon", "lat"), crs = 4326)

# Koordinatlar� EPSG:5255 (Turef TM30 / Zone 33) sistemine d�n��t�r
points_5255 <- st_transform(points_sf, crs = 5255)

# Koordinatlar� ay�r
coords_transformed <- st_coordinates(points_5255)

# Yeni koordinatlar� ve anten y�ksekli�i d�zeltmesini veri �er�evesine ekle
points_5255 <- points_5255 %>%
  mutate(
    X_5255 = coords_transformed[, 1],
    Y_5255 = coords_transformed[, 2],
    Elev_antenna = elev - 1.91
  )

# Ondal�k hassasiyet art�r
options(digits = 10)

# Y koordinatlar�n� yazd�r (iste�e ba�l�)
print(points_5255$Y_5255)

# SHP dosyas�na yaz (ayn� dizine .shp, .shx, .dbf, .prj olarak kaydeder)
st_write(points_5255, "koordinatlar_5255.shp", delete_layer = TRUE)
