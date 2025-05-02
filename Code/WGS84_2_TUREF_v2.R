library(readr)
library(sf)
library(dplyr)

# CSV dosyasýný oku (ondalýk nokta ayarý ile)
data <- read_csv("Helen_Layer1.csv", locale = locale(decimal_mark = "."))

# Latitude, Longitude ve Elevation deðerlerinden sf objesi oluþtur
data_coords <- data %>%
  select(Latitude, Longitude, Elevation) %>%
  rename(lat = Latitude, lon = Longitude, elev = Elevation)

# Noktalarý EPSG:4326 koordinat sistemi ile sf objesine çevir
points_sf <- st_as_sf(data_coords, coords = c("lon", "lat"), crs = 4326)

# Koordinatlarý EPSG:5255 (Turef TM30 / Zone 33) sistemine dönüþtür
points_5255 <- st_transform(points_sf, crs = 5255)

# Koordinatlarý ayýr
coords_transformed <- st_coordinates(points_5255)

# Yeni koordinatlarý ve anten yüksekliði düzeltmesini veri çerçevesine ekle
points_5255 <- points_5255 %>%
  mutate(
    X_5255 = coords_transformed[, 1],
    Y_5255 = coords_transformed[, 2],
    Elev_antenna = elev - 1.91
  )

# Ondalýk hassasiyet artýr
options(digits = 10)

# Y koordinatlarýný yazdýr (isteðe baðlý)
print(points_5255$Y_5255)

# SHP dosyasýna yaz (ayný dizine .shp, .shx, .dbf, .prj olarak kaydeder)
st_write(points_5255, "koordinatlar_5255.shp", delete_layer = TRUE)
