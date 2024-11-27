# 5.2 Kürenin Düzleme Ordinat Koruyan (Cassini-Soldner Projeksiyonu) Projeksiyonu
# Ordinat Koruyan Projeksiyonda Jeodezik Temel Ödevlerin Çözümü:
# II. Jeodezik Temel Ödev
## Referans: https://avys.omu.edu.tr/storage/app/public/sbektas/71424/matjeo9-d%C3%B6n%C3%BC%C5%9Ft%C3%BCr%C3%BCld%C3%BC.pdf 


# Gerekli parametreler
X1 <- 18560.115  # Başlangıç x koordinatı (metre)
Y1 <- 17043.856  # Başlangıç y koordinatı (metre)
X2 <- 46587.077  # Hedef x koordinatı (metre)
Y2 <- 59433.954  # Hedef y koordinatı (metre)
R <- 6373924.115 # Yer yarıçapı (metre)
rho <- (180/pi) # Dereceden saniyeye dönüşüm sabiti

# Dereceden radyana çevirme fonksiyonu
deg2rad <- function(degree) {
  return(degree * pi / 180)
}

# Radyandan dereceye çevirme fonksiyonu
rad2deg <- function(radian) {
  return(radian * 180 / pi)
}

# 1. Adım: Delta değerlerini hesaplama
delta_x <- X2 - X1
delta_y <- Y2 - Y1

# 2. Adım: Başlangıç yönü (T1) ve karşı yönü (T2)
t1_rad <- atan2(delta_y, delta_x) # Radyan cinsinden başlangıç yönü
t1_deg <- rad2deg(t1_rad) # Dereceye çevir

# Karşı yön (t2 = t1 + 180 derece)
t2_rad <- t1_rad + pi
t2_deg <- rad2deg(t2_rad)

# 3. Adım: Mesafeyi (S) hesaplama
S <- sqrt(delta_x^2 + delta_y^2)

# 4. Adım: Düzeltilmiş mesafe ve yön
s_correction <- (S / (6 * R^2)) * (Y1^2 + Y1 * Y2 + Y2^2) * cos(t1_rad)^2
S_corrected <- S - s_correction

# Düzeltilmiş yönler
T1 <- t1_deg + ((rho / (6 * R^2)) * delta_x * (2 * Y1 + Y2)) +
  ((rho / (6 * R^2)) * (Y1^2 + Y1 * Y2 + Y2^2) * sin(t1_rad) * cos(t1_rad))

T2 <- t2_deg - ((rho / (6 * R^2)) * delta_x * (Y1 + 2 * Y2)) +
  ((rho / (6 * R^2)) * (Y1^2 + Y1 * Y2 + Y2^2) * sin(t2_rad) * cos(t2_rad))

degree_to_dms <- function(degree) {
  d <- floor(degree) # Tam sayı kısmı (derece)
  m <- floor((degree - d) * 60) # Dakika
  s <- ((degree - d) * 60 - m) * 60 # Saniye
  return(c(d = d, m = m, s = round(s, 2))) # Derece, dakika ve saniyeyi döndür
}

# T1-2'yi derece dakika saniyeye çevir
T1_dms <- degree_to_dms(T1)
T2_dms <- degree_to_dms(T2)

# Sonuçları yazdırma
cat("Mesafe (S):", S_corrected, "m\n")
cat("Baslangic Yonu (T1):", T1_dms, "derece,dk,sn\n")
cat("Karsi Yon (T2):", T2_dms, "derece,dk,sn\n")
