# 5.2 Kürenin Düzleme Ordinat Koruyan (Cassini-Soldner Projeksiyonu) Projeksiyonu
# Ordinat Koruyan Projeksiyonda Jeodezik Temel Ödevlerin Çözümü:
# l. Jeodezik Temel Ödev
## Referans: https://avys.omu.edu.tr/storage/app/public/sbektas/71424/matjeo9-d%C3%B6n%C3%BC%C5%9Ft%C3%BCr%C3%BCld%C3%BC.pdf 


# Gerekli parametreler

X1 <- -92276.440 # Başlangıç x koordinatı (metre)
Y1 <- 82130.142  # Başlangıç y koordinatı (metre)
T1 <- 321 + 56 / 60 + 6.09 / 3600 # Başlangıç yönü (derece)
S <- 105455.230  # Mesafe (metre)
R <- 6373924.115 # Yer yarıçapı (metre)
rho <- 180 / pi # Dereceden saniyeye dönüşüm sabiti

# Dereceden radyana çevirme fonksiyonu
deg2rad <- function(degree) {
  return(degree * pi / 180)
}

# Radyandan dereceye çevirme fonksiyonu
rad2deg <- function(radian) {
  return(radian * 180 / pi)
}

# Başlangıç yönünü radyana çevirme
T1_rad <- deg2rad(T1)

# 1. Adım: Yaklaşık koordinatlar
x2 <- X1 + S * cos(T1_rad)
y2 <- Y1 + S * sin(T1_rad)

# 2. Adım: Iteratif düzeltme
t1 <- T1 # Başlangıç değeri
epsilon <- 1e-6 # Yakınsama hata toleransı (derece cinsinden)
error <- 1e6 # Başlangıç hatası
iteration <- 0 # Iterasyon sayacı
t1 <- T1 - (rho / (6 * R^2)) * ((x2 - X1) * (2 * Y1 + y2)) -
  (rho / (6 * R^2)) * ((Y1^2 + Y1 * y2 + Y1^2) * sin(t1) * cos(t1))
while (error > epsilon) {
  iteration <- iteration + 1
  t1_old <- t1 # Bir önceki iterasyondaki t1
  t1_rad <- deg2rad(t1_old) # Dereceden radyana çevir
  t1 <- T1 - (rho / (6 * R^2)) * ((x2 - X1) * (2 * Y1 + y2)) -
    (rho / (6 * R^2)) * ((Y1^2 + Y1 * y2 + y2^2) * sin(t1_rad) * cos(t1_rad))
  
  error <- abs(t1 - t1_old) # Hata hesaplama
}

# Düzeltilmiş yön (derece biriminde)
t1_rad <- deg2rad(t1)
s <- S + (S / (6 * R^2)) * (Y1^2 + Y1 * y2 + y2^2) * cos(t1_rad)^2

# 3. Adım: Kesin koordinat hesaplama
x2_final <- X1 + s * cos(t1_rad)
y2_final <- Y1 + s * sin(t1_rad)

# 4. Adım: Karşı yön (T2) hesaplama
t2 <- t1 - 180 # 180 dereceyi çıkartarak karşı yön bulunur
t2_rad <- deg2rad(t2)
T2 <- t2 + (rho / (6 * R^2)) * ((X1 - x2_final) * (Y1 + 2 * y2_final)) +
  (rho / (6 * R^2)) * ((Y1^2 + Y1 * y2_final + y2_final^2) * sin(t2_rad) * cos(t2_rad))

degree_to_dms <- function(degree) {
  d <- floor(degree) # Tam sayı kısmı (derece)
  m <- floor((degree - d) * 60) # Dakika
  s <- ((degree - d) * 60 - m) * 60 # Saniye
  return(c(d = d, m = m, s = round(s, 2))) # Derece, dakika ve saniyeyi döndür
}

# T1-2'yi derece dakika saniyeye çevir
T1_dms <- degree_to_dms(t1)
T2_dms <- degree_to_dms(T2)


# Sonuçlar
library(celestial)
cat("Kesin Koordinatlar:\n")
cat("X2:", x2_final, "m\n")
cat("Y2:", y2_final, "m\n")
cat("\n Duzeltilmis Baslangic Yonu (T1):", T1_dms, "derece,dk,sn\n")
cat("Karsi Yon (T2):", T2_dms, "derece,dk,sn\n")
cat("\n Iterasyon Sayisi:", iteration, "\n")


