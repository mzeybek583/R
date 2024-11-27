# Gauss- Krüger Koordinatlardan Küresel Coğrafi Koordinatların Hesabı
## Referans: https://avys.omu.edu.tr/storage/app/public/sbektas/71424/matjeo9-d%C3%B6n%C3%BC%C5%9Ft%C3%BCr%C3%BCld%C3%BC.pdf 

# Gerekli parametreler
R <- 6373924.115  # Yer yarıçapı (metre)

# Derece, dakika, saniyeyi ondalık dereceye çevirme fonksiyonu
dms_to_decimal <- function(degree, minute, second) {
  return(degree + minute / 60 + second / 3600)
}

# Verilen değerler
phi_d <- dms_to_decimal(38, 12, 24.16)  # Enlem φ (ondalık derece)
lambda_d <- dms_to_decimal(31, 10, 29.22)  # Boylam λ (ondalık derece)
lambda_0_d <- 30  # Meridyen merkezi λ₀ (derece)

# Boylam farkı (Δλ)
delta_lambda <- lambda_d - lambda_0_d

# Dereceden radyana çevirme fonksiyonu
deg2rad <- function(degree) {
  return(degree * pi / 180)
}

# Enlem ve boylamı radyana çevirme
phi_rad <- deg2rad(phi_d)
lambda_rad <- deg2rad(delta_lambda)

# Gauss-Kruger koordinatları formülleri
x <- R * atan(tan(phi_rad) / cos(lambda_rad))
y <- R * atanh(sin(lambda_rad) * cos(phi_rad))

# Sonuçları yazdırma
cat("Gauss-Kruger Koordinatları:\n")
cat("x:", sprintf("%.3f", x), "m\n")  
cat("y:", sprintf("%.3f", y), "m\n")  
