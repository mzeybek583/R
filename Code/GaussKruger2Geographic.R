# Gauss- Krüger Koordinatlardan Küresel Coğrafi Koordinatların Hesabı
## Referans: https://avys.omu.edu.tr/storage/app/public/sbektas/71424/matjeo9-d%C3%B6n%C3%BC%C5%9Ft%C3%BCr%C3%BCld%C3%BC.pdf 

# Gerekli parametreler
x <- 4250993.561  # Gauss-Kruger x koordinatı (metre)
y <- 102695.782   # Gauss-Kruger y koordinatı (metre)
R <- 6373924.115  # Yer yarıçapı (metre)
lambda_0 <- 30    # Meridyen merkezi (derece)

# Dereceden dakika ve saniyeye çevirme fonksiyonu
decimal_to_dms <- function(decimal_degree) {
  degree <- floor(decimal_degree)
  minute <- floor((decimal_degree - degree) * 60)
  second <- ((decimal_degree - degree) * 60 - minute) * 60
  return(c(degree = degree, minute = minute, second = round(second, 2)))
}

# Enlem (φ) hesaplama
phi_rad <- asin(sin(x / R) / cosh(y / R))  # Radyan cinsinden φ
phi_deg <- phi_rad * (180 / pi)         # Dereceye çevir

# Boylam farkı (Δλ) hesaplama
delta_lambda_rad <- atan(sinh(y / R) / cos(x / R))  # Radyan cinsinden Δλ
delta_lambda_deg <- delta_lambda_rad * (180 / pi)  # Dereceye çevir

# Boylam (λ) hesaplama
lambda_deg <- lambda_0 + delta_lambda_deg  # λ = λ₀ + Δλ

# Enlem ve boylamı derece, dakika, saniye formatına çevirme
phi_dms <- decimal_to_dms(phi_deg)
lambda_dms <- decimal_to_dms(lambda_deg)

# Sonuçları yazdırma
cat("Kuresel Cografi Koordinatlar:\n")
cat("φ (Enlem):", phi_dms["degree"], "°", phi_dms["minute"], "'", phi_dms["second"], "\"\n")
cat("λ (Boylam):", lambda_dms["degree"], "°", lambda_dms["minute"], "'", lambda_dms["second"], "\"\n")
