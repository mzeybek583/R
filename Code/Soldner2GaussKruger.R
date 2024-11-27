# 5.3.1 Jeodezik Dik (Soldner) Koordinatlardan Gauss-Kruger
#Koordinatlarının Bulunması

## Referans: https://avys.omu.edu.tr/storage/app/public/sbektas/71424/matjeo9-d%C3%B6n%C3%BC%C5%9Ft%C3%BCr%C3%BCld%C3%BC.pdf 


# Gerekli parametreler
X <- 4183627  # Başlangıç X koordinatı (metre)
Y <- 153728   # Başlangıç Y koordinatı (metre)
R <- 6373934  # Yer yarıçapı (metre)

# Gauss-Kruger koordinatları hesaplama
xg <- X  # Gauss-Kruger'deki X koordinatı aynıdır

# y_g formülü
yg <- Y + (Y^3 / (6 * R^2)) + (Y^5 / (24 * R^4))

# Sonuçları yazdırma
cat("Gauss-Kruger Koordinatları:\n")
cat("xg:", xg, "m\n")
cat("yg:", round(yg, 3), "m\n")
