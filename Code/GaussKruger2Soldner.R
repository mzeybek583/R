# 5.3.2 Gauss- Kruger Koordinatlarından Jeodezik Dik (Soldner)
# Koordinatlarının Bulunması

## Referans: https://avys.omu.edu.tr/storage/app/public/sbektas/71424/matjeo9-d%C3%B6n%C3%BC%C5%9Ft%C3%BCr%C3%BCld%C3%BC.pdf 

# Gerekli parametreler
xg <- 4183627  # Gauss-Kruger x koordinatı (metre)
yg <- 153742.908  # Gauss-Kruger y koordinatı (metre)
R <- 6373934  # Yer yarıçapı (metre)

# Jeodezik Dik Koordinatlar Hesaplama
X <- xg  # X değeri Gauss-Kruger xg'ye eşittir

# Y için formül
Y <- yg - (yg^3 / (6 * R^2)) + (yg^5 / (24 * R^4))

# Sonuçları yazdırma
cat("Jeodezik Dik (Soldner) Koordinatları:\n")
cat("X:", X, "m\n")
cat("Y:", round(Y, 4), "m\n")
