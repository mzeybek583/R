# Jeosantrik koordinatları ekleyelim
X <- 1241581.343
Y <- -4638917.074
Z <- 4183965.568

# WGS84 elipsoid parametrelerini tanımlayalım
a <- 6378137 # Yarı büyük eksen (metre cinsinden)
f <- 1 / 298.257223563 # Düzlük (WGS84 için)
e2 <- 2 * f - f^2 # Eksantriklik karesi

# Jeosantrik koordinatlardan jeodezik koordinatlara dönüşüm
# İlk tahminleri yapalım
p <- sqrt(X^2 + Y^2)
theta <- atan(Z * a / (p * (1 - f)))
phi <- atan((Z + e2 * a * sin(theta)^3) / (p - e2 * a * cos(theta)^3))
lambda <- atan(Y / X)
N <- a / sqrt(1 - e2 * sin(phi)^2)
h <- p / cos(phi) - N

# İteratif olarak daha doğru sonuçlar için dönüşümü tekrarlayalım
for (i in 1:5) {
  N <- a / sqrt(1 - e2 * sin(phi)^2)
  h <- p / cos(phi) - N
  phi <- atan((Z + e2 * N * sin(phi)) / p)
  print(phi)
}

# Sonuçları derece cinsine dönüştürelim
phi_deg <- phi * (180 / pi)
lambda_deg <- lambda * (180 / pi)
if (X < 0 && Y < 0) {
  lambda_deg <- lambda_deg - 180
} else if (X < 0) {
  lambda_deg <- lambda_deg + 180
}

# Sonuçları yazdıralım
cat("Jeodezik Enlem (phi):", phi_deg, "derece\n")
cat("Jeodezik Boylam (lambda):", lambda_deg, "derece\n")
cat("Yükseklik (h):", h, "metre\n")

# Derece cinsinden koordinatları tanımlayalım
#phi_deg <- 41.352697036919835
#lambda_deg <- -75.01628130085454

# Dereceyi derece, dakika ve saniyeye dönüştürme fonksiyonu
convert_dms <- function(degree) {
  sign <- ifelse(degree >= 0, 1, -1) # Derecenin işaretini sakla
  degree <- abs(degree) # Mutlak değeri al
  
  degrees <- floor(degree)
  minutes <- floor((degree - degrees) * 60)
  seconds <- (degree - degrees - minutes / 60) * 3600
  
  # Sonuçları işaret ile çarpıp geri döndür
  return(sign * c(degrees, minutes, seconds))
}

# Jeodezik enlem ve boylamı DMS formatına dönüştürelim
phi_dms <- convert_dms(phi_deg)
lambda_dms <- convert_dms(lambda_deg)

# Sonuçları yazdıralım
cat("Jeodezik Enlem (DMS):", phi_dms[1], "derece", phi_dms[2], "dakika", round(phi_dms[3], 4), "saniye\n")
cat("Jeodezik Boylam (DMS):", lambda_dms[1], "derece", lambda_dms[2], "dakika", round(lambda_dms[3], 4), "saniye\n")

