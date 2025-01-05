# Robust Yan Nokta Hesaplama Fonksiyonu
yan_nokta_hesapla <- function(ya, xa, yb, xb, da, db, nokta_adi) {
  # Semt açısını güvenli şekilde hesapla
  delta_x <- xb - xa
  delta_y <- yb - ya
  
  if (delta_x == 0 && delta_y == 0) {
    stop("A ve B noktaları aynı yerde olamaz!")
  }
  
  semt_ab <- atan2(delta_y, delta_x) * 200 / pi  # atan2 kullanımı daha güvenli
  
  # Mesafe ve açı hesaplamaları
  d <- sqrt(da^2 + db^2)
  alfa <- atan2(db, da) * 200 / pi  # atan2 kullanımı ile daha güvenli
  
  # P noktasının semt açısı
  p_semt <- semt_ab + alfa
  
  # P noktasının koordinatları
  yp <- ya + d * sin(p_semt * pi / 200)
  xp <- xa + d * cos(p_semt * pi / 200)
  
  # Sonuçları döndür
  return(list(xp = xp, yp = yp, p_semt = p_semt, semt_ab = semt_ab, nokta_adi = nokta_adi))
}

# Çoklu da ve db için işlem yapma fonksiyonu
coklu_da_db_hesapla <- function(ya, xa, yb, xb, da_db_pairs) {
  # Giriş kontrolleri
  if (!is.data.frame(da_db_pairs)) {
    stop("da_db_pairs bir veri çerçevesi olmalıdır!")
  }
  if (!all(c("da", "db", "nokta_adi") %in% names(da_db_pairs))) {
    stop("da_db_pairs veri çerçevesinde 'da', 'db' ve 'nokta_adi' sütunları bulunmalıdır!")
  }
  
  # Çoklu işlem
  results <- lapply(1:nrow(da_db_pairs), function(i) {
    row <- da_db_pairs[i, ]
    yan_nokta_hesapla(ya, xa, yb, xb, row$da, row$db, row$nokta_adi)
  })
  
  # Sonuçları bir veri çerçevesine dönüştür
  xp_values <- sapply(results, function(res) res$xp)
  yp_values <- sapply(results, function(res) res$yp)
  p_semt_values <- sapply(results, function(res) res$p_semt)
  semt_ab_values <- sapply(results, function(res) res$semt_ab)
  nokta_adlari <- sapply(results, function(res) res$nokta_adi)
  
  # Orijinal da, db çiftleriyle birleştir
  output <- cbind(da_db_pairs, xp = xp_values, yp = yp_values, p_semt = p_semt_values, semt_ab = semt_ab_values, nokta_adi = nokta_adlari)
  return(as.data.frame(output))
}

# A ve B noktalarının koordinatları
ya <- 24610.32
xa <- 12410.21
yb <- 24709.18
xb <- 12500.33

# Çoklu da ve db girişleri
da_db_pairs <- data.frame(
  da = c(32.11, 67.12, 99.68),
  db = c(0.00, -27.15, 19.45),
  nokta_adi = c("Nokta 1", "Nokta 2", "Nokta 3")
)

# Çoklu da ve db için hesaplama
results <- coklu_da_db_hesapla(ya, xa, yb, xb, da_db_pairs)

# Sonuçları ekrana yazdır
cat("Hesaplanan Yan Nokta Sonuçları:\n")
print(results)

# Görselleştirme için kütüphane
ggplot2::ggplot() + 
  # A ve B noktalarını çiz
  ggplot2::geom_point(aes(x = c(xa, xb), y = c(ya, yb)), color = "red", shape = 17, size = 5) +
  ggplot2::geom_text(aes(x = c(xa, xb), y = c(ya, yb), label = c("A", "B")), vjust = -1, color = "red") +
  # AB doğrultusunu çiz
  ggplot2::geom_segment(aes(x = xa, y = ya, xend = xb, yend = yb), linetype = "dashed", color = "green") +
  # Hesaplanan noktaları çiz
  ggplot2::geom_point(aes(x = results$xp, y = results$yp), color = "black", shape = 16, size = 4) +
  ggplot2::geom_text(aes(x = results$xp, y = results$yp, label = results$nokta_adi), vjust = -1, color = "black") +
  # da ve db vektörlerini çiz
  lapply(1:nrow(results), function(i) {
    ggplot2::geom_segment(aes(
      x = xa + results$da[i] * cos(results$semt_ab[i] * pi / 200), 
      y = ya + results$da[i] * sin(results$semt_ab[i] * pi / 200), 
      xend = results$xp[i], yend = results$yp[i]), 
      color = "orange", arrow = ggplot2::arrow())
  }) +
  # Grafik ayarları
  ggplot2::labs(title = "Yan Nokta Hesaplama", x = "X Koordinatları", y = "Y Koordinatları") +
  ggplot2::theme_minimal() +
  coord_fixed(ratio = 1)
