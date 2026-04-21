# =========================================================
# Pt6-Pt7 doğrultusuna göre, Pt8 yönüne açılan dikdörtgen içinde
# A0'dan başlayarak 1 m aralıklarla grid noktaları üretme
# =========================================================

# -------------------------
# 1) NOKTALAR
# -------------------------
Pt6 <- c(X = 474973.830, Y = 4127800.206)
Pt7 <- c(X = 474981.240, Y = 4127785.934)
Pt8 <- c(X = 474955.968, Y = 4127773.017)

grid_araligi <- 1  # metre

# -------------------------
# 2) YARDIMCI FONKSİYONLAR
# -------------------------
norm_vec <- function(v) {
  sqrt(sum(v^2))
}

unit_vec <- function(v) {
  v / norm_vec(v)
}

# -------------------------
# 3) Pt6 -> Pt7 DOĞRULTUSU (x ekseni)
# -------------------------
v67 <- Pt7 - Pt6
Lx  <- norm_vec(v67)       # dikdörtgen uzunluğu
ux  <- unit_vec(v67)       # birim doğrultu vektörü

# Dik vektör adayları
uy1 <- c(-ux[2], ux[1])
uy2 <- c( ux[2], -ux[1])

# Pt8 hangi taraftaysa o dik yön seçilir
v68 <- Pt8 - Pt6
if (sum(v68 * uy1) >= 0) {
  uy <- uy1
} else {
  uy <- uy2
}

# Pt8'in doğruya dik imzalı uzaklığı
Ly_signed <- sum(v68 * uy)
Ly <- abs(Ly_signed)       # dikdörtgen genişliği

# -------------------------
# 4) DİKDÖRTGEN KÖŞELERİ
# -------------------------
A0 <- Pt6
A1 <- Pt6 + Lx * ux
B0 <- Pt6 + Ly * uy
B1 <- Pt6 + Lx * ux + Ly * uy

cat("Köşe koordinatları:\n")
print(rbind(A0 = A0, A1 = A1, B0 = B0, B1 = B1))

# -------------------------
# 5) GRID NOKTALARI
# -------------------------
# Satır: dik yönde (A, B, C, ...)
# Sütun: doğrultu boyunca (0, 1, 2, ...)
nx <- floor(Lx / grid_araligi)
ny <- floor(Ly / grid_araligi)

x_mesafeler <- seq(0, nx * grid_araligi, by = grid_araligi)
y_mesafeler <- seq(0, ny * grid_araligi, by = grid_araligi)

# Harf üretici (A, B, C, ..., Z, AA, AB, ...)
num_to_letters <- function(n) {
  out <- character(length(n))
  for (k in seq_along(n)) {
    x <- n[k]
    s <- ""
    repeat {
      r <- x %% 26
      s <- paste0(LETTERS[r + 1], s)
      x <- x %/% 26 - 1
      if (x < 0) break
    }
    out[k] <- s
  }
  out
}

grid_list <- vector("list", length(y_mesafeler) * length(x_mesafeler))
idx <- 1

for (j in seq_along(y_mesafeler)) {
  for (i in seq_along(x_mesafeler)) {
    
    d_x <- x_mesafeler[i]
    d_y <- y_mesafeler[j]
    
    P <- A0 + d_x * ux + d_y * uy
    
    satir_adi <- num_to_letters(j - 1)   # A, B, C...
    sutun_no  <- i - 1                   # 0,1,2...
    nokta_adi <- paste0(satir_adi, sutun_no)
    
    grid_list[[idx]] <- data.frame(
      Nokta = nokta_adi,
      Satir = satir_adi,
      Sutun = sutun_no,
      dx_m = d_x,
      dy_m = d_y,
      X = round(P[1], 3),
      Y = round(P[2], 3)
    )
    
    idx <- idx + 1
  }
}

grid_df <- do.call(rbind, grid_list)

# -------------------------
# 6) SONUÇLARI GÖSTER
# -------------------------
cat("\nİlk 20 grid noktası:\n")
print(head(grid_df, 20))

# -------------------------
# 7) CSV KAYDET
# -------------------------
write.table(grid_df, "grid_noktalari_1m.csv", row.names = FALSE, 
            fileEncoding = "UTF-8", sep = ";")

# -------------------------
# 7B) SHP EXPORT (EPSG:5255)
# -------------------------
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
library(sf)

# grid noktalarını sf objesine çevir
grid_sf <- st_as_sf(
  grid_df,
  coords = c("X", "Y"),
  crs = 5255
)

# SHP olarak kaydet
st_write(
  grid_sf,
  "grid_noktalari_1m_epsg5255.shp",
  delete_layer = TRUE,
  quiet = TRUE
)

cat("\nSHP dosyası EPSG:5255 olarak kaydedildi: grid_noktalari_1m_epsg5255.shp\n")

# -------------------------
# 8) GRAFİK
# -------------------------
plot(grid_df$X, grid_df$Y,
     asp = 1, pch = 16, cex = 0.5,
     xlab = "X", ylab = "Y",
     main = "Pt6-Pt7 doğrultusunda, Pt8 yönüne 1 m grid")

# Dikdörtgen sınırı
rect_x <- c(A0[1], A1[1], B1[1], B0[1], A0[1])
rect_y <- c(A0[2], A1[2], B1[2], B0[2], A0[2])
lines(rect_x, rect_y, lwd = 2, col = "red")

# Ana noktalar
points(rbind(Pt6, Pt7, Pt8), pch = 19, col = c("blue", "blue", "darkgreen"), cex = 1.1)
text(Pt6[1], Pt6[2], "Pt6 / A0", pos = 3, col = "blue")
text(Pt7[1], Pt7[2], "Pt7", pos = 3, col = "blue")
text(Pt8[1], Pt8[2], "Pt8", pos = 3, col = "darkgreen")
