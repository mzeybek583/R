# =========================================================
# 3 noktaya gore en yakin dikdortgen
# Pt6 -> Pt7 ilk kenar
# Pt7 civarinda Pt8'e en yakin dik kose uretilir
# Grid, CSV, SHP, Plotly
# EPSG:5255
# =========================================================

# -------------------------
# 1) NOKTALAR
# -------------------------
Pt6 <- c(X = 474973.830, Y = 4127800.206)
Pt7 <- c(X = 474981.240, Y = 4127785.934)
Pt8 <- c(X = 474955.968, Y = 4127773.017)

grid_araligi <- 1

# -------------------------
# 2) KUTUPHANELER
# -------------------------
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")

library(sf)
library(plotly)

# -------------------------
# 3) YARDIMCI FONKSIYONLAR
# -------------------------
norm_vec <- function(v) {
  sqrt(sum(v^2))
}

unit_vec <- function(v) {
  v / norm_vec(v)
}

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

# -------------------------
# 4) DIKDORTGENIN OLUSTURULMASI
# -------------------------
# Pt6 -> Pt7 ilk kenar
v67 <- Pt7 - Pt6
ux <- unit_vec(v67)
Lx <- norm_vec(v67)

# Pt7'den dik yon
uy1 <- c(-ux[2], ux[1])
uy2 <- c( ux[2], -ux[1])

v78 <- Pt8 - Pt7

# Pt8 hangi dik tarafta ise onu sec
if (sum(v78 * uy1) >= 0) {
  uy <- uy1
} else {
  uy <- uy2
}

# Pt8'in Pt7'den itibaren:
# - ux yonundeki bileseni: paralellik hatasi
# - uy yonundeki bileseni: dik kenar boyu
paralel_hata <- sum(v78 * ux)
Ly <- sum(v78 * uy)

if (Ly < 0) {
  stop("Ly negatif cikti. Nokta sirasi veya yon secimi kontrol edilmeli.")
}

# Dikdortgen koseleri
A0 <- Pt6
A1 <- Pt7
B1 <- Pt7 + Ly * uy
B0 <- Pt6 + Ly * uy

# Pt8'in dikdortgene en yakin duzeltilmis hali
Pt8_rect <- B1

cat("Kose koordinatlari:\n")
print(rbind(
  Pt6_A0   = A0,
  Pt7_A1   = A1,
  Pt8_rect = Pt8_rect,
  B0       = B0
))

cat("\nGercek Pt8 ile duzeltilmis Pt8_rect farki (m):\n")
print(Pt8 - Pt8_rect)

cat("\nPt8'in Pt7->Pt8 vektorunde paralel hata (m):\n")
print(paralel_hata)

# -------------------------
# 5) GRID NOKTALARI
# -------------------------
nx <- floor(Lx / grid_araligi)
ny <- floor(Ly / grid_araligi)

x_mesafeler <- seq(0, nx * grid_araligi, by = grid_araligi)
y_mesafeler <- seq(0, ny * grid_araligi, by = grid_araligi)

grid_list <- vector("list", length(x_mesafeler) * length(y_mesafeler))
idx <- 1

for (j in seq_along(y_mesafeler)) {
  for (i in seq_along(x_mesafeler)) {
    
    dx <- x_mesafeler[i]
    dy <- y_mesafeler[j]
    
    P <- A0 + dx * ux + dy * uy
    
    satir_adi <- num_to_letters(j - 1)
    sutun_no  <- i - 1
    nokta_adi <- paste0(satir_adi, sutun_no)
    
    grid_list[[idx]] <- data.frame(
      Nokta = nokta_adi,
      Satir = satir_adi,
      Sutun = sutun_no,
      dx_m = dx,
      dy_m = dy,
      X = round(P[1], 3),
      Y = round(P[2], 3)
    )
    
    idx <- idx + 1
  }
}

grid_df <- do.call(rbind, grid_list)

cat("\nIlk 20 grid noktasi:\n")
print(head(grid_df, 20))

# -------------------------
# 6) CSV EXPORT
# -------------------------
write.table(
  grid_df,
  "grid_noktalari_1m.csv",
  row.names = FALSE,
  sep = ";",
  fileEncoding = "UTF-8"
)

# -------------------------
# 7) SHP EXPORT
# -------------------------
grid_sf <- st_as_sf(
  grid_df,
  coords = c("X", "Y"),
  crs = 5255
)

st_write(
  grid_sf,
  "grid_noktalari_1m_epsg5255.shp",
  delete_layer = TRUE,
  quiet = TRUE
)

rect_coords <- matrix(
  c(
    A0[1], A0[2],
    A1[1], A1[2],
    B1[1], B1[2],
    B0[1], B0[2],
    A0[1], A0[2]
  ),
  ncol = 2,
  byrow = TRUE
)

rect_sf <- st_sf(
  Name = "Dikdortgen",
  geometry = st_sfc(st_polygon(list(rect_coords)), crs = 5255)
)

st_write(
  rect_sf,
  "dikdortgen_epsg5255.shp",
  delete_layer = TRUE,
  quiet = TRUE
)

# -------------------------
# 8) PLOTLY
# -------------------------
ana_noktalar <- data.frame(
  Nokta = c("Pt6", "Pt7", "Pt8", "Pt8_rect", "B0"),
  X = c(A0[1], A1[1], Pt8[1], Pt8_rect[1], B0[1]),
  Y = c(A0[2], A1[2], Pt8[2], Pt8_rect[2], B0[2])
)

sinir_df <- data.frame(
  Nokta = c("Pt6", "Pt7", "Pt8_rect", "B0", "Pt6"),
  X = c(A0[1], A1[1], Pt8_rect[1], B0[1], A0[1]),
  Y = c(A0[2], A1[2], Pt8_rect[2], B0[2], A0[2])
)

grid_df$hover_text <- paste0(
  "Nokta: ", grid_df$Nokta,
  "<br>Satir: ", grid_df$Satir,
  "<br>Sutun: ", grid_df$Sutun,
  "<br>dx_m: ", grid_df$dx_m,
  "<br>dy_m: ", grid_df$dy_m,
  "<br>X: ", format(grid_df$X, nsmall = 3),
  "<br>Y: ", format(grid_df$Y, nsmall = 3)
)

p <- plot_ly()

p <- p %>%
  add_markers(
    data = grid_df,
    x = ~X, y = ~Y,
    text = ~hover_text,
    hoverinfo = "text",
    name = "Grid_Noktalari",
    marker = list(size = 5)
  )

p <- p %>%
  add_paths(
    data = sinir_df,
    x = ~X, y = ~Y,
    name = "Dikdortgen_Siniri",
    line = list(width = 3, color = "red"),
    text = ~Nokta,
    hoverinfo = "text"
  )

p <- p %>%
  add_markers(
    data = ana_noktalar,
    x = ~X, y = ~Y,
    text = ~paste0(
      Nokta,
      "<br>X: ", format(X, nsmall = 3),
      "<br>Y: ", format(Y, nsmall = 3)
    ),
    hoverinfo = "text",
    name = "Ana_Noktalar",
    marker = list(
      size = 10,
      color = c("blue", "blue", "blue", "blue", "blue")
    )
  )

p <- p %>%
  add_text(
    data = ana_noktalar,
    x = ~X, y = ~Y,
    text = ~Nokta,
    textposition = "top center",
    showlegend = FALSE,
    hoverinfo = "none"
  )

p <- p %>%
  layout(
    title = "Pt6-Pt7 dogrultusuna gore en yakin dikdortgen ve 1 m grid",
    xaxis = list(title = "X"),
    yaxis = list(title = "Y", scaleanchor = "x", scaleratio = 1),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.15
    )
  )

p
