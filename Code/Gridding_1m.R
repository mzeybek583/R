library(sf)
library(dplyr)

# --- 1. REFERANS NOKTALAR ---
ref_pts <- data.frame(
  Name = c("Pt1", "Pt2", "Pt3", "Pt4"),
  X = c(4127786.068, 4127772.944, 4127787.337, 4127800.157),
  Y = c(474981.330, 474956.046, 474948.600, 474973.915)
)
dd <- 1 #m

# Grid yönlerini belirle
Pt1 <- c(ref_pts$X[1], ref_pts$Y[1])
Pt2 <- c(ref_pts$X[2], ref_pts$Y[2])
Pt4 <- c(ref_pts$X[4], ref_pts$Y[4])

v_row <- Pt2 - Pt1  # satır yönü (A, B, ...)
v_col <- Pt4 - Pt1  # sütun yönü (1, 2, ...)

# Grid boyutu
n_row <- floor(sqrt(sum(v_row^2)) / dd)
n_col <- floor(sqrt(sum(v_col^2)) / dd)

# Harf etiketleme fonksiyonu (A-Z, AA-ZZ, ...)
excel_letters <- function(n) {
  result <- character(n)
  for (i in 1:n) {
    temp <- i
    str <- ""
    while (temp > 0) {
      mod <- (temp - 1) %% 26
      str <- paste0(LETTERS[mod + 1], str)
      temp <- (temp - mod - 1) %/% 26
    }
    result[i] <- str
  }
  return(result)
}

# Grid oluşturma
grid_points <- list()
labels <- character()
row_letters <- excel_letters(n_row + 1)

for (i in 0:n_row) {
  for (j in 0:n_col) {
    coord <- Pt1 + i * dd * (v_row / sqrt(sum(v_row^2))) + j * dd * (v_col / sqrt(sum(v_col^2)))
    grid_points <- append(grid_points, list(coord))
    label <- paste0(row_letters[i + 1], j + 1)
    labels <- c(labels, label)
  }
}

# Grid dataframe
grid_df <- do.call(rbind, grid_points) %>% as.data.frame()
colnames(grid_df) <- c("X", "Y")
grid_df$Name <- labels
grid_df$Type <- "Grid"

# Referans noktaları ekle
ref_pts$Type <- "Ref"
combined <- bind_rows(
  ref_pts[, c("X", "Y", "Name", "Type")],
  grid_df[, c("X", "Y", "Name", "Type")]
)

# sf objesine dönüştür
sf_all <- st_as_sf(combined, coords = c("Y", "X"), crs = 5255)

# Kaydet
st_write(sf_all, "yonlu_grid_ref_noktalar.shp", delete_dsn = TRUE)
write.csv(combined, "grid_noktalar.csv")
