# Gerekli Paketler
if (!require("dbscan")) install.packages("dbscan", dependencies = TRUE)
if (!require("rgl")) install.packages("rgl", dependencies = TRUE)
if (!require("stats")) install.packages("stats", dependencies = TRUE)
if (!require("factoextra")) install.packages("factoextra", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)

library(dbscan)
library(rgl)
library(stats)
library(factoextra)
library(ggplot2)

# Örnek Nokta Bulutu Üretimi
set.seed(123)
n_points <- 1000

# Yoğun bölgeler oluşturma
dense_cluster1 <- matrix(rnorm(n_points * 3, mean = 0, sd = 0.3), ncol = 3)
dense_cluster2 <- matrix(rnorm(n_points * 3, mean = 5, sd = 0.3), ncol = 3)

# Seyrek bölgeler oluşturma
sparse_cluster <- matrix(rnorm(n_points * 3, mean = 2.5, sd = 1.2), ncol = 3)

# Nokta bulutlarını birleştirme
points <- rbind(dense_cluster1, dense_cluster2, sparse_cluster)

# 3B Nokta Bulutunu Görselleştirme
plot3d(points, col = "blue", size = 3)

# Elbow Yöntemi ile Optimal `k` Değerini Belirleme
find_optimal_k <- function(points, max_k = 10) {
  wcss <- numeric(max_k)
  
  for (k in 1:max_k) {
    kmeans_result <- kmeans(points, centers = k, nstart = 100)  # İterasyon sayısını artırdık
    wcss[k] <- kmeans_result$tot.withinss
  }
  
  # WCSS Grafiği Çizme
  data <- data.frame(k = 1:max_k, WCSS = wcss)
  ggplot(data, aes(x = k, y = WCSS)) +
    geom_point() + geom_line() +
    labs(title = "Elbow Yöntemi ile Optimal K", x = "Küme Sayısı (k)", y = "Toplam Hata Kareleri (WCSS)") +
    theme_minimal()
  
  # Optimal `k` değerini belirleme (dirseğin oluştuğu nokta)
  optimal_k <- which(diff(diff(wcss)) == min(diff(diff(wcss)))) + 1
  return(optimal_k)
}

# Optimal `k` Değerini Hesapla
optimal_k <- find_optimal_k(points)
cat("Optimal k değeri:", optimal_k, "\n")

# Optimal `k` ile Kümeleme
kmeans_result <- kmeans(points, centers = optimal_k, nstart = 100)
point_clusters <- kmeans_result$cluster

# Yoğunluk Hesaplama ve Yerel `minPts` Belirleme
calculate_local_density <- function(points, point_clusters) {
  local_density <- numeric(length(point_clusters))
  
  for (i in unique(point_clusters)) {
    cluster_points <- points[point_clusters == i, ]
    local_density[point_clusters == i] <- apply(kNN(cluster_points, k = 8)$dist, 1, mean)
  }
  
  return(local_density)
}

# Adaptif Epsilon Hesaplama
adaptive_eps <- function(local_density) {
  min_eps <- 0.5
  max_eps <- 1.0
  scale_density <- (local_density - min(local_density)) / (max(local_density) - min(local_density))
  return(min_eps + (max_eps - min_eps) * (1 - scale_density))
}

# Yerel Yoğunluk ve Adaptif Epsilon Hesaplama
local_density <- calculate_local_density(points, point_clusters)
eps_values <- adaptive_eps(local_density)

# Yerel `minPts` Belirleme
calculate_local_minPts <- function(points, point_clusters) {
  local_minPts <- numeric(length(point_clusters))
  
  for (i in unique(point_clusters)) {
    cluster_points <- points[point_clusters == i, ]
    local_density <- mean(dist(cluster_points))
    
    # Yoğunluğa göre dinamik minPts belirleme
    if (local_density < 0.5) {
      local_minPts[point_clusters == i] <- 500  # Yoğun alanlar için daha düşük minPts
    } else if (local_density < 1.0) {
      local_minPts[point_clusters == i] <- 600  # Orta yoğunluk
    } else {
      local_minPts[point_clusters == i] <- 700  # Seyrek alanlar için daha yüksek minPts
    }
  }
  
  return(local_minPts)
}

# Her nokta için adaptif `minPts` değerini hesapla
minPts_values <- calculate_local_minPts(points, point_clusters)

# Modifiye DBSCAN algoritmasını güncelle
modified_dbscan_adaptive <- function(points, eps_values, minPts_values) {
  clusters <- rep(-1, nrow(points))
  cluster_id <- 0
  
  for (i in 1:nrow(points)) {
    if (clusters[i] != -1) {
      next
    }
    
    neighbors <- which(rowSums((points - points[i, ])^2) <= eps_values[i]^2)
    
    if (length(neighbors) < minPts_values[i]) {
      clusters[i] <- 0  # Gürültü
    } else {
      cluster_id <- cluster_id + 1
      clusters[neighbors] <- cluster_id
      expand_cluster <- neighbors
      
      while (length(expand_cluster) > 0) {
        current_point <- expand_cluster[1]
        expand_cluster <- expand_cluster[-1]
        
        current_neighbors <- which(rowSums((points - points[current_point, ])^2) <= eps_values[current_point]^2)
        
        if (length(current_neighbors) >= minPts_values[current_point]) {
          new_points <- current_neighbors[clusters[current_neighbors] == -1]
          clusters[new_points] <- cluster_id
          expand_cluster <- c(expand_cluster, new_points)
        }
      }
    }
  }
  
  return(clusters)
}

# Adaptif DBSCAN Uygulama
cluster_result <- modified_dbscan_adaptive(points, eps_values, minPts_values)

# Negatif ve sıfır değerlerden kaçınarak renk indekslerini ayarlama
cluster_result[cluster_result == -1] <- max(cluster_result) + 1  # Gürültü için farklı renk
cluster_result[cluster_result == 0] <- max(cluster_result) + 2   # Tanımsız noktalar için farklı renk

# Sonuçları Görselleştirme
plot3d(points, col = cluster_result + 1, size = 3)

