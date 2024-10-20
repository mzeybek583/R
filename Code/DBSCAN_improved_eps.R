# Gerekli Paketler
if (!require("dbscan")) install.packages("dbscan", dependencies = TRUE)
if (!require("rgl")) install.packages("rgl", dependencies = TRUE)


library(dbscan)
library(rgl)

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

# Yerel Yoğunluk Hesaplama
k_nearest_neighbors <- 10
distances <- kNN(points, k = k_nearest_neighbors)$dist
local_density <- apply(distances, 1, mean)

# Adaptif Epsilon Hesaplama
adaptive_eps <- function(density) {
  min_eps <- 0.3
  max_eps <- 1.0
  # Yerel yoğunluklar aralığında ölçekleme yaparak epsilon değerini ayarla
  scale_density <- (density - min(local_density)) / (max(local_density) - min(local_density))
  return(min_eps + (max_eps - min_eps) * (1 - scale_density))
}

# Her bir nokta için adaptif epsilon hesaplama
eps_values <- adaptive_eps(local_density)

# DBSCAN Modifikasyonu
modified_dbscan <- function(points, eps_values, minPts) {
  clusters <- rep(-1, nrow(points))
  cluster_id <- 0
  
  for (i in 1:nrow(points)) {
    if (clusters[i] != -1) {
      next
    }
    
    # Yerel epsilon değerine göre komşuları bul
    neighbors <- which(rowSums((points - points[i, ])^2) <= eps_values[i]^2)
    
    if (length(neighbors) < minPts) {
      clusters[i] <- 0  # Gürültü
    } else {
      cluster_id <- cluster_id + 1
      clusters[neighbors] <- cluster_id
      expand_cluster <- neighbors
      
      while (length(expand_cluster) > 0) {
        current_point <- expand_cluster[1]
        expand_cluster <- expand_cluster[-1]
        
        current_neighbors <- which(rowSums((points - points[current_point, ])^2) <= eps_values[current_point]^2)
        
        if (length(current_neighbors) >= minPts) {
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
minPts <- 600
cluster_result <- modified_dbscan(points, eps_values, minPts)

# Negatif ve sıfır değerlerden kaçınarak renk indekslerini ayarlama
cluster_result[cluster_result == -1] <- max(cluster_result) + 1  # Gürültü için farklı renk
cluster_result[cluster_result == 0] <- max(cluster_result) + 2   # Tanımsız noktalar için farklı renk

# Sonuçları Görselleştirme
plot3d(points, col = cluster_result + 1, size = 5)
