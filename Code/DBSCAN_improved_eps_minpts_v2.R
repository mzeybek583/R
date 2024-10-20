# Gerekli Paketler
packages <- c("dbscan", "rgl", "stats", "factoextra", "ggplot2", 
              "parallel", "cluster", "microbenchmark", "clusterCrit")

for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Windows için paralel işlem kümesi oluşturma
cl <- makeCluster(max(1, detectCores() - 1))

# Veri Üretimi
set.seed(123)
n_points <- 1000

generate_cluster <- function(n, mean, sd) {
  matrix(rnorm(n * 3, mean = mean, sd = sd), ncol = 3)
}

points <- do.call(rbind, list(
  generate_cluster(n_points, 0, 0.3),
  generate_cluster(n_points, 5, 0.3),
  generate_cluster(n_points, 2.5, 1.2)
))

# Yoğunluk Hesaplama
calculate_density <- function(points) {
  knn_res <- dbscan::kNN(points, k = 8)
  density_values <- apply(knn_res$dist, 1, mean)
  return(density_values)
}

# Adaptif Epsilon Hesaplama
calculate_epsilon <- function(density_values) {
  min_eps <- 0.3
  max_eps <- 0.8
  normalized_density <- (density_values - min(density_values)) / 
    (max(density_values) - min(density_values))
  eps_values <- min_eps + (max_eps - min_eps) * normalized_density
  return(eps_values)
}

# MinPts Hesaplama
calculate_minpts <- function(density_values) {
  quantiles <- quantile(density_values, probs = c(0.25, 0.75))
  minpts <- numeric(length(density_values))
  
  for(i in 1:length(density_values)) {
    if(density_values[i] < quantiles[1]) {
      minpts[i] <- 10  # Yoğun bölgeler için
    } else if(density_values[i] < quantiles[2]) {
      minpts[i] <- 15  # Orta yoğunluklu bölgeler için
    } else {
      minpts[i] <- 20  # Seyrek bölgeler için
    }
  }
  
  return(minpts)
}

# İyileştirilmiş DBSCAN
modified_dbscan <- function(points, eps_values, minpts_values) {
  n_points <- nrow(points)
  clusters <- rep(-1, n_points)
  cluster_id <- 0
  
  dist_matrix <- as.matrix(dist(points))
  
  for(i in 1:n_points) {
    if(clusters[i] != -1) next
    
    neighbors <- which(dist_matrix[i, ] <= eps_values[i])
    
    if(length(neighbors) < minpts_values[i]) {
      clusters[i] <- 0
      next
    }
    
    cluster_id <- cluster_id + 1
    clusters[i] <- cluster_id
    
    seed_points <- neighbors[neighbors != i]
    
    while(length(seed_points) > 0) {
      current <- seed_points[1]
      seed_points <- seed_points[-1]
      
      if(clusters[current] == -1) {
        clusters[current] <- cluster_id
        
        current_neighbors <- which(dist_matrix[current, ] <= eps_values[current])
        
        if(length(current_neighbors) >= minpts_values[current]) {
          seed_points <- unique(c(seed_points, 
                                  current_neighbors[clusters[current_neighbors] == -1]))
        }
      }
    }
  }
  
  return(clusters)
}

# Performans Değerlendirme
evaluate_clustering <- function(points, clusters) {
  # Gürültü noktalarını çıkar
  valid_idx <- clusters != 0
  if(sum(valid_idx) <= 1 || length(unique(clusters[valid_idx])) <= 1) {
    return(list(
      silhouette = NA,
      dunn_index = NA,
      calinski_harabasz = NA
    ))
  }
  
  valid_points <- points[valid_idx, ]
  valid_clusters <- clusters[valid_idx]
  
  # Küme etiketlerini 1'den başlayarak sıralı tam sayılara dönüştür
  unique_clusters <- unique(valid_clusters)
  cluster_map <- setNames(seq_along(unique_clusters), unique_clusters)
  valid_clusters_mapped <- unname(cluster_map[as.character(valid_clusters)])
  
  # Silhouette skoru
  sil <- silhouette(valid_clusters_mapped, dist(valid_points))
  avg_sil <- mean(sil[, 3])
  
  # Dunn indeksi
  dunn <- intCriteria(
    traj = as.matrix(valid_points),
    part = valid_clusters_mapped,
    crit = "Dunn"
  )$dunn
  
  # Calinski-Harabasz indeksi
  ch_index <- intCriteria(
    traj = as.matrix(valid_points),
    part = valid_clusters_mapped,
    crit = "Calinski_Harabasz"
  )$calinski_harabasz
  
  return(list(
    silhouette = avg_sil,
    dunn_index = dunn,
    calinski_harabasz = ch_index,
    silhouette_obj = sil
  ))
}

# Ana işlem akışı
t_start <- Sys.time()

# Yoğunluk ve parametre hesaplama
density_values <- calculate_density(points)
eps_values <- calculate_epsilon(density_values)
minpts_values <- calculate_minpts(density_values)

# Kümeleme
cluster_result <- modified_dbscan(points, eps_values, minpts_values)

# Performans değerlendirme
metrics <- evaluate_clustering(points, cluster_result)

t_end <- Sys.time()
execution_time <- difftime(t_end, t_start, units = "secs")

# Sonuçları yazdırma
cat("\nPerformans Metrikleri:\n")
cat("Calisma Suresi:", execution_time, "saniye\n")
cat("Silhouette Skoru:", metrics$silhouette, "\n")
cat("Dunn İndeksi:", metrics$dunn_index, "\n")
cat("Calinski-Harabasz İndeksi:", metrics$calinski_harabasz, "\n")

# Küme istatistikleri
n_clusters <- length(unique(cluster_result[cluster_result != 0]))
noise_points <- sum(cluster_result == 0)
total_points <- length(cluster_result)

cat("\nKumeleme Istatistikleri:\n")
cat("Toplam Kume Sayısı:", n_clusters, "\n")
cat("Gurultu Noktasi Sayisi:", noise_points, 
    sprintf("(%.2f%%)", 100 * noise_points/total_points), "\n")

# Görselleştirmeler
# 3D Plot
rgl::plot3d(points, col = rainbow(max(cluster_result) + 1)[cluster_result + 1], 
            size = 3, xlab = "X", ylab = "Y", zlab = "Z")

# Küme Dağılımı Pasta Grafiği
cluster_dist <- table(factor(cluster_result, 
                             levels = c(0:max(cluster_result))))
pie(cluster_dist, 
    main = "Kume Dagilimi",
    labels = paste("Kume", names(cluster_dist), 
                   "\n", cluster_dist, "nokta"),
    col = rainbow(length(cluster_dist)))

# Silhouette Plot
if(!is.na(metrics$silhouette)) {
  plot(metrics$silhouette_obj, main = "Silhouette Analizi")
}

# Paralel işlem kümesini kapatma
stopCluster(cl)
