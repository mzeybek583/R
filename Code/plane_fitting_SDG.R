# Gerekli paketleri yükleme
library(rgl)

# Veri oluşturma
X1 <- runif(100, -10, 10)  # X1 bağımsız değişken (girdi)
X2 <- runif(100, -10, 10)  # X2 bağımsız değişken (girdi)
y <- 2*X1 + 3*X2 + 5 + rnorm(100, mean=0, sd=0.1)  # y bağımlı değişken (çıktı) + gürültü

# Model parametreleri
w1 <- 0.5  # Ağırlık 1
w2 <- 0.5  # Ağırlık 2
b <- 0.5  # Bias (kesim terimi)
learning_rate <- 0.01  # Öğrenme oranı
epochs <- 1000  # Toplam iterasyon sayısı
batch_size <- 1  # Mini-batch boyutu (SGD'de 1 olarak belirlenir)

# SGD algoritması
for (epoch in 1:epochs) {
  # Mini-batch oluşturma
  indices <- sample(1:length(X1), size=batch_size)
  X1_batch <- X1[indices]
  X2_batch <- X2[indices]
  y_batch <- y[indices]
  
  # Gradyan hesaplaması
  y_pred <- w1*X1_batch + w2*X2_batch + b
  dw1 <- mean((y_pred - y_batch) * X1_batch)
  dw2 <- mean((y_pred - y_batch) * X2_batch)
  db <- mean(y_pred - y_batch)
  
  # Parametre güncellemesi
  w1 <- w1 - learning_rate * dw1
  w2 <- w2 - learning_rate * dw2
  b <- b - learning_rate * db
  
  # Her 100 iterasyonda bir hata hesaplayıp ekrana yazdırma
  if (epoch %% 100 == 0) {
    y_pred <- w1*X1 + w2*X2 + b
    mse <- mean((y_pred - y) ^ 2)
    cat("Epoch:", epoch, ", MSE:", mse, "\n")
  }
}

# Eğitim sonrası tahmin
y_pred <- w1*X1 + w2*X2 + b
mse <- mean((y_pred - y) ^ 2)
cat("Eğitim sonrası tahmin - MSE:", mse, "\n")
cat("w1:", w1, ", w2:", w2, ", b:", b, "\n")

# 3B grafiği oluşturma
open3d()  # 3B grafiği açma
points3d(X1, X2, y, col="blue", size=3)  # Veri noktalarını eklen
points3d(X1, X2, y_pred, col="red", size=3)  # Tahmin noktalarını eklen
planes3d(w1, w2, -1, b, alpha=0.5, color="blue") # Oturtulan düzlemi eklen
grid3d(c("x", "y", "z"), n = 10)
title3d("SGD ile Duzlem Oturtma") # Grafiğe başlık ekleme
axes3d() # Eksenleri eklen
legend3d("topright", c("Veri Noktalari", "Tahmin Noktalari", "Duzlem"), col=c("blue", "red", "blue"), pch=c(16, 16, NA)) # Legend eklen
