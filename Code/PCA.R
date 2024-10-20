# Load necessary library
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("ggbiplot")) install.packages("ggbiplot", dependencies = TRUE)
library(ggplot2)
library(ggbiplot)

# Simulate a dataset if the original Kaggle glass data is not available
set.seed(123)
data <- data.frame(
  RI = rnorm(214, mean = 1.518, sd = 0.003),
  Na = rnorm(214, mean = 13.0, sd = 0.5),
  Mg = rnorm(214, mean = 2.5, sd = 1.5),
  Al = rnorm(214, mean = 1.4, sd = 0.5),
  Si = rnorm(214, mean = 72.6, sd = 0.5),
  K = rnorm(214, mean = 0.6, sd = 0.2),
  Ca = rnorm(214, mean = 8.9, sd = 1.5),
  Ba = rnorm(214, mean = 0.2, sd = 0.5),
  Fe = rnorm(214, mean = 0.05, sd = 0.1)
)

# Perform PCA on the dataset, excluding the target variable if there is one
pca_result <- prcomp(data, scale. = TRUE, center = TRUE)

# View a summary of the PCA results
summary(pca_result)

# Check PCA rotation (principal components' loadings)
pca_result$rotation

# Variance explained by each principal component
pca_variance <- pca_result$sdev^2
explained_variance <- pca_variance / sum(pca_variance)

# Scree plot of the variance explained by each principal component
x11(width = 6, height = 6)
plot(explained_variance, type = "b", pch = 19, xlab = "Principal Component", 
     ylab = "Variance Explained", main = "Scree Plot")

# Cumulative variance explained
x11(width = 6, height = 6)
plot(cumsum(explained_variance), type = "b", pch = 19, xlab = "Principal Component", 
     ylab = "Cumulative Variance Explained", main = "Cumulative Variance Explained")

# Biplot for PCA visualization
x11(width = 8, height = 6)
ggbiplot(pca_result, obs.scale = 1, var.scale = 1, 
         ellipse = TRUE, circle = TRUE) +
  theme_minimal() +
  labs(title = "PCA Biplot", x = "PC1", y = "PC2")

# Close graphics devices to avoid potential overflow issues
dev.off()
