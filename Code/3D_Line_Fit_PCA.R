
## 3D line fitting with PCA

# Reference : https://stackoverflow.com/questions/39915328/fit-a-line-to-3d-point-cloud-in-r/39917318
if (!require("scatterplot3d")) install.packages("scatterplot3d", dependencies = TRUE)
if (!require("pdist")) install.packages("pdist", dependencies = TRUE)
if (!require("rgl")) install.packages("rgl", dependencies = TRUE)

### Load Libs.
library(scatterplot3d) 
library(pdist)
library(rgl)

set.seed(4208)
# Generate random numbers
x <- rnorm(50, mean = 2, sd=10)
y <- 0.5*x + 4 
y.noise <- rnorm(20, mean=0.2, sd=1)
y <- y + y.noise
z <- 0.3*x + 0.7*y 
z.noise <- rnorm(20, mean = 0.5, sd=1)
z <- z+ z.noise

cat("Program started!")
t <- proc.time()
xyz <- data.frame(x = x, y = y, z = z)
plot3d(xyz)

N <- nrow(xyz) 

mean_xyz <- apply(xyz, 2, mean)
xyz_pca   <- princomp(xyz)  # PCA
dirVector <- xyz_pca$loadings[, 1]   # PC1

xyz_fit <- matrix(rep(mean_xyz, each = N), ncol=3) + xyz_pca$score[, 1] %*% t(dirVector) 

t_ends <- c(min(xyz_pca$score[,1]) - 0.2, max(xyz_pca$score[,1]) + 0.2)  # for both ends of line
endpts <- rbind(mean_xyz + t_ends[1]*dirVector, mean_xyz + t_ends[2]*dirVector)

s3d <- scatterplot3d(xyz, type="b")
s3d$points3d(endpts, type="l", col="blue", lwd=2)
for(i in 1:N) s3d$points3d(rbind(xyz[i,], xyz_fit[i,]), type="l", col="green3", lty=2)

## 3D Plot
plot3d(xyz, type="s", rad=0.5)
abclines3d(mean_xyz, a = dirVector, col="blue", lwd=2)     # mean + t * direction_vector
for(i in 1:N) segments3d(rbind(xyz[i,], xyz_fit[i,]), col="red3", lwd=3)

out <- data.frame(matrix(rep(0,N), ncol = 1))
for (i in 1:N) { 
  dists <- pdist((xyz[i,]), (xyz_fit[i,])) 
  out[i,] <- dists@dist
}
str(out)
out <- as.numeric(unlist(out))
sprintf("Standard deviation of resid. %.2f",sd(out))
sprintf("Mean error of resid. %.2f",mean(out))
hist(out) # Hist plot
hh <- proc.time()-t
sprintf("Program ended in %.3f second" ,hh[3])
