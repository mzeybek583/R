
# Finding Nearest Point and Compare Distance

# Load Libraries ----------------------------------------------------------

library(FNN)   
library(ggpubr)
library("car")

# Read Test Data
H_RTK <- read.csv("coordinates.csv", sep = ";")
names(H_RTK) <- c("ID","x", "y", "z", "Lat_acc", "Lon_acc", "Z_acc") # Tidy up
H_RTK <- H_RTK[c(-1,-2,-5,-24),] #Remove unnecessary points/ comment if not necessary
H_RTK <- H_RTK[,c(-5,-6,-7)] # Remove accuracy values
#Read reference data
T_RTK <- read.csv("topcon-pol-koord.txt", header = F)
T_RTK <- data.frame(ID= T_RTK$V1,x=T_RTK$V3, y=T_RTK$V2, z=T_RTK$V4) # Tidy up data
T_RTK <- T_RTK[c(-2,-3,-7,-28,-29,-36),] #Remove unnecessary points/ comment if not necessary


# Find nearest data -------------------------------------------------------
neighbors1 <- get.knnx(H_RTK[, c(-1,-4,-5,-6,-7)], T_RTK[,c(-1,-4)], k=1)
str(neighbors1)
min(neighbors1$nn.dist); max(neighbors1$nn.dist);mean(neighbors1$nn.dist)

# Stats and plots
results <- data.frame(cbind(T_RTK, H_RTK[neighbors1$nn.index,]))
dx <- results$x- results$x.1
dy <- (results$y- results$y.1) - 0.025
dz <- results$z- results$z.1

results <- cbind(results, dx,dy,dz)

hist(dx); hist(dy) ; hist(dz)
m_dx <- mean(dx); m_dy <- mean(dy) ; m_dz <- mean(dz)
sd_dx <- sd(dx); sd_dy <- sd(dy); sd_dz <- sd(dz)
dnormx <- dnorm(dx, mean = m_dx, sd = sd_dx)
dnormy <- dnorm(dy, mean = m_dy, sd = sd_dy)
dnormz <- dnorm(dz, mean = m_dz, sd = sd_dz)

plot(dx, dnormx); plot(dy, dnormy); plot(dz, dnormz)

ggdensity(dx, 
          main = "Density plot of X",
          xlab = "X")
ggdensity(dy, 
          main = "Density plot of Y",
          xlab = "Y")
ggdensity(dz, 
          main = "Density plot of Z",
          xlab = "Z")

ggqqplot(dx); ggqqplot(dy); ggqqplot(dz)

# qqPlot(dx); qqPlot(dy); qqPlot(dz)

shapiro.test(dx); shapiro.test(dy); shapiro.test(dz)


plot(H_RTK$x, H_RTK$y, col="green")
points(T_RTK$x, T_RTK$y, pch=2, cex=0.5, col="red")
text(T_RTK$x, T_RTK$y, labels=T_RTK$ID, cex= 0.7,pos=2)
text(H_RTK$x, H_RTK$y, labels=H_RTK$ID, cex= 0.7,pos=1)
