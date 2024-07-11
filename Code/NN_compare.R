
# Finding Nearest Point and Compare Distance

# Load Libs
library(FNN)    

# Read Test Data
H_RTK <- read.csv("coordinates.csv", sep = ";")
names(H_RTK) <- c("ID","x", "y", "z", "Lat_acc", "Lon_acc", "Z_acc")

#Remove unnecessary points
H_RTK <- H_RTK[c(-1,-2,-5,-24),]

#Read reference data
T_RTK <- read.csv("topcon-pol-koord.txt", header = F)

T_RTK <- data.frame(ID= T_RTK$V1,x=T_RTK$V3, y=T_RTK$V2, z=T_RTK$V4)
#Remove unnecessary points

T_RTK <- T_RTK[c(-2,-3,-7,-28,-29,-36),]

neighbors1 <- get.knnx(H_RTK[, c(-1,-4,-5,-6,-7)], T_RTK[,c(-1,-4)], k=1)
str(neighbors1)
min(neighbors1$nn.dist)
max(neighbors1$nn.dist)
mean(neighbors1$nn.dist)

# Stats
results <- data.frame(cbind(T_RTK, H_RTK[neighbors1$nn.index,c(-5,-6,-7)]))
dx <- results$x- results$x.1
dy <- (results$y- results$y.1) - 0.025
dz <- results$z- results$z.1
results <- cbind(results, dx,dy,dz)
hist(dx)
hist(dy)
hist(dz)
mean(dx)
mean(dy)
mean(dz)
sd(dx)
sd(dy)
sd(dz)


plot(H_RTK$x, H_RTK$y, col="green")
points(T_RTK$x, T_RTK$y, pch=2, cex=0.5, col="red")
text(T_RTK$x, T_RTK$y, labels=T_RTK$ID, cex= 0.7,pos=2)
text(H_RTK$x, H_RTK$y, labels=H_RTK$ID, cex= 0.7,pos=1)
