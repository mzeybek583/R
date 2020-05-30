##
# Hat aplikasyonu
#Coder Assist. Prof. Dr. Mustafa Zeybek

# set working directory

setwd("Desktop/") # change

#Enter points

p1 <- c(455450.00,4435670.00)
p2 <- c(455500.00,4435676.00)
pts <- data.frame(rbind(p1,p2))
colnames(pts) <- c("X", "Y")
#plot(pts)

# Compute azimuth
semt <- atan((pts$X[2]- pts$X[1])/(pts$Y[2]- pts$Y[1]))*200/pi
if ((pts$Y[2]- pts$Y[1])==0 && pts$X[2]>pts$X[1]) {
  semt <- 100
} else if ((pts$Y[2]- pts$Y[1])==0 && pts$X[2]<pts$X[1]) {
  semt <- 200
}
delta_x <- pts$X[2]-pts$X[1]
delta_y <- pts$Y[2]-pts$Y[1]
if ((delta_x)>0 & (delta_y)>0) {
  semt<- atan((delta_x)/(delta_y))*200/pi   
} else if ((delta_x)>0 & (delta_y)<0){
  semt<- atan((delta_x)/(delta_y))*200/pi+200   
} else if ((delta_x)<0 & (delta_y)<0) {
  semt<- atan((delta_x)/(delta_y))*200/pi+200   
} else {
  semt<- atan((delta_x)/(delta_y))*200/pi+400
}

# setting out interval
# 10 m aralıklarda hat aplikasyonu

dist <- sqrt((pts$X[2]- pts$X[1])^2 + (pts$Y[2]- pts$Y[1])^2)
ite <- seq(10, dist-0.1, by=10)
apk <- data.frame()
n <- length(ite)
i= 1
for (i in 1:n) {
  print(i)
  out1 <- pts$Y[1] + ite[i]*cos(semt*pi/200)
  print(out1)
  out2 <- pts$X[1] + ite[i]*sin(semt*pi/200)
  print(out2)
  apk[i,1] <- out2
  apk[i,2] <- out1
}

## graphics
plot(pts, col= "green")
lines(pts)
points(apk$V1, apk$V2, col="red", type = "p")
grid()
colnames(apk) <- c("X", "Y")
# export
write.csv(format(round(apk,3), nsmall=3), "hat.dat", quote = FALSE)
