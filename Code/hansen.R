
# Hansen problem
#https://www.wikiwand.com/en/Hansen%27s_problem
# Assist. Prof. Dr. Mustafa Zeybek


set.seed(4208)
#Params
ro <- 200/pi

# Observations
#coords
N1 <- c(4559950.9, 418583.68)
N2 <- c(4560051.805, 424068.155)

#Angle
a1 <- 75.39444 
a2 <- 5.14516
b1 <- 34.30717
b2 <- 140.0562
a <- a1 + a2
b <- b1 + b2
veri <- data.frame(N1,N2)
veri
delta <- veri$N2-veri$N1

delta_x <- delta[2]
delta_y <- delta[1]
if ((delta_x)>0 & (delta_y)>0) {
  semt<- atan((delta_x)/(delta_y))*200/pi   
} else if ((delta_x)>0 & (delta_y)<0){
  semt<- atan((delta_x)/(delta_y))*200/pi+200   
} else if ((delta_x)<0 & (delta_y)<0) {
  semt<- atan((delta_x)/(delta_y))*200/pi+200   
} else {
  semt<- atan((delta_x)/(delta_y))*200/pi+400
}

N1N2.dist <- sqrt(delta_x^2 + delta_y^2)

nu <- atan(sin(a1/ro)*sin(b1/ro)*sin((a2+b)/ro) / (sin((a+b1)*1/ro)*sin(a2/ro)*sin(b2/ro)))*ro
fi_ksi <- (a2+b1)/2
fi_eksi <- atan(tan(fi_ksi/ro) * 1/tan((50+nu)/ro))*ro

fi <- fi_ksi + fi_eksi
ksi <- fi_ksi - fi_eksi

fi.u <- 200 -(a1+ksi)

semt1 <- semt + fi.u

dist.1 <- N1N2.dist*sin(ksi/ro)/sin(a1/ro)

x1 <- veri$N1[1] + dist.1*cos(semt1/ro)
y1 <- veri$N1[2] + dist.1*sin(semt1/ro)

fi.u2 <- fi.u - fi 
if (semt1>200) {
  semt2 <- semt1 - 200 + a
}else{
  semt2 <- semt1 + 200 + a
}
if (semt2 > 400) {
  semt2 <- semt2-400
}

dist.2 <- dist.1* sin(fi.u2/ro)/sin(b1/ro)

x2 <- x1 + dist.2*cos(semt2/ro)
y2 <- y1 + dist.2*sin(semt2/ro)

#control

ksi.u <- 200- (a2+b)


if (semt2>200) {
  semt3 <- semt2 - 200 + b
}else{
  semt3 <- semt2 + 200 + b
}
if (semt3 > 400) {
  semt3 <- semt3-400
}

dist.3 <- dist.2 * sin(a2/ro) / sin(ksi.u/ro)

Cx <- x2 + dist.3*cos(semt3/ro)
Cy <- y2 + dist.3*sin(semt3/ro)

# Deviations
veri$N2[1] - Cx
veri$N2[2] - Cy

#Plot

df <- data.frame(cbind(veri,c(x1,y1),c(x2,y2)))
xlim <- c(min(df[2,])-50, max(df[2,]+50))
ylim <- c(min(df[1,])-50, max(df[1,]+50))
colnames(df) <- c("N1","N2","P1","P2")
plot(df$N1[2],df$N1[1], xlab = "Y", ylab = "X", xlim = xlim, ylim = ylim, asp = 1, pch=2)
points(df$N2[2],df$N2[1], pch=2)
points(df$P1[2],df$P1[1], col="red")
points(df$P2[2],df$P2[1], col="red")
text(df[2,],df[1,], labels = colnames(df), pos = 4, col = "green")
