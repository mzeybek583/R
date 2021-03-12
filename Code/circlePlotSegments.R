


# Load Libraries ----------------------------------------------------------

library(conicfit)
library(nabor)


# Load data or generate ---------------------------------------------------


# Calculate KASA Algebric------------------------------------------------------
xy<-calculateCircle(0,0,200,50,randomDist=TRUE,noiseFun=function(x) (x+rnorm(1,mean=0,sd=20)))
plot(xy[,1],xy[,2],xlim=c(-250,250),ylim=c(-250,250), asp=1, xlab="", ylab="");par(new=TRUE)
c3 <- CircleFitByKasa(xy)
xyc3<-calculateCircle(c3[1],c3[2],c3[3],720)
plot(xyc3[,1],xyc3[,2],xlim=c(-250,250),ylim=c(-250,250),col='green',type='l', asp=1)
par(new=TRUE)


nn1 <- knn(data=xyc3, query=xy, k=1)
 #idx <- c(nn1$nn.idx)
 idx <- nn1$nn.idx
 xy <- data.frame(xy)
 xyc3 <- data.frame(xyc3)
 
 a <- xyc3[idx]
 b <- xyc3[idx,2]
segments(a, b, xy[,1], xy[,2],  col= 'red', lwd=3)
