library(pracma)
# set.seed(8421)

### 1. Adım
n  <- 20
w  <- 2*pi*runif(n)
xp <- cos(w) + 1 + 0.25 * (runif(n) - 0.5)
yp <- sin(w) + 1 + 0.25 * (runif(n) - 0.5)

### 2. Adım
circle <- circlefit(xp, yp)  #=> 0.9899628 1.0044920 1.0256633
# RMS error: 0.07631986 
## Not run: 
x0 <- circle[1]; y0 <- circle[2]; r0 <- circle[3]

### 3. Adım
plot(c(-0.2, 2.2), c(-0.2, 2.2), type="n", asp=1, xlab = "X", ylab = "Y")
grid()
abline(h=x0, col="gray"); abline(v=y0, col="gray")
points(xp, yp, pch=19,col="red")
w  <- seq(0, 2*pi, len=100)
xx <- r0 * cos(w) + x0
yy <- r0 * sin(w) + y0
lines(xx, yy, col="blue")
## End(Not run)
