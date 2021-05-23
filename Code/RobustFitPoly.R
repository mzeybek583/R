
library(pracma)
library(MASS)

# Fitting the sine function by a polynomial
x <- seq(0, pi, length.out=25)
y <- sin(x)

# Add noise
x <- c(x,0.5)
x <- sort(x)
y <- c(y, 0.9)

p <- polyfit(x, y, 3)

## Plot sin and fitted polynomial
   plot(x, y, type="p")
   yf <- polyval(p, x)
   lines(x, yf, col="red")
   grid()## End(Not run)
  
#Linear regression Fit

   lm.fit <- lm(y~ poly(x,6))
  lines(x, lm.fit$fitted.values, col="green")

  
   #Robust Poly Fit

  r.fit <- rlm(y~poly(x,3))
  summary(r.fit) 
  lines(x, r.fit$fitted.values, col="black", lwd=2, lty=2)

  legend(x="bottomright", c("PolyFit 3", "LmFit 6", "RobustFit 3"), col=c("red", "green", "black"), lty=c(1,1,2))## End(Not run)
  
  
