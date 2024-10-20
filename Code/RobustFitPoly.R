# Load necessary libraries
if (!require("pracma")) install.packages("pracma", dependencies = TRUE)
if (!require("MASS")) install.packages("MASS", dependencies = TRUE)
library(pracma)
library(MASS)

# Generate data points for fitting the sine function
x <- seq(0, pi, length.out = 25)
y <- sin(x)

# Add noise to the data
x <- c(x, 0.5)
x <- sort(x)
y <- c(y, 0.9)

# Fit a 3rd-degree polynomial to the noisy sine data
p <- polyfit(x, y, 3)

# Plot the original data points and the polynomial fit
plot(x, y, type = "p", main = "Polynomial Fitting Comparison",
     xlab = "x", ylab = "y", pch = 16)
yf <- polyval(p, x)
lines(x, yf, col = "red", lwd = 2)
grid()

# Linear regression fit using a 6th-degree polynomial
lm_fit <- lm(y ~ poly(x, 6))
lines(x, lm_fit$fitted.values, col = "green", lwd = 2)

# Robust polynomial fit using a 3rd-degree polynomial
r_fit <- rlm(y ~ poly(x, 3))
lines(x, r_fit$fitted.values, col = "black", lwd = 2, lty = 2)

# Display a summary of the robust fit
summary(r_fit)

# Add a legend to distinguish the different fits
legend("bottomright", legend = c("PolyFit 3rd Degree", "LmFit 6th Degree", "RobustFit 3rd Degree"),
       col = c("red", "green", "black"), lty = c(1, 1, 2), lwd = 2, bty = "n")
