# Load Required Library
if (!require("lubridate")) install.packages("lubridate", dependencies = TRUE)
library(lubridate)

# Initial XYZ coordinates and velocities
xyz.t0 <- matrix(c(4211499.0334, 2218482.8809, 4231454.2803))

# Date and time calculations
gun <- ymd("2018-06-26")
t0 <- decimal_date(gun)  # Epok (2018.482)

t <- 2005.0  # Reference Epok

# Velocities for XYZ coordinates
v.xyz <- matrix(c(-0.0232, 0.0168, -0.0005))

# Calculate the new XYZ coordinates at time 't'
xyz.t <- xyz.t0 + (t0 - t) * v.xyz

# Output XYZ coordinates
out <- data.frame(xyz.t)
rownames(out) <- c("X", "Y", "Z")
print.data.frame(out, digits = 11)

# Define coordinates for interpolation
x1 <- 4636534.985
x2 <- 4651609.513
x3 <- 4614475.580
y1 <- 493514.385
y2 <- 534885.907
y3 <- 523649.465
xp <- 4627235.171
yp <- 516175.362

P <- matrix(c(yp, xp), ncol = 2, byrow = TRUE)

# Define velocity vectors
v1 <- c(-0.0110, 0.0131, 0.0113)
v2 <- c(-0.0135, 0.0130, 0.0103)
v3 <- c(-0.0124, 0.0128, 0.0107)

# Calculate interpolated velocities using barycentric coordinates
interpolated_velocity <- (
  ((x2 - xp) * (y3 - y2) - (y2 - yp) * (x3 - x2)) / ((x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)) * v1 +
    ((x3 - xp) * (y1 - y3) - (y3 - yp) * (x1 - x3)) / ((x3 - x2) * (y1 - y3) - (y3 - y2) * (x1 - x3)) * v2 +
    ((x1 - xp) * (y2 - y1) - (y1 - yp) * (x2 - x1)) / ((x1 - x3) * (y2 - y1) - (y1 - y3) * (x2 - x1)) * v3
)

# Print the interpolated velocities
cat("Interpolated Velocity:", interpolated_velocity, "\n")

# Define matrix and velocity components for linear model
A <- matrix(c(y1, x1, y2, x2, y3, x3), ncol = 2, byrow = TRUE)
Vxyz <- matrix(c(v1, v2, v3), ncol = 3, byrow = TRUE)

# Fit linear model
fit <- lm(Vxyz ~ A)

# Summary of the fit
summary(fit)

# Coefficients for the fit
fit$coefficients

# Predict using coefficients
pred_x <- fit$coefficients[1, 1] + fit$coefficients[2, 1] * yp + fit$coefficients[3, 1] * xp
pred_y <- fit$coefficients[1, 2] + fit$coefficients[2, 2] * yp + fit$coefficients[3, 2] * xp
pred_z <- fit$coefficients[1, 3] + fit$coefficients[2, 3] * yp + fit$coefficients[3, 3] * xp

cat("Predicted X:", pred_x, "\n")
cat("Predicted Y:", pred_y, "\n")
cat("Predicted Z:", pred_z, "\n")

# Plotting - Visualization over a 10-year period
t.diff <- 1:10

# Function to compute new positions based on velocity
f1 <- function(x) xyz.t0 + x * v.xyz

# Apply function to calculate positions
out <- t(sapply(t.diff, f1))

# Extract new X and Y positions
X.new <- out[, 1]
Y.new <- out[, 2]

# Generate the plot
plot(X.new, Y.new, ylim = c(min(Y.new), max(Y.new) + 0.1), xlab = "X", ylab = "Y", 
     main = "10-Year Period Position Shift", pch = 16, col = "blue")
arrows(X.new[-10], Y.new[-10], X.new[-1], Y.new[-1], col = "red", length = 0.1)
text(X.new, Y.new + 0.009, labels = t.diff, cex = 0.9, font = 2)
