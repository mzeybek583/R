# Function to convert degrees, minutes, seconds to decimal degrees
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split = ' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2] / 60 + y[3] / 3600
  })
  return(x)
}

# Test data
T1 <- c("321 56 06.09")

# Convert DMS to decimal degrees
T1 <- angle2dec(T1)
cat("Decimal Degrees:", T1, "\n")

# Given coordinates and constants
x1 <- -92276.440
x2 <- -9250.265
y1 <- 82130.142
y2 <- 17111.224
R <- 6373924.115

# Adjust the angle calculation
T1 <- T1 - ((180 / pi) / (6 * R^2)) * (x2 - x1) * (2 * y1 + y2) - 
  ((180 / pi) / (6 * R^2)) * (y1^2 + y1 * y2 + y2^2) * sin(T1 * pi / 180) * cos(T1 * pi / 180)
cat("Adjusted Decimal Degrees:", T1, "\n")

# Function to convert decimal degrees back to DMS
dec2dms <- function(decimal_degree) {
  deg <- floor(decimal_degree)
  min <- floor((decimal_degree - deg) * 60)
  sec <- (((decimal_degree - deg) * 60) - min) * 60
  return(sprintf("%d deg, %d min, %.2f sec", deg, min, sec))
}

# Convert back to degrees, minutes, seconds
dms_result <- dec2dms(T1)
cat("Degrees, Minutes, Seconds:", dms_result, "\n")
