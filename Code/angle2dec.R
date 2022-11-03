# Degree min sec to degree decimal
T1 <- c("321 56 06.09")
  
  
angle2dec <- function(angle) {
    angle <- as.character(angle)
    x <- do.call(rbind, strsplit(angle, split=' '))
    x <- apply(x, 1L, function(y) {
      y <- as.numeric(y)
      y[1] + y[2]/60 + y[3]/3600
    })
    return(x)
  }

T1 <- angle2dec(T1)
T1

# Calculate oordinate

x1 <- -92276.440
x2 <- -9250.265
y1 <- 82130.142
y2 <- 17111.224
R <- 6373924.115

T1 <- T1 - ((180/pi)/(6*R^2))*(x2-x1)*(2*y1+y2) -  ((180/pi))/(6*R^2)*(y1^2+y1*y2+y2^2)*sin(T1*pi/180)*cos(T1*pi/180)
T1

# Degree decimal to deg min sec.

deg <- floor(T1)
min <- floor(T1%%1 * 60)
sec <- min%%1 * 60

print(sprintf("%1.0f deg, %1.0f min, %1.2f sec",deg,min,sec))
