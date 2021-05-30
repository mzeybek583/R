

## Epok Kaydırma
# Hız hesabı
# Library

library(lubridate)

# x <- c(1998.0, 2000.45, 2005, 2018.485)
# f <- format(date_decimal(x), "%d-%m-%Y")
#f

xyz.t0 <- matrix(c(4211499.0334, 2218482.8809, 4231454.2803))

gun <- ymd("2018-06-26")
t0 <-decimal_date(gun)  # 2018.482

t <- 2005.0 # Epok

v.xyz <- matrix(c(-0.0232, 0.0168, -0.0005))

# Formul XYZ <- XYZ + (T-T0)*V

xyz.t <- xyz.t0 + (t0-t)*v.xyz

out <- data.frame(xyz.t)
rownames(out) <- c("X", "Y", "Z")

print.data.frame(out, digits = 11)




