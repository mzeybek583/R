

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

x1 <- 4636534.985
x2 <- 4651609.513
x3 <- 4614475.580
y1 <- 493514.385
y2 <- 534885.907
y3 <- 523649.465
xp <- 4627235.171
yp <- 516175.362
P <- matrix(c(yp,xp), ncol=2, byrow = TRUE)
v1 <- c(-0.0110, 0.0131, 0.0113)
v2 <- c(-0.0135, 0.0130, 0.0103)
v3 <- c(-0.0124, 0.0128, 0.0107)

((x2-xp)*(y3-y2)-(y2-yp)*(x3-x2))/((x2-x1)*(y3-y2)-(y2-y1)*(x3-x2))*v1+
  ((x3-xp)*(y1-y3)-(y3-yp)*(x1-x3))/((x3-x2)*(y1-y3)-(y3-y2)*(x1-x3))*v2 +
  ((x1-xp)*(y2-y1)-(y1-yp)*(x2-x1))/((x1-x3)*(y2-y1)-(y1-y3)*(x2-x1))*v3
  
A <- matrix(c(y1,x1,y2,x2,y3,x3), ncol = 2, byrow = TRUE)
A1 <- A
Vxyz <- matrix(c(v1,v2,v3), ncol = 3, byrow=TRUE)

fit <- lm(Vxyz ~ A1)

summary(fit)

fit$coefficients

fit$coefficients[1,1] + fit$coefficients[2,1]*yp + fit$coefficients[3,1]*xp
fit$coefficients[1,2] + fit$coefficients[2,2]*yp + fit$coefficients[3,2]*xp
fit$coefficients[1,3] + fit$coefficients[2,3]*yp + fit$coefficients[3,3]*xp

