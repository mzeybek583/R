
## PCA

#Kaggle glass data
df <- read.csv(file = "C:/Users/LENOVO-7/Desktop/datasets_738_1370_glass.csv")

str(df)

data <- df[,-10] # Hedef kolon eksiltme
str(data)

pcr <- prcomp(x = data, scale=TRUE, center = TRUE) # PCA

summary(pcr)

pcr$rotation

x11(width=4, height=4)
biplot(pcr)

pcr.var <- pcr$sdev^2
exp.var.pcr <- pcr.var/sum(pcr.var)

x11(width=4, height=4)
plot(exp.var.pcr, type = "b")

x11(width=4, height=4)
plot(cumsum(exp.var.pcr), type = "b")
