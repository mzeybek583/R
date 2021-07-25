

## Statistical testing of PCL

library(lidR)
library(ggpubr)
library(dplyr)

set.seed(4208)

n.size <-1000
threshold <- 0.50

## Agisoft 35
agi35 <- readLAS(files = "results/agi35-M3C2.las")
res.agi <- agi35@data$`M3C2 distance`
#res.agi <- na.omit(res.agi)

agi35.15 <- res.agi[which(res.agi > -1*threshold & res.agi < threshold)]
sd(agi35.15); mean(agi35.15); median(agi35.15)

ind <- floor(runif(n.size, min=0, max=length(res.agi)))

ind.agi35.15 <- agi35.15[ind]
shapiro.test(ind.agi35.15)
hist(ind.agi35.15, main="Agisoft35m", xlab="Deviations (m)")
ggqqplot(ind.agi35.15,title="Agisoft35m")

data <- data.frame(group="Agisoft35m", Residuals=ind.agi35.15)

## Pix4D 35
pix4d35 <- readLAS(files = "results/pix4d35-M3C2.las")
res.pix4d35 <- pix4d35@data$`M3C2 distance`
#res.pix4d35 <- na.omit(res.pix4d35)

pix4d35.15 <- res.pix4d35[which(res.pix4d35 > -1*threshold & res.pix4d35 < threshold)]
sd(pix4d35.15); mean(pix4d35.15); median(pix4d35.15)

ind <- floor(runif(n.size, min=0, max=length(res.pix4d35)))

ind.pix4d35.15 <- pix4d35.15[ind]

shapiro.test(ind.pix4d35.15)
hist(ind.pix4d35.15, main="Pix4D35m", xlab="Deviations (m)")
ggqqplot(ind.pix4d35.15,title="Pix4D35m")

add <- data.frame(group="Pix4D35m", Residuals=ind.pix4d35.15)
data <- rbind(data,add)

##### Compare 35 m ####
group_by(data, group) %>%
  summarise(
    count = n(),
    median = median(Residuals, na.rm = TRUE),
    IQR = IQR(Residuals, na.rm = TRUE)
  )

ggboxplot(data, x = "group", y = "Residuals", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Residuals", xlab = "Groups")

wil.test <- wilcox.test(Residuals ~ group, data = data,
                   exact = FALSE)

var.test(ind.agi35.15,ind.pix4d35.15)

wil.test
wil.test$p.value
