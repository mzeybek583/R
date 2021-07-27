
### 3DReshaper reader #####

library(stringr)
temp = list.files(pattern="*.txt")

for (i in 1:length(temp)) {
  file3 <- temp[i]
#file3 <- "11.txt"
CylinderFit <- read.csv(file = file3, header = F, skip = 1)
class(CylinderFit)
colnames(CylinderFit) <- c("data")
CylinderFit <- str_split_fixed(CylinderFit$data, pattern = ":", 2)
CylinderFit <- as.data.frame(CylinderFit)
colnames(CylinderFit) <- c("Name", "Variable")

cy_fit <- CylinderFit[which(CylinderFit$Name == "Radius", arr.ind = TRUE),]
cy_fit$Variable<-as.numeric(as.character(cy_fit$Variable))
dia <- cy_fit$Variable*2
dia
exp.name <- tools::file_path_sans_ext(file3)
write.table(dia,paste0("result/",exp.name,"-DBH.csv",sep = ""))
}
