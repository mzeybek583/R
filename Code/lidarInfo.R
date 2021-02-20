

## LiDAR Data info


# Load Libs ---------------------------------------------------------------

library(lidR)

getwd()

info <- data.frame(matrix(ncol = 4, nrow = 0, 
                             dimnames = list(NULL, c("Plot ID", "Plot Area", "Plot Density", "Point Records"))))
n <- 10
options(width = 80)

for (i in 1:n) {
  extra <- nchar('||100%')
  width <- options()$width
  step <- round(i / n * (width - extra))
  text <- sprintf('|%s%s|% 3s%%', strrep('=', step),
                  strrep(' ', width - step - extra), round(i / n * 100))
  cat(text)
  
  kk <- paste("../Plot",i, sep = "")
  k <- paste(kk, "step1","Filtered_test_point.las" ,sep = "/")
  las <- readLAS(k)
  #lascheck(las)
  head <- las@header
  pts <- head@PHB$`Number of point records`
  info[i,1] <- kk
  info[i,2] <- round(lidR::area(las), digits = 2)
  info[i,3] <- round(lidR::density(las), digits = 2)
  info[i,4] <- round(pts, digits = 2)
 
  cat(if (i == n) '\n' else '\014')
}
write.table(file = "las_infos.csv", info, quote = FALSE, sep = ";",row.names = F) 



