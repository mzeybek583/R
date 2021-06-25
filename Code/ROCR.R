
# Load Libraries
library(ROCR) #Roc Curve

library(raster) # Read Rasters

bys_point <- "D:/Cem_roc_veriler/roc/bys_point.tif"
bys_poly <- "D:/Cem_roc_veriler/roc/bys_polygon.tif"
miv_point <- "D:/Cem_roc_veriler/roc/miv_point.tif"
miv_poly <- "D:/Cem_roc_veriler/roc/miv_polygon.tif"
#Reference Ground truth data
reference <- "D:/Cem_roc_veriler/roc/landslide_non_landslide.tif"

raster_data <- stack(bys_point, bys_poly, miv_point, miv_poly , reference) # Merge

#raster_data

df <- as.data.frame(raster_data)

#Control NA
sapply(df, function(x)sum(is.na(x)))

# Delete NA rows
data_df_NA <- na.omit(df)
df <- data_df_NA

pred_bys_point <- prediction(df$bys_point, df$landslide_non_landslide)
perf_bys_point <- performance(pred_bys_point,"tpr","fpr")
auc_ROCR_bys_point <- performance(pred_bys_point, measure = "auc")
auc_ROCR_bys_point <- auc_ROCR_bys_point@y.values[[1]]

pred_bys_poly <- prediction(df$bys_polygon, df$landslide_non_landslide)
perf_bys_poly <- performance(pred_bys_poly,"tpr","fpr")
auc_ROCR_bys_poly <- performance(pred_bys_poly, measure = "auc")
auc_ROCR_bys_poly <- auc_ROCR_bys_poly@y.values[[1]]

pred_miv_point <- prediction(df$miv_point, df$landslide_non_landslide)
perf_miv_point <- performance(pred_miv_point,"tpr","fpr")
auc_ROCR_miv_point <- performance(pred_miv_point, measure = "auc")
auc_ROCR_miv_point <- auc_ROCR_miv_point@y.values[[1]]

pred_miv_poly <- prediction(df$miv_polygon, df$landslide_non_landslide)
perf_miv_poly <- performance(pred_miv_poly,"tpr","fpr")
auc_ROCR_miv_poly <- performance(pred_miv_poly, measure = "auc")
auc_ROCR_miv_poly <- auc_ROCR_miv_poly@y.values[[1]]

plot(perf_bys_point,colorize=FALSE)
plot(perf_bys_poly, add=TRUE, col="red",print.auc=FALSE)
plot(perf_miv_point, add=TRUE, col="green",print.auc=FALSE)
plot(perf_miv_poly, add=TRUE, col="blue",print.auc=FALSE)

legend("bottomright", legend=c(paste("Bys point:", format(round(auc_ROCR_bys_point,3),nsmall=3)), 
                               paste("Bys poly:", format(round(auc_ROCR_bys_poly,3), nsmall=3)), 
                               paste("MIV point:",format(round(auc_ROCR_miv_point,3), nsmall=3)),
                               paste("MIV poly:",format(round(auc_ROCR_miv_poly,3), nsmall=3))),
       col=c(par("fg"), "red", "green", "blue"), lwd=2)


# Open a pdf file
pdf("rrocplot.pdf") 
# 2. Create a plot
plot(perf_bys_point,colorize=FALSE)
plot(perf_bys_poly, add=TRUE, col="red",print.auc=FALSE)
plot(perf_miv_point, add=TRUE, col="green",print.auc=FALSE)
plot(perf_miv_poly, add=TRUE, col="blue",print.auc=FALSE)

legend("bottomright", legend=c(paste("Bys point:", format(round(auc_ROCR_bys_point,3),nsmall=3)), 
                               paste("Bys poly:", format(round(auc_ROCR_bys_poly,3), nsmall=3)), 
                               paste("MIV point:",format(round(auc_ROCR_miv_point,3), nsmall=3)),
                               paste("MIV poly:",format(round(auc_ROCR_miv_poly,3), nsmall=3))),
       col=c(par("fg"), "red", "green", "blue"), lwd=2)
# Close the pdf file
dev.off() 

# Open a png file
png("rrocplot.png") 
# 2. Create a plot
plot(perf_bys_point,colorize=FALSE)
plot(perf_bys_poly, add=TRUE, col="red",print.auc=FALSE)
plot(perf_miv_point, add=TRUE, col="green",print.auc=FALSE)
plot(perf_miv_poly, add=TRUE, col="blue",print.auc=FALSE)

legend("bottomright", legend=c(paste("Bys point:", format(round(auc_ROCR_bys_point,3),nsmall=3)), 
                               paste("Bys poly:", format(round(auc_ROCR_bys_poly,3), nsmall=3)), 
                               paste("MIV point:",format(round(auc_ROCR_miv_point,3), nsmall=3)),
                               paste("MIV poly:",format(round(auc_ROCR_miv_poly,3), nsmall=3))),
       col=c(par("fg"), "red", "green", "blue"), lwd=2)
# Close the pdf file
dev.off() 
