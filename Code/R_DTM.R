
## Generate DTM

# Ground Nonground


# Load Library ------------------------------------------------------------

library(lidR)
library(raster)

time <- proc.time()

#Load classified PCL
dd <- readLAS("../R/per2_agi_auto_ALL_feat.las")

print(dd)
# 
# mycsf <- csf(TRUE, 1, 1, time_step = 1)
# las <- classify_ground(dd, mycsf)

#plot(las, color = "Classification")

# Write to file Classification --------------------------------------------

#writeLAS(las = las, file = "per2_agi_ALL_feat_GNG.las")

dtm <- grid_terrain(dd, 1, tin(), use_class = 2L)

plot(dtm)

rf <- writeRaster(dtm, filename="R_DTM.tif", format="GTiff", overwrite=TRUE)

#las <- normalize_height(las, dtm)

#writeLAS(las = las,"per2_agi_free_normalized.las")

proc.time() - time
