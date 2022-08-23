


# CloudCompare  -----------------------------------------------------------


# Import libs -------------------------------------------------------------


library(cloudcompare)
library(lidR)

las <- readLAS("cloudcompare_test.las") 

CC_dir <- set_CCdir(CCdir = "C:/Program Files/CloudCompare/CloudCompare.exe")

pc_files <- list.files("path_to_las", 
                       pattern='test.las$',full.names=T)

if (!dir.exists("CC_Result")) {dir.create("CC_Result")}

setwd("CC_REsult/")

# Compute Features --------------------------------------------------------

r <- 0.05 # Radius

CC(feature(file = pc_files, 
           type = "VERTICALITY",
           radius = r,
           output_dir = "verticality",
           global_shift = F,
           global_shift_type = "AUTO",
           filter_sf = F,
           c_export_fmt = "LAS",
           c_ext = "las",
           silent = T,
           no_timestamp = T))

CC(feature(file = pc_files, 
           type = "PLANARITY",
           radius = r,
           output_dir = "planarity",
           global_shift = F,
           global_shift_type = "AUTO",
           filter_sf = F,
           c_export_fmt = "LAS",
           c_ext = "las",
           silent = T,
           no_timestamp = T))
CC(feature(file = pc_files, 
           type = "EIGENTROPY",
           radius = r,
           output_dir = "eigentropy",
           global_shift = F,
           global_shift_type = "AUTO",
           filter_sf = F,
           c_export_fmt = "LAS",
           c_ext = "las",
           silent = T,
           no_timestamp = T))


vert <- readLAS("verticality.las")
eigen <- readLAS("eigentropy.las")
planar <- readLAS("planarity.las")

las <- add_lasattribute(las, vert$`Verticality (0.05)`, "Verticality (0.05)", "Verticality")
las <- add_lasattribute(las, eigen$`Eigenentropy (0.05)`, "Eigenentropy (0.05)", "Eigentropy")
las <- add_lasattribute(las, planar$`Planarity (0.05)`, "Planarity (0.05)", "Planarity")

writeLAS(las, "feature_computed_las.las")

file.remove("eigentropy.las",
            "planarity.las",
            "verticality.las")
CC("CloudCompare -O feature_computed_las.las -SILENT")
