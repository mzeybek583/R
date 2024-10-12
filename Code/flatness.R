# Load required packages
if (!require("lidR")) install.packages("lidR", dependencies = TRUE)
if (!require("MASS")) install.packages("MASS", dependencies = TRUE)

library(lidR)
library(MASS)  # For robust regression

# Step 1: Load the LAS file
las_file <- "../export.las"  # Path to the input LAS file
las <- readLAS(las_file)

# Check if the LAS file was loaded successfully
if (is.null(las)) stop("Failed to load the LAS file. Check the file path and format.")

# Step 2: Modify header information (if necessary)
header <- las@header

# Adjust X, Y, and Z scale factors (e.g., 0.001 for precision)
header@PHB[["X scale factor"]] <- 0.001
header@PHB[["Y scale factor"]] <- 0.001
header@PHB[["Z scale factor"]] <- 0.001

# Apply the corrected header to the LAS object
las@header <- header

# Step 3: Extract point coordinates (X, Y, Z) from the LAS data
coords <- as.data.frame(las@data[, c("X", "Y", "Z")])

# Step 4: Perform robust regression to fit a plane
robust_fit <- rlm(Z ~ X + Y, data = coords)  # Robust regression model

# Retrieve the coefficients of the plane equation
coef <- coefficients(robust_fit)
a <- coef["X"]
b <- coef["Y"]
c <- -1  # Z coefficient is fixed to -1
d <- coef["(Intercept)"]

# Step 5: Compute the unit normal vector of the plane
normal_vector <- c(a, b, c)  # Plane normal vector
normal_length <- sqrt(sum(normal_vector^2))  # Length of the normal vector
unit_normal <- normal_vector / normal_length  # Unit normal vector

# Step 6: Calculate signed deviations for each point
# Deviation formula: (a * x + b * y + c * z + d) / ||normal||
coords$Deviation <- (a * coords$X + b * coords$Y + c * coords$Z + d) / normal_length * -1

# Step 7: Add the Deviation attribute to the LAS data
las <- add_lasattribute(las, coords$Deviation, "Deviation", 
                        desc = "Signed deviation from the plane")

# Step 8: Export the updated LAS file
output_file <- "R_output_file.las"  # Path to the output LAS file
writeLAS(las, output_file)

# Completion message
cat("The updated LAS file was successfully saved as", output_file, "\n")
