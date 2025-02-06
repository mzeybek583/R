## Load necessary libraries
library(tidyverse)

## Define file name
file3 <- "ref_circle.txt"

## Read and process data
CylinderFit <- read.csv(file = file3, header = FALSE, col.names = c("data")) %>%
  separate(data, into = c("Name", "Variable"), sep = ":", extra = "merge", fill = "right") %>%
  mutate(Name = str_trim(Name), Variable = str_trim(Variable))

## Extract center and radius values
cy_fit_center <- CylinderFit %>% filter(Name == "Center")
cy_fit_r <- CylinderFit %>%
  filter(Name == "Radius") %>%
  mutate(Variable = as.numeric(str_remove_all(Variable, "[a-zA-Z ]")))

## Ensure numeric conversion success
if (any(is.na(cy_fit_r$Variable))) {
  stop("Error: Radius values contain non-numeric characters.")
}

## Process center coordinates correctly and remove first column
centers <- cy_fit_center %>%
  mutate(Variable = str_squish(Variable)) %>%  # Remove extra spaces
  separate(Variable, into = c("X", "Y", "Z"), sep = "\\s+", convert = TRUE) %>%
  select(-1)  # Remove the first column

## Combine center coordinates with radius and compute additional metrics
center_rad <- centers %>%
  mutate(
    `Radius (m)` = cy_fit_r$Variable,
    `Diameter (m)` = `Radius (m)` * 2,
    `DBH (m)` = 1.30
  ) %>%
  rowid_to_column("ID")

## Write output to file
write.csv(center_rad, "ref_manu_diam_centers.txt", quote = FALSE, row.names = FALSE)

## Print success message
cat("Processed data successfully saved to 'ref_manu_diam_centers.txt'.\n")
