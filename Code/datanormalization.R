# Normalization Function
normalize <- function(df, mn, mx, int = TRUE, plot = TRUE) {
  # Calculate the range of the data
  range <- max(df) - min(df)
  
  # Normalize the data
  norm <- (df - min(df)) / range
  norm <- mn + (mx - mn) * norm
  
  # Convert to integer if specified
  if (int) {
    norm <- as.integer(norm)
  }
  
  # Plot original and normalized data if specified
  if (plot) {
    par(mfrow = c(1, 2))  # Set the plotting area to a 1x2 grid
    hist(df, main = "Original Data", xlab = "Values")
    hist(norm, main = "Normalized Data", xlab = "Values")
    par(mfrow = c(1, 1))  # Reset plotting area
  }
  
  return(norm)
}

# Set seed for reproducibility
set.seed(4208)

# Generate random data
df <- rnorm(50, mean = 40000, sd = 2000)

# Define normalization bounds
mn <- 10  # minimum value of normalization
mx <- 200  # maximum value of normalization

# Normalize the data and display plots
normalized_data <- normalize(df, mn, mx, int = TRUE, plot = TRUE)
