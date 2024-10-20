if (!require("zoo")) install.packages("zoo", dependencies = TRUE)
library(zoo)

# Load the FTSE index data
ftseIndex <- EuStockMarkets[, 4]

# Plot the original FTSE index data
plot(ftseIndex, col = "gray", main = "FTSE Index with Rolling Means", ylab = "Index Value", xlab = "Time")

# Calculate a 30-day rolling mean
smoothIndex <- rollmean(x = ftseIndex, k = 30, fill = NA)

# Add the rolling mean line to the plot
lines(smoothIndex, col = "red")

# Introduce NA values to the FTSE index data
ftseIndex[c(40, 90, 300)] <- NA

# Calculate a 90-day rolling mean with NA handling
smoothIndex2 <- rollapply(data = ftseIndex, width = 90, FUN = mean, na.rm = TRUE, fill = NA)

# Add the second rolling mean line to the plot
lines(smoothIndex2, col = "green")

# Add a legend to distinguish the lines
legend("topright", legend = c("Original FTSE Index", "30-day Rolling Mean", "90-day Rolling Mean with NA Handling"),
       col = c("gray", "red", "green"), lty = 1)
