# Load required libraries
library(ggplot2)
library(dplyr)  # For data manipulation

# Define the data
data <- data.frame(
  Period = paste("Period", 1:12),
  Start = as.Date(c("2022-03-01", "2022-03-01", "2022-03-01", "2022-04-01", "2022-04-01", 
                    "2022-04-01", "2022-05-01", "2022-05-01", "2022-05-01", "2021-11-01", 
                    "2021-11-01", "2021-11-01")),
  End = as.Date(c("2022-06-30", "2022-09-30", "2023-03-31", "2022-07-31", "2022-10-31", 
                  "2023-04-30", "2022-08-31", "2022-11-30", "2023-05-31", "2022-03-31", 
                  "2022-06-30", "2022-11-30"))
)

# Sort the data by the Start date
#data <- data %>% arrange(Period)
data$Period <- factor(data$Period, levels = unique(data$Period))

# Create the Gantt chart
ggplot(data, aes(y = Period, x = Start, xend = End)) +
  geom_segment(aes(yend = Period), size = 3, color = "blue") +
  labs(
    title = "Time Periods for Kucukmuhsine Area",
    x = "Date",
    y = "Periods"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )
