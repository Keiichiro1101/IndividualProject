# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create a data frame with my time log data
time_log <- data.frame(
  Week = c("Week 1", "Week 1", "Week 2", "Week 3", "Week 3", "Week 4", "Week 4", "Week 5", "Week 5", "Week 6", "Week 7", "Week 7"),
  Task = c("Data Finding", "Data Visualization", "Data Cleaning", "Data Analyzing", "Documentation", "Creating WebApp", "Data Analyzing", "Creating WebApp", "Data Visualization", "Creating WebApp", "Creating WebApp", "Documentation"),
  Hours = c(2, 0.5, 2, 2, 4, 4, 2, 4, 2, 6, 6, 2)
)

# Create a bar chart
time_chart <- ggplot(time_log, aes(x = Week, y = Hours, fill = Task)) +
  geom_bar(stat = "identity") +
  labs(title = "Time Log for Individual Project",
       x = "Week",
       y = "Hours",
       fill = "Task") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Data Finding" = "blue", "Data Cleaning" = "green", "Data Analyzing" = "orange", "Documentation" = "purple", "Data Visualization" = "red", "Creating WebApp" = "brown"))

# Print the chart
print(time_chart)
