library(ggplot2)

# Create a data frame with tasks and time spent
task_data <- data.frame(
  Task = c("Searching and Dataset", "creating visualization (chart)"),
  Hours = c(2, 0.5),
  Week = c("Week 1","Week 1")
)

# Generate a bar chart
ggplot(task_data, aes(x=Task, y=Hours)) +
  geom_bar(stat="identity", fill="green") +
  ggtitle("Time Spent on Tasks This Week") +
  xlab("Tasks") +
  ylab("Hours Spent") +
  facet_wrap(~Week) +
  theme_minimal()