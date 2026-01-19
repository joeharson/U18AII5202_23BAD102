print("Santhiya Joe Harson J - 23BAD102")

library(ggplot2)
library(dplyr)

student_data <- read.csv("student_performance.csv")

student_data <- student_data %>%
  filter(!is.na(Internal_Test1), !is.na(Internal_Test2))
student_data <- student_data %>%
  mutate(Average_Marks = (Internal_Test1 + Internal_Test2) / 2)

subject_avg <- student_data %>%
  group_by(Subject) %>%
  summarise(Avg_Marks = mean(Average_Marks))
ggplot(subject_avg, aes(x = Subject, y = Avg_Marks)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Subject-wise Average Marks",
       x = "Subject",
       y = "Average Marks")

trend_data <- student_data %>%
  select(Student_ID, Subject, Internal_Test1, Internal_Test2) %>%
  tidyr::pivot_longer(
    cols = c(Internal_Test1, Internal_Test2),
    names_to = "Test",
    values_to = "Marks"
  )
ggplot(trend_data, aes(x = Test, y = Marks, group = Subject, color = Subject)) +
  geom_line() +
  labs(title = "Performance Trend Across Internal Tests",
       x = "Test",
       y = "Marks")

grade_data <- student_data %>%
  group_by(Final_Grade) %>%
  summarise(Count = n())
ggplot(grade_data, aes(x = "", y = Count, fill = Final_Grade)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Final Grade Distribution")
