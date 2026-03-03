# DMV LAB – Students Performance Analysis

# Load required libraries
library(tidyverse)
library(ggplot2)

# Load dataset
data <- read.csv("C:/DMV LAB/students/StudentsPerformance.csv")

# View structure and summary
str(data)
summary(data)

# Check missing values
colSums(is.na(data))

# Create total score
data$total_score <- data$math.score +
  data$reading.score +
  data$writing.score

# Overall averages
colMeans(data[, c("math.score", "reading.score", "writing.score")])

# Gender-wise analysis
gender_avg <- data %>%
  group_by(gender) %>%
  summarise(
    avg_math = mean(math.score),
    avg_reading = mean(reading.score),
    avg_writing = mean(writing.score),
    avg_total = mean(total_score)
  )

print(gender_avg)

# Test preparation analysis
prep_avg <- data %>%
  group_by(test.preparation.course) %>%
  summarise(
    avg_math = mean(math.score),
    avg_reading = mean(reading.score),
    avg_writing = mean(writing.score),
    avg_total = mean(total_score)
  )

print(prep_avg)

# Parental education analysis
parent_avg <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(avg_total = mean(total_score)) %>%
  arrange(desc(avg_total))

print(parent_avg)

# Histogram of math scores
ggplot(data, aes(x = math.score)) +
  geom_histogram(bins = 20, fill = "blue") +
  theme_minimal() +
  labs(title = "Distribution of Math Scores")

# Boxplot: Gender vs Math score
ggplot(data, aes(x = gender, y = math.score, fill = gender)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Math Score by Gender")

# Boxplot: Test preparation vs Total score
ggplot(data, aes(x = test.preparation.course, y = total_score,
                 fill = test.preparation.course)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Total Score vs Test Preparation Course")

# Scatter plot: Math vs Reading
ggplot(data, aes(x = math.score, y = reading.score)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Math Score vs Reading Score")

# Correlation analysis
cor(data$math.score, data$reading.score)
cor(data$math.score, data$writing.score)
cor(data$reading.score, data$writing.score)