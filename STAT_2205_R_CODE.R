# Setting home directory
getwd()
list.files()
setwd("C:/Users/Eva/Downloads/BRUR_2026(STAT-2205)/R_PROGRAMMING")

## Problem with source
.libPaths()
.libPaths("C:/Users/Eva/Downloads/BRUR_2026(STAT-2205)/R_PROGRAMMING")

# file.choose() function
d <- read.csv(file.choose(), header = T)
d

# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Q1 Analysis
summary(sample_df$coach_price)
sd(sample_df$coach_price)


# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Q2 Analysis
install.packages("tidyverse")
library(tidyverse)
eight_hr <- sample_df %>% filter(hours == 8)
cat("Number of 8-hour flights:", nrow(eight_hr), "\n")
summary(eight_hr$coach_price)
sd(eight_hr$coach_price)


# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Q3 Analysis
cat("=== Flight Delay Distribution ===\n")
print(summary(sample_df$delay))
cat("Standard Deviation:", round(sd(sample_df$delay), 2), "\n\n")

cat("Delays > 30 minutes:", sum(sample_df$delay > 30), 
    paste0("(", round(mean(sample_df$delay > 30)*100, 2), "%)"), "\n")
cat("Delays > 60 minutes:", sum(sample_df$delay > 60), "\n")

cat("\nMost Common Delays:\n")
print(head(sort(table(sample_df$delay), decreasing = TRUE), 10))



# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Q4 Analysis
numeric_cols <- c("miles", "passengers", "delay", "hours", "coach_price")
cor_matrix <- cor(sample_df[numeric_cols])
print(round(cor_matrix[,"coach_price"], 4))




# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Q5 Analysis
correlation <- cor(sample_df$coach_price, sample_df$firstclass_price)
cat("Correlation between Coach and First Class Price:", round(correlation, 4), "\n")

cat("\nCoach Price Mean    :", round(mean(sample_df$coach_price), 2), "\n")
cat("First Class Price Mean:", round(mean(sample_df$firstclass_price), 2), "\n")



# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Q6 Analysis
features <- c("inflight_meal", "inflight_entertainment", "inflight_wifi")

for (f in features) {
  
  cat("\n=== ", gsub("_", " ", f), " ===\n")
  
  stats <- sample_df %>% 
    group_by(.data[[f]]) %>% 
    summarise(
      count = n(),
      mean_price = mean(coach_price),
      median_price = median(coach_price),
      .groups = "drop"
    )
  
  yes_mean <- stats %>% filter(.data[[f]] == "Yes") %>% pull(mean_price)
  no_mean  <- stats %>% filter(.data[[f]] == "No") %>% pull(mean_price)
  
  premium <- yes_mean - no_mean
  
  stats_print <- stats %>%
    mutate(across(where(is.numeric), ~sprintf("%.2f", .x)))
  
  print(stats_print)
  
  cat("Price Premium (Yes vs No): $", sprintf("%.2f", premium), "\n")
}
# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

cat("Overall Average Passengers:", round(mean(sample_df$passengers), 2), "\n\n")

cat("Average Passengers by Flight Duration:\n")
sample_df %>% 
  group_by(hours) %>% 
  summarise(avg_passengers = mean(passengers)) %>% 
  mutate(avg_passengers = sprintf("%.2f", avg_passengers)) %>%
  print()


# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

cat("=== Coach Price: Weekend vs Weekday ===\n")
cat("Weekend Coach Mean    :", round(mean(sample_df[sample_df$weekend == "Yes", ]$coach_price), 2), "\n")
cat("Weekday Coach Mean    :", round(mean(sample_df[sample_df$weekend == "No",  ]$coach_price), 2), "\n\n")

cat("=== First Class Price: Weekend vs Weekday ===\n")
cat("Weekend First Class Mean:", round(mean(sample_df[sample_df$weekend == "Yes", ]$firstclass_price), 2), "\n")
cat("Weekday First Class Mean:", round(mean(sample_df[sample_df$weekend == "No",  ]$firstclass_price), 2), "\n")



# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Q9 Analysis
cat("=== Coach Price: Redeye vs Non-Redeye by Day of Week ===\n")
sample_df %>% 
  group_by(day_of_week, redeye) %>% 
  summarise(mean_coach_price = mean(coach_price), .groups = "drop") %>% 
  mutate(mean_coach_price = sprintf("%.2f", mean_coach_price)) %>% 
  arrange(day_of_week) %>% 
  print()


sample_df %>% 
  group_by(day_of_week) %>% 
  summarise(mean_coach_price = mean(coach_price)) %>% 
  mutate(mean_coach_price = sprintf("%.2f", mean_coach_price)) %>%  
  arrange(day_of_week) %>% 
  print()

(10)
# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Summary Statistics
numeric_vars <- c('miles', 'passengers', 'delay', 'hours', 
                  'coach_price', 'firstclass_price')

cat("=== SUMMARY STATISTICS (Mean, Median, SD) ===\n\n")
print(summary(sample_df[numeric_vars]))

cat("\n=== Standard Deviation ===\n")
print(round(sapply(sample_df[numeric_vars], sd), 2))


library(ggplot2)
library(dplyr)
library(tidyr)

# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Select numeric columns
numeric_cols <- c('miles', 'passengers', 'delay', 'hours', 
                  'coach_price', 'firstclass_price')

# Reshape and create faceted histograms
sample_df %>%
  select(all_of(numeric_cols)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  geom_density(color = "red", linewidth = 0.8) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  labs(title = "Histograms of Numeric Variables",
       x = "Value", 
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


library(ggplot2)
library(dplyr)

# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Create multiple boxplots
p1 <- ggplot(sample_df, aes(x = weekend, y = coach_price, fill = weekend)) +
  geom_boxplot() +
  labs(title = "Coach Price: Weekend vs Weekday", x = "Weekend", y = "Coach Price ($)") +
  theme_minimal()

p2 <- ggplot(sample_df, aes(x = inflight_entertainment, y = coach_price, fill = inflight_entertainment)) +
  geom_boxplot() +
  labs(title = "Coach Price by In-flight Entertainment", 
       x = "In-flight Entertainment", y = "Coach Price ($)") +
  theme_minimal()

p3 <- ggplot(sample_df, aes(x = redeye, y = coach_price, fill = redeye)) +
  geom_boxplot() +
  labs(title = "Coach Price: Redeye vs Non-Redeye", x = "Redeye", y = "Coach Price ($)") +
  theme_minimal()

p4 <- ggplot(sample_df, aes(x = factor(hours), y = coach_price, fill = factor(hours))) +
  geom_boxplot() +
  labs(title = "Coach Price by Flight Duration", x = "Hours", y = "Coach Price ($)") +
  theme_minimal()

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)


library(ggplot2)
library(dplyr)

# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Bar Charts
p1 <- sample_df %>% 
  group_by(weekend) %>% 
  summarise(avg_price = mean(coach_price)) %>% 
  ggplot(aes(x = weekend, y = avg_price, fill = weekend)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Coach Price: Weekend vs Weekday", 
       x = "Weekend", y = "Average Price ($)") +
  theme_minimal()

p2 <- sample_df %>% 
  group_by(inflight_entertainment) %>% 
  summarise(avg_price = mean(coach_price)) %>% 
  ggplot(aes(x = inflight_entertainment, y = avg_price, fill = inflight_entertainment)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Coach Price by In-flight Entertainment", 
       x = "Entertainment", y = "Average Price ($)") +
  theme_minimal()

p3 <- sample_df %>% 
  group_by(redeye) %>% 
  summarise(avg_price = mean(coach_price)) %>% 
  ggplot(aes(x = redeye, y = avg_price, fill = redeye)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Coach Price: Redeye vs Non-Redeye", 
       x = "Redeye", y = "Average Price ($)") +
  theme_minimal()

p4 <- sample_df %>% 
  group_by(inflight_wifi) %>% 
  summarise(avg_price = mean(coach_price)) %>% 
  ggplot(aes(x = inflight_wifi, y = avg_price, fill = inflight_wifi)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Coach Price by In-flight WiFi", 
       x = "WiFi", y = "Average Price ($)") +
  theme_minimal()

# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)



# ====================== LOAD DATA & SAMPLE ======================
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

cat("=== INDEPENDENT T-TESTS ===\n\n")

# 1. T-Test: Weekend vs Weekday
cat("1. Coach Price: Weekend vs Weekday\n")
t1 <- t.test(coach_price ~ weekend, data = sample_df, var.equal = FALSE)
print(t1)

cat("\nMean Comparison:\n")
print(sample_df %>% 
        group_by(weekend) %>% 
        summarise(Mean = mean(coach_price), 
                  Median = median(coach_price),
                  Count = n()))

# 2. T-Test: Redeye vs Non-Redeye
cat("\n\n2. Coach Price: Redeye vs Non-Redeye\n")
t2 <- t.test(coach_price ~ redeye, data = sample_df, var.equal = FALSE)
print(t2)

cat("\nMean Comparison:\n")
print(sample_df %>% 
        group_by(redeye) %>% 
        summarise(Mean = mean(coach_price), 
                  Median = median(coach_price),
                  Count = n()))

# 3. T-Test: In-flight Entertainment Yes vs No
cat("\n\n3. Coach Price: In-flight Entertainment\n")
t3 <- t.test(coach_price ~ inflight_entertainment, data = sample_df, var.equal = FALSE)
print(t3)

cat("\nMean Comparison:\n")
print(sample_df %>% 
        group_by(inflight_entertainment) %>% 
        summarise(Mean = mean(coach_price), 
                  Median = median(coach_price),
                  Count = n()))

# 4. T-Test: In-flight WiFi Yes vs No
cat("\n\n4. Coach Price: In-flight WiFi\n")
t4 <- t.test(coach_price ~ inflight_wifi, data = sample_df, var.equal = FALSE)
print(t4)



# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Z-Test Function
z_test <- function(x, y) {
  mean1 <- mean(x)
  mean2 <- mean(y)
  sd1 <- sd(x)
  sd2 <- sd(y)
  n1 <- length(x)
  n2 <- length(y)
  
  z <- (mean1 - mean2) / sqrt((sd1**2/n1) + (sd2**2/n2))
  p_value <- 2 * pnorm(-abs(z))
  
  cat("Z-Statistic :", round(z, 4), "\n")
  cat("P-Value     :", format.pval(p_value, digits = 10), "\n")
  cat("Redeye Mean :", round(mean1, 2), "\n")
  cat("Non-Redeye Mean :", round(mean2, 2), "\n")
}

# Run Z-Test
z_test(sample_df$coach_price[sample_df$redeye == "Yes"], 
       sample_df$coach_price[sample_df$redeye == "No"])


# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

cat("=== CHI-SQUARE TESTS ===\n\n")

# 1. Entertainment vs Weekend
cat("1. In-flight Entertainment vs Weekend\n")
print(chisq.test(table(sample_df$inflight_entertainment, sample_df$weekend)))

# 2. WiFi vs Weekend
cat("\n2. In-flight WiFi vs Weekend\n")
print(chisq.test(table(sample_df$inflight_wifi, sample_df$weekend)))

# 3. Redeye vs Weekend
cat("\n3. Redeye vs Weekend\n")
print(chisq.test(table(sample_df$redeye, sample_df$weekend)))

# 4. Meal vs Weekend
cat("\n4. In-flight Meal vs Weekend\n")
print(chisq.test(table(sample_df$inflight_meal, sample_df$weekend)))


# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

cat("=== PRICE DIFFERENCES BETWEEN WEEKEND AND WEEKDAY FLIGHTS ===\n\n")

# Summary Statistics
summary_stats <- sample_df %>% 
  group_by(weekend) %>% 
  summarise(
    Count = n(),
    Mean_Price = mean(coach_price),
    Median_Price = median(coach_price),
    SD = sd(coach_price)
  )

print(summary_stats)

# Price Difference
diff <- summary_stats$Mean_Price[summary_stats$weekend == "Yes"] - 
  summary_stats$Mean_Price[summary_stats$weekend == "No"]

cat("\nAbsolute Price Difference (Weekend - Weekday) :", round(diff, 2), "\n")
cat("Percentage Increase on Weekend               :", round((diff / summary_stats$Mean_Price[summary_stats$weekend == "No"] * 100), 2), "%\n")

# t-test for significance
t.test(coach_price ~ weekend, data = sample_df, var.equal = FALSE)



# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

numeric_cols <- c('miles', 'passengers', 'delay', 'hours', 
                  'coach_price', 'firstclass_price')

# Correlation Matrix
corr_matrix <- cor(sample_df[numeric_cols])

cat("=== CORRELATION MATRIX ===\n")
print(round(corr_matrix, 4))

cat("\n=== Correlation with Coach Price ===\n")
print(round(sort(corr_matrix[,"coach_price"], decreasing = TRUE), 4))




# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Linear Regression
model <- lm(coach_price ~ miles + hours + passengers + delay, data = sample_df)

# Model Summary
summary(model)

# Confidence Intervals
confint(model)




# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Linear Regression
model <- lm(coach_price ~ miles + hours + passengers + delay, data = sample_df)

# Model Summary
summary(model)

# Confidence Intervals
confint(model)




# Load data and sample
df <- read.csv("flight_data.csv")
set.seed(10063)
sample_df <- df[sample(nrow(df), 10063), ]

# Data Preparation: Convert categorical to numeric binary (1/0)
sample_df$redeye_num <- ifelse(sample_df$redeye == "Yes", 1, 0)
sample_df$weekend_num <- ifelse(sample_df$weekend == "Yes", 1, 0)

# Fit Logistic Regression Model
model <- glm(redeye_num ~ miles + hours + passengers + delay + weekend_num, 
             data = sample_df, 
             family = binomial(link = "logit"))

# Model Summary
summary(model)

# Calculate Odds Ratios and 95% Confidence Intervals
cat("\n--- ODDS RATIOS & 95% CI ---\n")
odds_ratios <- exp(coef(model))
conf_intervals <- exp(confint(model))

results_table <- cbind(Odds_Ratio = odds_ratios, conf_intervals)
print(results_table)

