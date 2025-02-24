getwd()
library(readxl)
setwd('C:/Users/olani/OneDrive - Western Michigan University/Documents/University of Maine/Presentation')
data <- read_excel('Book1.xlsx')
View(data)

library(dplyr)
library(ggplot2)

# 1. Overall trend of cancer incidence over time
overall_trend <- data %>%
  filter(County == "All Counties" & `Cancer Type` == "All Cancers" & Sex == "Male and Female") %>%
  select(Year, `Age-Adjusted Incidence Rate per 100,000`)

# Plot the trend
ggplot(overall_trend, aes(x = Year, y = `Age-Adjusted Incidence Rate per 100,000`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Overall Cancer Incidence Rate in Maine (1999-2020)",
       x = "Year",
       y = "Age-Adjusted Incidence Rate per 100,000")



library(tidyr)

# Read the Excel file
df <- readxl::read_excel("Book1.xlsx")

# Clean the data
df_cleaned <- data %>%
  mutate(
    Sex = case_when(
      Sex == "Male and Female" ~ "Both",
      TRUE ~ Sex
    ),
    Year = as.numeric(substr(Year, 1, 4))  # Extract first 4 digits for year range
  ) %>%
  filter(!is.na(Year))  # Remove any rows with NA in Year

# Calculate average incidence rate by year
df_yearly <- df_cleaned %>%
  group_by(Year) %>%
  summarize(Avg_Incidence_Rate = mean(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE))

# Create a more beautiful plot
ggplot(df_yearly, aes(x = Year, y = Avg_Incidence_Rate)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 3) +
  theme_minimal() +
  labs(
    title = "Average Age-Adjusted Cancer Incidence Rate in Maine (1999-2020)",
    x = "Year",
    y = "Incidence Rate per 100,000",
    caption = "Source: Maine Cancer Registry"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) +
  scale_x_continuous(breaks = seq(min(df_yearly$Year), max(df_yearly$Year), by = 2))

# Display summary of cleaned data
summary(df_cleaned)

# Show unique values in Sex column
unique(df_cleaned$Sex)

# Show distribution of Cancer Types
df_cleaned %>%
  group_by(`Cancer Type`) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10)


##################################################################################

# Comparative Analysis: Counties
county_comparison <- df_cleaned %>%
  filter(`Cancer Type` == "All Cancers", Sex == "Both") %>%
  group_by(County, Year) %>%
  summarise(Incidence_Rate = mean(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE)) %>%
  arrange(desc(Incidence_Rate)) %>%
  ungroup()

# Plot county comparison
ggplot(county_comparison, aes(x = Year, y = Incidence_Rate, color = County)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cancer Incidence Rates by County Over Time",
       x = "Year", y = "Age-Adjusted Incidence Rate per 100,000") +
  theme(legend.position = "right")

# Comparative Analysis: Gender
gender_comparison <- df_cleaned %>%
  filter(`Cancer Type` == "All Cancers", County == "All Counties", Sex != "Both") %>%
  group_by(Sex, Year) %>%
  summarise(Incidence_Rate = mean(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE)) %>%
  ungroup()

# Plot gender comparison
ggplot(gender_comparison, aes(x = Year, y = Incidence_Rate, color = Sex)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cancer Incidence Rates by Sex Over Time",
       x = "Year", y = "Age-Adjusted Incidence Rate per 100,000") +
  theme(legend.position = "right")

# Summary statistics
summary_stats <- df_cleaned %>%
  filter(`Cancer Type` == "All Cancers", County == "All Counties") %>%
  group_by(Sex) %>%
  summarise(
    Avg_Incidence_Rate = mean(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE),
    Min_Incidence_Rate = min(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE),
    Max_Incidence_Rate = max(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE)
  )

print(summary_stats)
#########################################################################


library(ggplot2)
library(dplyr)

# Prepare data for box plot
county_box_data <- df_cleaned %>%
  filter(`Cancer Type` == "All Cancers", Sex == "Both") %>%
  group_by(County, Year) %>%
  summarise(Incidence_Rate = mean(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE)) %>%
  ungroup()

# Create box plot
box_plot <- ggplot(county_box_data, aes(x = reorder(County, Incidence_Rate, FUN = median), y = Incidence_Rate)) +
  geom_boxplot(aes(fill = County)) +
  coord_flip() +  # Flip coordinates for horizontal box plot
  labs(title = "Distribution of Cancer Incidence Rates by County",
       x = "County",
       y = "Age-Adjusted Incidence Rate per 100,000") +
  theme_minimal() +
  theme(legend.position = "none",  # Remove legend
        axis.text.y = element_text(size = 8))  # Adjust text size for readability

# Print the plot
print(box_plot)

# Calculate and print summary statistics
summary_stats <- county_box_data %>%
  group_by(County) %>%
  summarise(
    Median = median(Incidence_Rate),
    Mean = mean(Incidence_Rate),
    Min = min(Incidence_Rate),
    Max = max(Incidence_Rate)
  ) %>%
  arrange(desc(Median))

print(summary_stats)

##################################################################
summary_stats <- df_cleaned %>%
  group_by(`Cancer Type`, Sex) %>%
  summarise(
    Mean_Incidence = mean(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE),
    Median_Incidence = median(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE),
    Min_Incidence = min(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE),
    Max_Incidence = max(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Incidence))

print(head(summary_stats, 10))

# 1. Distribution of Incidence Rates
p1 <- ggplot(df_cleaned, aes(x = `Age-Adjusted Incidence Rate per 100,000`)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Incidence Rates", x = "Incidence Rate", y = "Count") +
  theme_minimal()

# 2. Top 10 Cancer Types by Mean Incidence Rate
top_10_cancers <- summary_stats %>%
  filter(Sex == "Both") %>%
  top_n(10, Mean_Incidence)

p2 <- ggplot(top_10_cancers, aes(x = reorder(`Cancer Type`, Mean_Incidence), y = Mean_Incidence)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Top 10 Cancer Types by Mean Incidence Rate", x = "Cancer Type", y = "Mean Incidence Rate") +
  theme_minimal()

# 3. Incidence Rates by Sex
p3 <- ggplot(df_cleaned, aes(x = Sex, y = `Age-Adjusted Incidence Rate per 100,000`, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Incidence Rates by Sex", x = "Sex", y = "Incidence Rate") +
  theme_minimal()

# 4. Trend of Incidence Rates Over Time
trend_data <- df_cleaned %>%
  group_by(Year) %>%
  summarise(Mean_Incidence = mean(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE))

p4 <- ggplot(trend_data, aes(x = Year, y = Mean_Incidence)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Mean Incidence Rates Over Time", x = "Year", y = "Mean Incidence Rate") +
  theme_minimal()

# Combine plots
grid.arrange(p1, p3, ncol = 1)

# Additional summary statistics
print(summary(df_cleaned$`Age-Adjusted Incidence Rate per 100,000`))
print(table(df_cleaned$`Cancer Type`))
print(table(df_cleaned$Sex))
print(table(df_cleaned$County))

##############################################################
# Create the boxplot for incidence rates by sex
ggplot(df_cleaned, aes(x = Sex, y = `Age-Adjusted Incidence Rate per 100,000`, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Incidence Rates by Sex", x = "Sex", y = "Age-Adjusted Incidence Rate per 100,000") +
  theme_minimal()

# Calculate summary statistics
summary_stats <- df_cleaned %>%
  group_by(Sex) %>%
  summarise(
    Mean = mean(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE),
    Median = median(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE),
    Min = min(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE),
    Max = max(`Age-Adjusted Incidence Rate per 100,000`, na.rm = TRUE)
  )

print(summary_stats)

#################################################
ci_analysis <- df_cleaned %>%
  select(`Age-Adjusted Incidence Rate per 100,000`,
         `Lower 95% Confidence Interval`,
         `Upper 95% Confidence Interval`) %>%
  mutate(
    CI_Range = `Upper 95% Confidence Interval` - `Lower 95% Confidence Interval`,
    CI_Midpoint = (`Lower 95% Confidence Interval` + `Upper 95% Confidence Interval`) / 2
  )

# Display summary statistics for confidence intervals
summary_ci <- ci_analysis %>%
  summarise(
    Mean_CI_Range = mean(CI_Range, na.rm = TRUE),
    Median_CI_Range = median(CI_Range, na.rm = TRUE),
    Min_CI_Range = min(CI_Range, na.rm = TRUE),
    Max_CI_Range = max(CI_Range, na.rm = TRUE),
    Mean_CI_Midpoint = mean(CI_Midpoint, na.rm = TRUE),
    Median_CI_Midpoint = median(CI_Midpoint, na.rm = TRUE)
  )

# Print summary statistics
print(summary_ci)