
# ==========================================
# SCREEN TIME DATA ANALYSIS
# ==========================================

# 1. Load packages
library(readr)
library(dplyr)
library(ggplot2)

# 2. Load dataset
screensandmentalhealth <- read_csv(
  "screensandmentalhealth.csv",
  show_col_types = FALSE
)

# 3. Examine dataset
View(screensandmentalhealth)

print(dim(screensandmentalhealth))
print(names(screensandmentalhealth))
summary(screensandmentalhealth)

# 4. Check for missing values
missing_values <- colSums(is.na(screensandmentalhealth))
print(missing_values)

# 5. Create graphs folder
if (!dir.exists("graphs")) {
  dir.create("graphs")
}

# ==========================================
# GRAPH 1: DAILY SCREEN TIME DISTRIBUTION
# ==========================================

graph1 <- ggplot(
  screensandmentalhealth,
  aes(x = daily_screen_time_hours)
) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Daily Screen Time Distribution",
    x = "Daily Screen Time (Hours)",
    y = "Number of People"
  ) +
  theme_minimal()

print(graph1)

ggsave("graphs/graph1_screen_time.png",
       graph1, width = 8, height = 5)

# ==========================================
# GRAPH 2: SCREEN TIME BY GENDER
# ==========================================

graph2 <- ggplot(
  screensandmentalhealth,
  aes(x = gender, y = daily_screen_time_hours, fill = gender)
) +
  geom_boxplot() +
  labs(
    title = "Daily Screen Time by Gender",
    x = "Gender",
    y = "Daily Screen Time (Hours)"
  ) +
  theme_minimal()

print(graph2)

ggsave("graphs/graph2_gender.png",
       graph2, width = 8, height = 5)

# ==========================================
# GRAPH 3: SCREEN TIME BY AGE
# ==========================================

graph3 <- ggplot(
  screensandmentalhealth,
  aes(x = age, y = daily_screen_time_hours)
) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Age and Screen Time",
    x = "Age (Years)",
    y = "Daily Screen Time (Hours)"
  ) +
  theme_minimal()

print(graph3)

ggsave("graphs/graph3_age.png",
       graph3, width = 8, height = 5)

# ==========================================
# GRAPH 4: SCREEN TIME BY DEVICE
# ==========================================

device_data <- screensandmentalhealth %>%
  summarise(
    Phone = mean(phone_usage_hours, na.rm = TRUE),
    Laptop = mean(laptop_usage_hours, na.rm = TRUE),
    Tablet = mean(tablet_usage_hours, na.rm = TRUE),
    Television = mean(tv_usage_hours, na.rm = TRUE),
    Social_Media = mean(social_media_hours, na.rm = TRUE),
    Work_Related = mean(work_related_hours, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Device_or_Activity",
    values_to = "Average_Hours"
  )

graph4 <- ggplot(
  device_data,
  aes(x = reorder(Device_or_Activity, Average_Hours),
      y = Average_Hours)
) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(
    title = "Average Daily Hours by Device or Activity",
    x = "Device or Activity",
    y = "Average Hours"
  ) +
  theme_minimal()

print(graph4)

ggsave("graphs/graph4_device_usage.png",
       graph4, width = 8, height = 5)

# ==========================================
# GRAPH 5: SOCIAL MEDIA AND TOTAL SCREEN TIME
# ==========================================

graph5 <- ggplot(
  screensandmentalhealth,
  aes(
    x = daily_screen_time_hours,
    y = social_media_hours
  )
) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Social Media Use and Daily Screen Time",
    x = "Daily Screen Time (Hours)",
    y = "Social Media Hours"
  ) +
  theme_minimal()

print(graph5)

ggsave("graphs/graph5_social_media.png",
       graph5, width = 8, height = 5)

# ==========================================
# 6. SUMMARY STATISTICS
# ==========================================

print(
  screensandmentalhealth %>%
    summarise(
      Average_Age = mean(age, na.rm = TRUE),
      Average_Screen_Time =
        mean(daily_screen_time_hours, na.rm = TRUE),
      Average_Phone_Use =
        mean(phone_usage_hours, na.rm = TRUE),
      Average_Social_Media =
        mean(social_media_hours, na.rm = TRUE)
    )
)

# ==========================================
# 7. SAVE SUMMARY DATA
# ==========================================

write_csv(device_data, "device_usage_summary.csv")

cat("Analysis complete! Check the graphs folder.\n")
