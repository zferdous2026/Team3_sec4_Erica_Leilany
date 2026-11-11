
# ==========================================
# TEAM 3: CHILDREN'S SCREEN TIME AND MENTAL HEALTH
# ==========================================

# 1. Load packages
library(readr)
library(dplyr)
library(ggplot2)

# 2. Load the correct dataset
screentimementalhealth <- read_csv(
  "screentimementalhealth.csv",
  show_col_types = FALSE
)

# 3. Prepare age groups
screentimementalhealth <- screentimementalhealth %>%
  mutate(
    Age_Group = case_when(
      `Age (Years)` >= 0 & `Age (Years)` <= 5 ~ "Preschool (0-5)",
      `Age (Years)` >= 6 & `Age (Years)` <= 10 ~ "Young Children (6-10)",
      `Age (Years)` >= 11 & `Age (Years)` <= 13 ~ "Older Children (11-13)",
      `Age (Years)` >= 14 & `Age (Years)` <= 17 ~ "Adolescents (14-17)",
      TRUE ~ NA_character_
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "Preschool (0-5)",
        "Young Children (6-10)",
        "Older Children (11-13)",
        "Adolescents (14-17)"
      )
    )
  )

# 4. Create folder for graphs
if (!dir.exists("graphs")) {
  dir.create("graphs")
}

# GRAPH 1: Distribution of weekday screen time
graph1 <- ggplot(
  screentimementalhealth,
  aes(x = Screentime)
) +
  geom_bar(fill = "#B9A4D6") +
  labs(
    title = "Children's Weekday Screen Time",
    x = "Screen Time",
    y = "Number of Children"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# GRAPH 2: Mental health treatment by screen time
graph2 <- ggplot(
  screentimementalhealth,
  aes(x = Screentime, fill = `Mental Health Treatment`)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Mental Health Treatment by Screen Time",
    x = "Screen Time",
    y = "Percentage of Children",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# GRAPH 3: Treatment not received by screen time
graph3 <- ggplot(
  screentimementalhealth,
  aes(x = Screentime, fill = `Treatment Not Received`)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Mental Health Treatment Not Received",
    x = "Screen Time",
    y = "Percentage of Children",
    fill = "Treatment Not Received"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# GRAPH 4: Age distribution across screen-time categories
graph4 <- ggplot(
  screentimementalhealth,
  aes(x = Screentime, y = `Age (Years)`)
) +
  geom_boxplot(fill = "#A8D8C8") +
  labs(
    title = "Age Distribution by Screen Time",
    x = "Screen Time",
    y = "Age in Years"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# GRAPH 5: Screen time patterns by sex
graph5 <- ggplot(
  screentimementalhealth,
  aes(x = Screentime, fill = Sex)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Screen Time Patterns by Sex",
    x = "Screen Time",
    y = "Percentage of Children",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# GRAPH 6: Mental health treatment by age group
graph6 <- ggplot(
  screentimementalhealth,
  aes(x = Age_Group, fill = `Mental Health Treatment`)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Mental Health Treatment Across Age Groups",
    x = "Age Group",
    y = "Percentage of Children",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# 5. Display all six graphs
print(graph1)
print(graph2)
print(graph3)
print(graph4)
print(graph5)
print(graph6)

# 6. Save all six graphs
ggsave("graphs/graph1_screen_time.png", graph1, width = 9, height = 6)
ggsave("graphs/graph2_treatment.png", graph2, width = 9, height = 6)
ggsave("graphs/graph3_treatment_not_received.png", graph3, width = 9, height = 6)
ggsave("graphs/graph4_age.png", graph4, width = 9, height = 6)
ggsave("graphs/graph5_sex.png", graph5, width = 9, height = 6)
ggsave("graphs/graph6_age_treatment.png", graph6, width = 9, height = 6)

# 7. Check dataset size
print(dim(screentimementalhealth))

cat("Analysis complete! Six graphs saved in the graphs folder.\n")

