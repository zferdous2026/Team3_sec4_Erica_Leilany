library(dplyr)
library(ggplot2)
data <- read.csv("screensandmentalhealth.csv")
summary(data)

head(data)
summary(data)
colSums(is.na(data))
data<- data%>%
  distinct()
ggplot(data,aes (x=variable)) +
  geom_histogram() 


# analysis.R  -- Screen time vs. mental health (children 0-17)
# Data Lead: clean the data, explore it, and make the final graphs here.
# Run the whole script with Ctrl+Shift+Enter (or the "Source" button).

library(tidyverse)   # loads dplyr, ggplot2, readr

# ---- 1. LOAD ---------------------------------------------------------------
data <- read_csv("data.csv")

# First look at the data (run these one at a time in the Console)
glimpse(data)          # columns and types
nrow(data)             # 41,890 rows: each row is ONE child
colSums(is.na(data))   # missing values per column
summary(data)

# ---- 2. CLEAN --------------------------------------------------------------
# Give the columns short names without spaces or parentheses
data <- data %>%
  rename(Treatment   = `Mental Health Treatment`,
         NotReceived = `Treatment Not Received`,
         Age         = `Age (Years)`)

# Turn text columns into factors with a sensible ORDER (otherwise R sorts
# the screen time bars alphabetically: "1 hour", "2 hours", "4 or more", "Less than 1"...)
data <- data %>%
  mutate(
    Screentime = factor(Screentime,
                        levels = c("Less than 1 hour", "1 hour", "2 hours",
                                   "3 hours", "4 or more hours")),
    Treatment  = factor(Treatment,
                        levels = c("Does Not Need Treatment", "Needs Treatment")),
    Sex        = factor(Sex),
    # TRUE/FALSE version of Treatment: makes percentages easy (mean of TRUE/FALSE = proportion)
    Needs      = Treatment == "Needs Treatment",
    # Age groups: the data goes from 0 to 17, so single years are too noisy for some graphs
    AgeGroup   = cut(Age, breaks = c(-1, 5, 11, 17),
                     labels = c("0-5", "6-11", "12-17"))
  )

# NotReceived is empty (NA) for about 97% of rows. That is NOT an error:
# it is only filled in for a small group of kids. We do not use it in the graphs
# (see README for the question to ask your teacher).
sum(is.na(data$NotReceived))

# ---- 3. EXPLORE ------------------------------------------------------------
table(data$Screentime)
prop.table(table(data$Screentime, data$Treatment), margin = 1)   # row percentages

data %>% group_by(Screentime) %>% summarise(Pct_Needs = mean(Needs) * 100)
data %>% group_by(AgeGroup)   %>% summarise(Pct_Needs = mean(Needs) * 100)
data %>% group_by(Sex)        %>% summarise(Pct_Needs = mean(Needs) * 100)

# ---- 4. GRAPHS -------------------------------------------------------------

# Graph 1 (Question 1): How many children are in each screen time group?
g1 <- data %>%
  count(Screentime) %>%
  ggplot(aes(x = Screentime, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Children by Daily Screen Time",
       x = "Daily screen time", y = "Number of children") +
  theme_minimal()
g1
ggsave("graph1_screentime_counts.png", g1, width = 7, height = 5)

# Graph 2 (Question 2): Does the share needing treatment rise with screen time?
g2 <- data %>%
  group_by(Screentime) %>%
  summarise(Pct_Needs = mean(Needs) * 100) %>%
  ggplot(aes(x = Screentime, y = Pct_Needs)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = paste0(round(Pct_Needs, 1), "%")), vjust = -0.5) +
  labs(title = "Percent of Children Needing Mental Health Treatment",
       x = "Daily screen time", y = "% needing treatment") +
  theme_minimal()
g2
ggsave("graph2_pct_needs_by_screentime.png", g2, width = 7, height = 5)

# Graph 3 (Question 3): Is the pattern still there inside each age group?
# (Older kids have more screen time AND more need, so age could be hiding the real story.)
g3 <- data %>%
  group_by(AgeGroup, Screentime) %>%
  summarise(Pct_Needs = mean(Needs) * 100, .groups = "drop") %>%
  ggplot(aes(x = Screentime, y = Pct_Needs, color = AgeGroup, group = AgeGroup)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Need for Treatment by Screen Time, Within Each Age Group",
       x = "Daily screen time", y = "% needing treatment", color = "Age group") +
  theme_minimal()
g3
ggsave("graph3_age_groups.png", g3, width = 7, height = 5)

# Graph 4 (Question 4): How does need change with age, and do boys and girls differ?
g4 <- data %>%
  group_by(Age, Sex) %>%
  summarise(Pct_Needs = mean(Needs) * 100, .groups = "drop") %>%
  ggplot(aes(x = Age, y = Pct_Needs, color = Sex)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Need for Treatment by Age and Sex",
       x = "Age (years)", y = "% needing treatment") +
  theme_minimal()
g4
ggsave("graph4_age_sex.png", g4, width = 7, height = 5)

data <- read.csv("data.csv", check.names = FALSE)
getwd()
list.files()


head(data)
names(data) 
data <- read_csv("screenandmentalhealth.csv")
data <- read_csv("data.csv")

library(dplyr)
library(ggplot2)

data <- read.csv("screenandmentalhealth.csv", check.names = FALSE)
names(data)
nrow(data)
