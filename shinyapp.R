
# ==========================================
# SCREEN TIME AND DEVICE USE SHINY APP
# ==========================================

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
<<<<<<< HEAD

# ---- DATA (same cleaning as analysis.R) ----
file_name <- ifelse(file.exists("mentalhealth.cvs"), "data.csv", "mentalhealth.csv")
data <- read.csv(file_name, stringsAsFactors = FALSE, na.strings = c("", "NA"))

names(data)[grepl("^Screentime", names(data))] <- "Screentime"
names(data)[grepl("^Mental", names(data))]     <- "Treatment"
names(data)[grepl("^Age", names(data))]        <- "Age"
names(data)[grepl("^Sex", names(data))]        <- "Sex"

screen_levels <- c("Less than 1 hour", "1 hour", "2 hours", "3 hours", "4 or more hours")

data <- data %>%
  select(Screentime, Treatment, Age, Sex) %>%
  mutate(
    Screentime      = factor(Screentime, levels = screen_levels),
    Sex             = factor(Sex),
    Needs_Treatment = Treatment == "Needs Treatment"
  )

# ---- UI ----
ui <- navbarPage(
  "Screen Time and Mental Health in Children",
  
  tabPanel("About",
           h3("Project question"),
           p("Is the amount of daily screen time related to whether a child needs mental health treatment?"),
           h3("About the data"),
           p("The dataset has children aged 0 to 17. For each child it records daily screen time,
       whether the child needs mental health treatment, age, and sex."),
           p(strong("Note:"), "This data shows associations, not causes."),
           h3("Team"),
           p("Add your team members' names here.")
  ),
  
  tabPanel("Screen Time",
           h3("How Much Screen Time Do Children Get?"),
           p("This bar chart counts the children in each daily screen time category."),
           plotOutput("graph1")
  ),
  
  tabPanel("Treatment by Screen Time",
           h3("Does More Screen Time Go With More Need for Treatment?"),
           p("This chart shows the percent of children in each screen time category who need
       mental health treatment."),
           plotOutput("graph2")
  ),
  
  tabPanel("Treatment by Age",
           h3("How Does Need for Treatment Change With Age?"),
           p("This line chart shows the percent of children at each age who need treatment."),
           plotOutput("graph3")
  ),
  
  tabPanel("Boys vs. Girls",
           h3("Is the Pattern Different for Boys and Girls?"),
           p("This chart compares boys and girls within each screen time category."),
           plotOutput("graph4")
  ),
  
  tabPanel("Screen Time by Age",
           h3("How Does Screen Time Change as Children Get Older?"),
           p("This line chart shows, for each age, the percent of children in each screen time category."),
           plotOutput("graph5")
  ),
  
  tabPanel("Data",
           h3("The Dataset (first 20 rows)"),
           tableOutput("data_table")
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  output$graph1 <- renderPlot({
    data %>%
      count(Screentime) %>%
      ggplot(aes(x = Screentime, y = n)) +
      geom_col(fill = "steelblue") +
      labs(x = "Daily screen time", y = "Number of children") +
=======
library(tidyr)

# Load data
screensandmentalhealth <- read_csv(
  "screensandmentalhealth.csv",
  show_col_types = FALSE
)

# Prepare device summary
device_data <- screensandmentalhealth %>%
  summarise(
    Phone = mean(phone_usage_hours, na.rm = TRUE),
    Laptop = mean(laptop_usage_hours, na.rm = TRUE),
    Tablet = mean(tablet_usage_hours, na.rm = TRUE),
    Television = mean(tv_usage_hours, na.rm = TRUE),
    Social_Media = mean(social_media_hours, na.rm = TRUE),
    Work_Related = mean(work_related_hours, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Device_or_Activity",
    values_to = "Average_Hours"
  )

# ==========================================
# USER INTERFACE
# ==========================================

ui <- navbarPage(
  title = "Screen Time and Device Use",
  
  # ABOUT TAB
  tabPanel(
    "About",
    h2("Screen Time and Device Use"),
    p("Research question: How do age and gender relate to daily screen time and patterns of device use?"),
    p("This project examines screen time, device use, social media activity, age, and gender."),
    p("Dataset: screensandmentalhealth.csv"),
    p("Team members: Add all team members' names here.")
  ),
  
  # GRAPH 1
  tabPanel(
    "Screen Time",
    h3("Daily Screen Time Distribution"),
    p("This histogram shows how daily screen time is distributed among participants."),
    plotOutput("graph1")
  ),
  
  # GRAPH 2
  tabPanel(
    "Gender",
    h3("Daily Screen Time by Gender"),
    p("This box plot compares daily screen time across gender categories."),
    plotOutput("graph2")
  ),
  
  # GRAPH 3
  tabPanel(
    "Age",
    h3("Age and Screen Time"),
    p("This scatter plot examines the relationship between participant age and daily screen time."),
    plotOutput("graph3")
  ),
  
  # GRAPH 4
  tabPanel(
    "Device Use",
    h3("Average Hours by Device or Activity"),
    p("This bar chart compares average daily hours reported for different devices and activities."),
    plotOutput("graph4")
  ),
  
  # GRAPH 5
  tabPanel(
    "Social Media",
    h3("Social Media and Screen Time"),
    p("This scatter plot compares social media hours with total daily screen time."),
    plotOutput("graph5")
  ),
  
  # GRAPH 6
  tabPanel(
    "Phone Use",
    h3("Phone Usage by Gender"),
    p("This box plot compares daily phone usage across gender categories."),
    plotOutput("graph6")
  ),
  
  # DATA TAB
  tabPanel(
    "Data",
    h3("Dataset Preview"),
    p("The table below displays the first 20 rows of the dataset."),
    tableOutput("data_table")
  )
)

# ==========================================
# SERVER
# ==========================================

server <- function(input, output) {
  
  output$graph1 <- renderPlot({
    ggplot(
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
>>>>>>> 58065ba2e4e8651b5b87486a7bb59cd66c2bbbdd
      theme_minimal()
  })
  
  output$graph2 <- renderPlot({
<<<<<<< HEAD
    data %>%
      group_by(Screentime) %>%
      summarise(Pct = mean(Needs_Treatment) * 100) %>%
      ggplot(aes(x = Screentime, y = Pct)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = paste0(round(Pct, 1), "%")), vjust = -0.4) +
      labs(x = "Daily screen time", y = "% needing treatment") +
=======
    ggplot(
      screensandmentalhealth,
      aes(
        x = gender,
        y = daily_screen_time_hours,
        fill = gender
      )
    ) +
      geom_boxplot() +
      labs(
        title = "Daily Screen Time by Gender",
        x = "Gender",
        y = "Daily Screen Time (Hours)"
      ) +
>>>>>>> 58065ba2e4e8651b5b87486a7bb59cd66c2bbbdd
      theme_minimal()
  })
  
  output$graph3 <- renderPlot({
<<<<<<< HEAD
    data %>%
      group_by(Age) %>%
      summarise(Pct = mean(Needs_Treatment) * 100) %>%
      ggplot(aes(x = Age, y = Pct)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(x = "Age (years)", y = "% needing treatment") +
=======
    ggplot(
      screensandmentalhealth,
      aes(
        x = age,
        y = daily_screen_time_hours
      )
    ) +
      geom_point(alpha = 0.5, color = "darkgreen") +
      geom_smooth(method = "lm", se = TRUE) +
      labs(
        title = "Age and Daily Screen Time",
        x = "Age (Years)",
        y = "Daily Screen Time (Hours)"
      ) +
>>>>>>> 58065ba2e4e8651b5b87486a7bb59cd66c2bbbdd
      theme_minimal()
  })
  
  output$graph4 <- renderPlot({
<<<<<<< HEAD
    data %>%
      group_by(Screentime, Sex) %>%
      summarise(Pct = mean(Needs_Treatment) * 100, .groups = "drop") %>%
      ggplot(aes(x = Screentime, y = Pct, fill = Sex)) +
      geom_col(position = "dodge") +
      labs(x = "Daily screen time", y = "% needing treatment") +
=======
    ggplot(
      device_data,
      aes(
        x = reorder(Device_or_Activity, Average_Hours),
        y = Average_Hours
      )
    ) +
      geom_col(fill = "coral") +
      coord_flip() +
      labs(
        title = "Average Hours by Device or Activity",
        x = "Device or Activity",
        y = "Average Hours"
      ) +
>>>>>>> 58065ba2e4e8651b5b87486a7bb59cd66c2bbbdd
      theme_minimal()
  })
  
  output$graph5 <- renderPlot({
<<<<<<< HEAD
    data %>%
      count(Age, Screentime) %>%
      group_by(Age) %>%
      mutate(Pct = n / sum(n) * 100) %>%
      ungroup() %>%
      ggplot(aes(x = Age, y = Pct, color = Screentime)) +
      geom_line(linewidth = 1) +
      labs(x = "Age (years)", y = "% of children at that age", color = "Screen time") +
=======
    ggplot(
      screensandmentalhealth,
      aes(
        x = daily_screen_time_hours,
        y = social_media_hours
      )
    ) +
      geom_point(alpha = 0.5, color = "purple") +
      geom_smooth(method = "lm", se = TRUE) +
      labs(
        title = "Social Media and Daily Screen Time",
        x = "Daily Screen Time (Hours)",
        y = "Social Media Hours"
      ) +
      theme_minimal()
  })
  
  output$graph6 <- renderPlot({
    ggplot(
      screensandmentalhealth,
      aes(
        x = gender,
        y = phone_usage_hours,
        fill = gender
      )
    ) +
      geom_boxplot() +
      labs(
        title = "Phone Usage by Gender",
        x = "Gender",
        y = "Daily Phone Usage (Hours)"
      ) +
>>>>>>> 58065ba2e4e8651b5b87486a7bb59cd66c2bbbdd
      theme_minimal()
  })
  
  output$data_table <- renderTable({
<<<<<<< HEAD
    head(data %>% select(Screentime, Treatment, Age, Sex, Needs_Treatment), 20)
=======
    head(screensandmentalhealth, 20)
>>>>>>> 58065ba2e4e8651b5b87486a7bb59cd66c2bbbdd
  })
}

# ==========================================
# RUN THE APP
# ==========================================

shinyApp(ui = ui, server = server)
