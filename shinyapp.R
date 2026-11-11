
# ==========================================
# SCREEN TIME AND DEVICE USE SHINY APP
# ==========================================

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
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
      theme_minimal()
  })
  
  output$graph2 <- renderPlot({
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
      theme_minimal()
  })
  
  output$graph3 <- renderPlot({
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
      theme_minimal()
  })
  
  output$graph4 <- renderPlot({
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
      theme_minimal()
  })
  
  output$graph5 <- renderPlot({
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
      theme_minimal()
  })
  
  output$data_table <- renderTable({
    head(screensandmentalhealth, 20)
  })
}

# ==========================================
# RUN THE APP
# ==========================================

shinyApp(ui = ui, server = server)
