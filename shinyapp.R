
# ==========================================
# SCREEN TIME AND MENTAL HEALTH SHINY APP
# ==========================================

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# ==========================================
# DATA
# ==========================================

# Load the dataset used for this project
data <- read_csv(
  "screentimementalhealth.csv",
  show_col_types = FALSE
)

# Clean column values
data <- data %>%
  mutate(
    across(
      where(is.character),
      ~ trimws(.x)
    )
  ) %>%
  filter(
    !if_all(everything(), is.na)
  ) %>%
  mutate(
    `Age (Years)` = parse_number(as.character(`Age (Years)`)),
    Screentime = factor(
      Screentime,
      levels = c(
        "Less than 1 hour",
        "1 hour",
        "2 hours",
        "3 hours",
        "4 or more hours"
      )
    )
  )

# Create age groups for Graph 6
data <- data %>%
  mutate(
    Age_Group = case_when(
      `Age (Years)` >= 0 & `Age (Years)` <= 5 ~ "Preschool",
      `Age (Years)` >= 6 & `Age (Years)` <= 10 ~ "Young Children",
      `Age (Years)` >= 11 & `Age (Years)` <= 13 ~ "Older Children",
      `Age (Years)` >= 14 & `Age (Years)` <= 17 ~ "Adolescents",
      TRUE ~ NA_character_
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "Preschool",
        "Young Children",
        "Older Children",
        "Adolescents"
      )
    )
  )

# ==========================================
# GRAPH THEME
# ==========================================

app_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 17,
        color = "#403653"
      ),
      plot.subtitle = element_text(
        color = "#77717F",
        size = 11
      ),
      axis.title = element_text(
        face = "bold",
        color = "#514A5C"
      ),
      axis.text = element_text(
        color = "#625C6B"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "#EAE6EF"
      ),
      legend.title = element_text(
        face = "bold"
      ),
      legend.position = "bottom",
      plot.margin = margin(15, 15, 15, 15)
    )
}

# ==========================================
# USER INTERFACE
# ==========================================

ui <- navbarPage(
  title = "Screen Time & Mental Health",
  
  header = tags$head(
    tags$style(HTML("
      body {
        background-color: #F8F7FA;
        font-family: Arial, sans-serif;
      }

      .navbar {
        background-color: white;
        border-bottom: 1px solid #E8E3ED;
      }

      .navbar-default .navbar-brand {
        color: #514466;
        font-weight: bold;
      }

      .navbar-default .navbar-nav > li > a {
        color: #625C6B;
      }

      .navbar-default .navbar-nav > .active > a {
        background-color: #EAE4F2;
        color: #514466;
      }

      .tab-content {
        padding: 25px;
      }

      h2, h3 {
        color: #514466;
        font-weight: bold;
      }

      .panel {
        background-color: white;
        border: 1px solid #EAE6EF;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.04);
      }

      p {
        color: #625C6B;
        line-height: 1.6;
      }

      .hero {
        background: linear-gradient(
          120deg,
          #EAE4F2,
          #E4F1EC
        );
        padding: 30px;
        border-radius: 14px;
        margin-bottom: 25px;
      }

      table {
        background-color: white;
      }
    "))
  ),
  
  # ABOUT TAB
  tabPanel(
    "About",
    div(
      class = "hero",
      h2("Screen Time and Mental Health in Children"),
      p(
        "This project examines the relationship between children's daily screen time and mental health treatment."
      )
    ),
    
    div(
      class = "panel",
      h3("Project Question"),
      p(
        "Is the amount of daily screen time related to whether a child needs mental health treatment?"
      ),
      
      h3("About the Data"),
      p(
        "The dataset includes children aged 0 to 17. It records daily screen time, mental health treatment, age, and sex."
      ),
      
      p(
        strong("Note: "),
        "This data shows associations, not causes."
      ),
      
      h3("Team"),
      p("Add your team members' names here.")
    )
  ),
  
  # GRAPH 1
  tabPanel(
    "Screen Time",
    h3("How Much Screen Time Do Children Get?"),
    p(
      "This chart shows the number of children in each daily screen time category."
    ),
    div(class = "panel", plotOutput("graph1"))
  ),
  
  # GRAPH 2
  tabPanel(
    "Treatment by Screen Time",
    h3("Mental Health Treatment by Screen Time"),
    p(
      "This chart compares mental health treatment across daily screen time categories."
    ),
    div(class = "panel", plotOutput("graph2"))
  ),
  
  # GRAPH 3
  tabPanel(
    "Treatment Not Received",
    h3("Treatment Not Received by Screen Time"),
    p(
      "This chart shows the distribution of children who did not receive needed mental health treatment across screen time categories."
    ),
    div(class = "panel", plotOutput("graph3"))
  ),
  
  # GRAPH 4
  tabPanel(
    "Age Distribution",
    h3("Age Distribution by Screen Time"),
    p(
      "This boxplot compares children's ages across daily screen time categories."
    ),
    div(class = "panel", plotOutput("graph4"))
  ),
  
  # GRAPH 5
  tabPanel(
    "Screen Time by Sex",
    h3("Screen Time Patterns by Sex"),
    p(
      "This chart compares screen time categories across sex groups."
    ),
    div(class = "panel", plotOutput("graph5"))
  ),
  
  # GRAPH 6
  tabPanel(
    "Treatment by Age Group",
    h3("Mental Health Treatment Across Age Groups"),
    p(
      "This chart compares mental health treatment across different childhood and adolescent age groups."
    ),
    div(class = "panel", plotOutput("graph6"))
  ),
  
  # DATA TAB
  tabPanel(
    "Data",
    h3("Dataset Preview"),
    p("The table below displays the first 20 rows of the cleaned dataset."),
    div(class = "panel", tableOutput("data_table"))
  )
)

# ==========================================
# SERVER
# ==========================================

server <- function(input, output) {
  
  # GRAPH 1: Screen Time Distribution
  output$graph1 <- renderPlot({
    data %>%
      filter(!is.na(Screentime)) %>%
      count(Screentime) %>%
      ggplot(aes(x = Screentime, y = n)) +
      geom_col(fill = "#B9A4D6", width = 0.7) +
      labs(
        title = "Children's Weekday Screen Time",
        x = "Daily Screen Time",
        y = "Number of Children"
      ) +
      app_theme()
  })
  
  # GRAPH 2: Treatment by Screen Time
  output$graph2 <- renderPlot({
    data %>%
      filter(
        !is.na(Screentime),
        !is.na(`Mental Health Treatment`)
      ) %>%
      ggplot(
        aes(
          x = Screentime,
          fill = `Mental Health Treatment`
        )
      ) +
      geom_bar(position = "fill") +
      scale_y_continuous(
        labels = scales::percent
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Mental Health Treatment by Screen Time",
        x = "Daily Screen Time",
        y = "Percentage of Children",
        fill = "Mental Health Treatment"
      ) +
      app_theme()
  })
  
  # GRAPH 3: Treatment Not Received
  output$graph3 <- renderPlot({
    data %>%
      filter(
        !is.na(Screentime),
        !is.na(`Treatment Not Received`)
      ) %>%
      ggplot(
        aes(
          x = Screentime,
          fill = `Treatment Not Received`
        )
      ) +
      geom_bar(position = "fill") +
      scale_y_continuous(
        labels = scales::percent
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Treatment Not Received by Screen Time",
        x = "Daily Screen Time",
        y = "Percentage of Children",
        fill = "Treatment Not Received"
      ) +
      app_theme()
  })
  
  # GRAPH 4: Age Distribution
  output$graph4 <- renderPlot({
    data %>%
      filter(
        !is.na(Screentime),
        !is.na(`Age (Years)`)
      ) %>%
      ggplot(
        aes(
          x = Screentime,
          y = `Age (Years)`
        )
      ) +
      geom_boxplot(
        fill = "#A8D8C8",
        color = "#608F80",
        alpha = 0.9
      ) +
      labs(
        title = "Age Distribution by Screen Time",
        x = "Daily Screen Time",
        y = "Age (Years)"
      ) +
      app_theme()
  })
  
  # GRAPH 5: Screen Time by Sex
  output$graph5 <- renderPlot({
    data %>%
      filter(
        !is.na(Screentime),
        !is.na(Sex)
      ) %>%
      ggplot(
        aes(
          x = Screentime,
          fill = Sex
        )
      ) +
      geom_bar(position = "fill") +
      scale_y_continuous(
        labels = scales::percent
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Screen Time Patterns by Sex",
        x = "Daily Screen Time",
        y = "Percentage of Children",
        fill = "Sex"
      ) +
      app_theme()
  })
  
  # GRAPH 6: Treatment by Age Group
  output$graph6 <- renderPlot({
    data %>%
      filter(
        !is.na(Age_Group),
        !is.na(`Mental Health Treatment`)
      ) %>%
      ggplot(
        aes(
          x = Age_Group,
          fill = `Mental Health Treatment`
        )
      ) +
      geom_bar(position = "fill") +
      scale_y_continuous(
        labels = scales::percent
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Mental Health Treatment Across Age Groups",
        x = "Age Group",
        y = "Percentage of Children",
        fill = "Mental Health Treatment"
      ) +
      app_theme()
  })
  
  # DATA TABLE
  output$data_table <- renderTable({
    head(
      data %>%
        select(
          Screentime,
          `Mental Health Treatment`,
          `Treatment Not Received`,
          `Age (Years)`,
          Sex
        ),
      20
    )
  })
}

# ==========================================
# RUN THE APP
# ==========================================

shinyApp(ui = ui, server = server)
