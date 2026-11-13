
# ============================================================
# TEAM 3: CHILDREN'S SCREEN TIME & MENTAL HEALTH
# SHINY DASHBOARD
# ============================================================

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

data_file <- "screentimementalhealth.csv"

screentimementalhealth <- read_csv(
  data_file,
  show_col_types = FALSE
)

# Create age groups, following your original R analysis

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

# ============================================================
# 2. DESIGN AND APPEARANCE
# ============================================================

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#315B6D",
  secondary = "#7896A3",
  success = "#79A99D",
  base_font = font_collection(
    "Arial",
    "Helvetica",
    "sans-serif"
  )
)

plot_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17,
      color = "#315B6D"
    ),
    axis.title = element_text(
      face = "bold",
      color = "#526775"
    ),
    axis.text = element_text(color = "#526775"),
    legend.title = element_text(
      face = "bold",
      color = "#315B6D"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

# ============================================================
# 3. USER INTERFACE
# ============================================================

ui <- navbarPage(
  
  title = "SCREEN TIME & MENTAL HEALTH",
  
  id = "navigation",
  
  theme = app_theme,
  
  header = tags$head(
    tags$style(HTML("

      body {
        background-color: #F3F6F8;
        color: #263746;
        font-family: Arial, Helvetica, sans-serif;
      }

      .navbar {
        background-color: white !important;
        border-bottom: 1px solid #E1E8EC;
        box-shadow: 0 2px 8px rgba(0,0,0,0.04);
      }

      .navbar-brand {
        color: #315B6D !important;
        font-weight: 800;
        letter-spacing: 0.5px;
      }

      .navbar-nav .nav-link {
        color: #526775 !important;
        font-weight: 600;
        font-size: 13px;
      }

      .navbar-nav .nav-link.active {
        color: #315B6D !important;
        border-bottom: 3px solid #79A99D;
      }

      .main-container {
        max-width: 1350px;
        margin: auto;
        padding: 30px;
      }

      .hero {
        background: linear-gradient(120deg, #315B6D, #547D8B);
        color: white;
        padding: 40px;
        border-radius: 15px;
        margin-bottom: 30px;
      }

      .hero h1 {
        font-size: 30px;
        font-weight: 800;
      }

      .hero p {
        color: #EDF4F6;
        line-height: 1.8;
        font-size: 14px;
      }

      .section-title {
        color: #315B6D;
        font-size: 24px;
        font-weight: 800;
        margin-bottom: 8px;
      }

      .section-description {
        color: #71818A;
        font-size: 14px;
        margin-bottom: 22px;
        line-height: 1.7;
      }

      .card-box {
        background: white;
        border: 1px solid #E4EAEE;
        border-radius: 13px;
        padding: 24px;
        margin-bottom: 22px;
        box-shadow: 0 3px 12px rgba(30,55,70,0.04);
      }

      .card-box h3 {
        color: #315B6D;
        font-size: 18px;
        font-weight: 750;
        margin-bottom: 15px;
      }

      .stat-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 15px;
        margin: 20px 0;
      }

      .stat-card {
        background: white;
        border: 1px solid #E2E9ED;
        border-radius: 12px;
        padding: 20px;
      }

      .stat-label {
        color: #78909B;
        font-size: 11px;
        font-weight: bold;
        text-transform: uppercase;
        letter-spacing: 1px;
      }

      .stat-value {
        color: #315B6D;
        font-size: 26px;
        font-weight: 800;
        margin-top: 8px;
      }

      .stat-note {
        color: #84939B;
        font-size: 11px;
      }

      .note-box {
        background: #F1F6F6;
        border-left: 4px solid #79A99D;
        padding: 14px;
        border-radius: 5px;
        margin-top: 15px;
        color: #526775;
        font-size: 13px;
        line-height: 1.7;
      }

      .footer {
        text-align: center;
        padding: 25px;
        color: #8999A2;
        font-size: 11px;
      }

      @media (max-width: 800px) {
        .stat-grid {
          grid-template-columns: repeat(2, 1fr);
        }
        .main-container {
          padding: 15px;
        }
      }

    "))
  ),
  
  # ----------------------------------------------------------
  # TAB 1: OVERVIEW
  # ----------------------------------------------------------
  
  tabPanel(
    "Overview",
    
    div(
      class = "main-container",
      
      div(
        class = "hero",
        
        h1("Children's Screen Time & Mental Health"),
        
        p(
          "An interactive exploration of weekday screen time ",
          "and its relationship with mental health treatment, ",
          "age, and sex in the dataset."
        ),
        
        p(
          "Explore six visualizations, descriptive statistics, ",
          "and the original dataset through the tabs above."
        )
      ),
      
      h2(class = "section-title", "Dataset Overview"),
      
      p(
        class = "section-description",
        "A quick summary of the observations and variables included in this analysis."
      ),
      
      div(
        class = "stat-grid",
        
        div(
          class = "stat-card",
          div(class = "stat-label", "Total observations"),
          div(class = "stat-value",
              textOutput("total_n", inline = TRUE))
        ),
        
        div(
          class = "stat-card",
          div(class = "stat-label", "Average age"),
          div(class = "stat-value",
              textOutput("average_age", inline = TRUE)),
          div(class = "stat-note", "Years")
        ),
        
        div(
          class = "stat-card",
          div(class = "stat-label", "Needs treatment"),
          div(class = "stat-value",
              textOutput("treatment_percent", inline = TRUE))
        ),
        
        
      ),
      
      div(
        class = "card-box",
        
        h3("Research Focus"),
        
        p(
          "This project examines weekday screen time categories ",
          "alongside recorded mental health treatment status, ",
          "treatment not received, age, and sex."
        ),
        
        p(
          "The visualizations describe patterns and associations ",
          "in the dataset. They do not establish that screen time ",
          "causes a mental health outcome."
        )
      ),
      
      div(
        class = "card-box",
        
        h3("About the Analysis"),
        
        p(strong("Screen Time: "),
          "Weekday screen time category."),
        
        p(strong("Mental Health Treatment: "),
          "Recorded mental health treatment status."),
        
        p(strong("Treatment Not Received: "),
          "Recorded category for treatment not received."),
        
        p(strong("Age: "),
          "Age in years, also organized into four age groups."),
        
        p(strong("Sex: "),
          "Recorded sex category.")
      )
      
    )
  ),
  
  # ----------------------------------------------------------
  # TAB 2: GRAPH 1 - SCREEN TIME DISTRIBUTION
  # ----------------------------------------------------------
  
  tabPanel(
    "1. Screen Time",
    
    div(
      class = "main-container",
      
      h2(class = "section-title",
         "Weekday Screen Time Distribution"),
      
      p(
        class = "section-description",
        "Graph 1 from the original R analysis. ",
        "This graph displays the number of children in each ",
        "weekday screen time category."
      ),
      
      div(
        class = "card-box",
        
        h3("Children's Weekday Screen Time"),
        
        plotOutput("graph1", height = "430px")
      ),
      
      div(
        class = "card-box",
        
        h3("Screen Time Statistics"),
        
        DTOutput("stats1")
      )
    )
  ),
  
  # ----------------------------------------------------------
  # TAB 3: GRAPH 2 - MENTAL HEALTH TREATMENT
  # ----------------------------------------------------------
  
  tabPanel(
    "2. Treatment",
    
    div(
      class = "main-container",
      
      h2(class = "section-title",
         "Mental Health Treatment by Screen Time"),
      
      p(
        class = "section-description",
        "Graph 2 from the original R analysis. ",
        "The proportional bars compare mental health treatment ",
        "statuses within each screen time category."
      ),
      
      div(
        class = "card-box",
        
        h3("Mental Health Treatment by Screen Time"),
        
        plotOutput("graph2", height = "430px")
      ),
      
      div(
        class = "card-box",
        
        h3("Treatment Statistics"),
        
        DTOutput("stats2")
      )
    )
  ),
  
  # ----------------------------------------------------------
  # TAB 4: GRAPH 3 - TREATMENT NOT RECEIVED
  # ----------------------------------------------------------
  
  tabPanel(
    "3. Treatment Not Received",
    
    div(
      class = "main-container",
      
      h2(class = "section-title",
         "Treatment Not Received"),
      
      p(
        class = "section-description",
        "Graph 3 from the original R analysis. ",
        "This graph examines the recorded treatment not received ",
        "categories across screen time groups."
      ),
      
      div(
        class = "card-box",
        
        h3("Treatment Not Received by Screen Time"),
        
        plotOutput("graph3", height = "430px")
      ),
      
      div(
        class = "card-box",
        
        h3("Treatment Not Received Statistics"),
        
        textOutput("missing_treatment_note"),
        
        DTOutput("stats3")
      )
    )
  ),
  
  # ----------------------------------------------------------
  # TAB 5: GRAPH 4 - AGE DISTRIBUTION
  # ----------------------------------------------------------
  
  tabPanel(
    "4. Age Distribution",
    
    div(
      class = "main-container",
      
      h2(class = "section-title",
         "Age Distribution by Screen Time"),
      
      p(
        class = "section-description",
        "Graph 4 from the original R analysis. ",
        "The boxplot compares the distribution of ages ",
        "across weekday screen time categories."
      ),
      
      div(
        class = "card-box",
        
        h3("Age Distribution by Screen Time"),
        
        plotOutput("graph4", height = "430px")
      ),
      
      div(
        class = "card-box",
        
        h3("Age Descriptive Statistics"),
        
        DTOutput("stats4")
      )
    )
  ),
  
  # ----------------------------------------------------------
  # TAB 6: GRAPH 5 - SEX
  # ----------------------------------------------------------
  
  tabPanel(
    "5. Sex",
    
    div(
      class = "main-container",
      
      h2(class = "section-title",
         "Screen Time Patterns by Sex"),
      
      p(
        class = "section-description",
        "Graph 5 from the original R analysis. ",
        "The proportional bars compare recorded sex categories ",
        "within each screen time group."
      ),
      
      div(
        class = "card-box",
        
        h3("Screen Time Patterns by Sex"),
        
        plotOutput("graph5", height = "430px")
      ),
      
      div(
        class = "card-box",
        
        h3("Sex Distribution Statistics"),
        
        DTOutput("stats5")
      )
    )
  ),
  
  # ----------------------------------------------------------
  # TAB 7: GRAPH 6 - AGE AND TREATMENT
  # ----------------------------------------------------------
  
  tabPanel(
    "6. Age & Treatment",
    
    div(
      class = "main-container",
      
      h2(class = "section-title",
         "Mental Health Treatment Across Age Groups"),
      
      p(
        class = "section-description",
        "Graph 6 from the original R analysis. ",
        "This graph compares mental health treatment status ",
        "across the four age groups."
      ),
      
      div(
        class = "card-box",
        
        h3("Treatment Across Age Groups"),
        
        plotOutput("graph6", height = "430px")
      ),
      
      div(
        class = "card-box",
        
        h3("Age Group Treatment Statistics"),
        
        DTOutput("stats6")
      )
    )
  ),
  
  # ----------------------------------------------------------
  # TAB 8: DATA EXPLORER
  # ----------------------------------------------------------
  
  tabPanel(
    "Data Explorer",
    
    div(
      class = "main-container",
      
      h2(class = "section-title", "Explore the Dataset"),
      
      p(
        class = "section-description",
        "Browse, search, and filter the observations ",
        "included in the analysis."
      ),
      
      div(
        class = "card-box",
        
        h3("Complete Dataset"),
        
        DTOutput("data_table")
      ),
      
      div(
        class = "card-box",
        
        h3("Summary of Variables"),
        
        verbatimTextOutput("data_summary")
      )
    )
  )
  
)

# ============================================================
# 4. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # HELPER FOR STATISTICS TABLES
  # ----------------------------------------------------------
  
  make_table <- function(x) {
    
    datatable(
      x,
      rownames = FALSE,
      filter = "none",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      class = "stripe hover compact"
    )
  }
  
  # ----------------------------------------------------------
  # OVERVIEW STATISTICS
  # ----------------------------------------------------------
  
  output$total_n <- renderText({
    format(nrow(screentimementalhealth), big.mark = ",")
  })
  
  output$average_age <- renderText({
    
    mean_age <- mean(
      screentimementalhealth$`Age (Years)`,
      na.rm = TRUE
    )
    
    sprintf("%.1f", mean_age)
  })
  
  output$treatment_percent <- renderText({
    
    x <- screentimementalhealth$`Mental Health Treatment`
    
    if (all(is.na(x))) {
      return("N/A")
    }
    
    percent(
      mean(x == "Needs Treatment", na.rm = TRUE),
      accuracy = 0.1
    )
  })
  
  output$missing_total <- renderText({
    format(sum(is.na(screentimementalhealth)), big.mark = ",")
  })
  
  # ==========================================================
  # GRAPH 1: SCREEN TIME DISTRIBUTION
  # ==========================================================
  
  output$graph1 <- renderPlot({
    
    ggplot(
      screentimementalhealth,
      aes(x = Screentime)
    ) +
      
      geom_bar(fill = "#B9A4D6") +
      
      labs(
        title = "Children's Weekday Screen Time",
        x = "Screen Time",
        y = "Number of Children"
      ) +
      
      plot_theme +
      
      theme(
        axis.text.x = element_text(
          angle = 25,
          hjust = 1
        )
      )
    
  })
  
  output$stats1 <- renderDT({
    
    tab <- screentimementalhealth %>%
      
      count(Screentime, name = "Children") %>%
      
      mutate(
        Percentage = percent(
          Children / sum(Children),
          accuracy = 0.1
        )
      )
    
    make_table(tab)
    
  })
  
  # ==========================================================
  # GRAPH 2: MENTAL HEALTH TREATMENT BY SCREEN TIME
  # ==========================================================
  
  output$graph2 <- renderPlot({
    
    ggplot(
      screentimementalhealth,
      aes(
        x = Screentime,
        fill = `Mental Health Treatment`
      )
    ) +
      
      geom_bar(position = "fill") +
      
      scale_y_continuous(
        labels = scales::percent
      ) +
      
      labs(
        title = "Mental Health Treatment by Screen Time",
        x = "Screen Time",
        y = "Percentage of Children",
        fill = "Treatment"
      ) +
      
      plot_theme +
      
      theme(
        axis.text.x = element_text(
          angle = 25,
          hjust = 1
        )
      )
    
  })
  
  output$stats2 <- renderDT({
    
    tab <- screentimementalhealth %>%
      
      filter(
        !is.na(Screentime),
        !is.na(`Mental Health Treatment`)
      ) %>%
      
      count(
        Screentime,
        `Mental Health Treatment`,
        name = "Children"
      ) %>%
      
      group_by(Screentime) %>%
      
      mutate(
        Percentage = percent(
          Children / sum(Children),
          accuracy = 0.1
        )
      ) %>%
      
      ungroup()
    
    make_table(tab)
    
  })
  
  # ==========================================================
  # GRAPH 3: TREATMENT NOT RECEIVED
  # ==========================================================
  
  output$graph3 <- renderPlot({
    
    ggplot(
      screentimementalhealth,
      aes(
        x = Screentime,
        fill = `Treatment Not Received`
      )
    ) +
      
      geom_bar(position = "fill") +
      
      scale_y_continuous(
        labels = scales::percent
      ) +
      
      labs(
        title = "Treatment Not Received by Screen Time",
        x = "Screen Time",
        y = "Percentage of Children",
        fill = "Treatment Not Received"
      ) +
      
      plot_theme +
      
      theme(
        axis.text.x = element_text(
          angle = 25,
          hjust = 1
        )
      )
    
  })
  
  output$missing_treatment_note <- renderText({
    
    valid <- sum(
      !is.na(screentimementalhealth$`Treatment Not Received`)
    )
    
    total <- nrow(screentimementalhealth)
    
    paste0(
      valid, " non-missing responses out of ",
      total, " total observations. ",
      percent(valid / total, accuracy = 0.1),
      " of the dataset has a recorded response."
    )
    
  })
  
  output$stats3 <- renderDT({
    
    tab <- screentimementalhealth %>%
      
      filter(
        !is.na(Screentime),
        !is.na(`Treatment Not Received`)
      ) %>%
      
      count(
        Screentime,
        `Treatment Not Received`,
        name = "Responses"
      ) %>%
      
      group_by(Screentime) %>%
      
      mutate(
        Percentage = percent(
          Responses / sum(Responses),
          accuracy = 0.1
        )
      ) %>%
      
      ungroup()
    
    make_table(tab)
    
  })
  
  # ==========================================================
  # GRAPH 4: AGE DISTRIBUTION BY SCREEN TIME
  # ==========================================================
  
  output$graph4 <- renderPlot({
    
    ggplot(
      screentimementalhealth,
      aes(
        x = Screentime,
        y = `Age (Years)`
      )
    ) +
      
      geom_boxplot(fill = "#A8D8C8") +
      
      labs(
        title = "Age Distribution by Screen Time",
        x = "Screen Time",
        y = "Age in Years"
      ) +
      
      plot_theme +
      
      theme(
        axis.text.x = element_text(
          angle = 25,
          hjust = 1
        )
      )
    
  })
  
  output$stats4 <- renderDT({
    
    tab <- screentimementalhealth %>%
      
      filter(
        !is.na(Screentime),
        !is.na(`Age (Years)`)
      ) %>%
      
      group_by(Screentime) %>%
      
      summarise(
        N = n(),
        Mean_Age = mean(`Age (Years)`),
        SD_Age = sd(`Age (Years)`),
        Median_Age = median(`Age (Years)`),
        Q1 = quantile(`Age (Years)`, 0.25),
        Q3 = quantile(`Age (Years)`, 0.75),
        .groups = "drop"
      )
    
    make_table(tab)
    
  })
  
  # ==========================================================
  # GRAPH 5: SCREEN TIME PATTERNS BY SEX
  # ==========================================================
  
  output$graph5 <- renderPlot({
    
    ggplot(
      screentimementalhealth,
      aes(
        x = Screentime,
        fill = Sex
      )
    ) +
      
      geom_bar(position = "fill") +
      
      scale_y_continuous(
        labels = scales::percent
      ) +
      
      labs(
        title = "Screen Time Patterns by Sex",
        x = "Screen Time",
        y = "Percentage of Children",
        fill = "Sex"
      ) +
      
      plot_theme +
      
      theme(
        axis.text.x = element_text(
          angle = 25,
          hjust = 1
        )
      )
    
  })
  
  output$stats5 <- renderDT({
    
    tab <- screentimementalhealth %>%
      
      filter(
        !is.na(Screentime),
        !is.na(Sex)
      ) %>%
      
      count(
        Screentime,
        Sex,
        name = "Children"
      ) %>%
      
      group_by(Screentime) %>%
      
      mutate(
        Percentage = percent(
          Children / sum(Children),
          accuracy = 0.1
        )
      ) %>%
      
      ungroup()
    
    make_table(tab)
    
  })
  
  # ==========================================================
  # GRAPH 6: TREATMENT ACROSS AGE GROUPS
  # ==========================================================
  
  output$graph6 <- renderPlot({
    
    ggplot(
      screentimementalhealth,
      aes(
        x = Age_Group,
        fill = `Mental Health Treatment`
      )
    ) +
      
      geom_bar(position = "fill") +
      
      scale_y_continuous(
        labels = scales::percent
      ) +
      
      labs(
        title = "Mental Health Treatment Across Age Groups",
        x = "Age Group",
        y = "Percentage of Children",
        fill = "Treatment"
      ) +
      
      plot_theme +
      
      theme(
        axis.text.x = element_text(
          angle = 20,
          hjust = 1
        )
      )
    
  })
  
  output$stats6 <- renderDT({
    
    tab <- screentimementalhealth %>%
      
      filter(
        !is.na(Age_Group),
        !is.na(`Mental Health Treatment`)
      ) %>%
      
      count(
        Age_Group,
        `Mental Health Treatment`,
        name = "Children"
      ) %>%
      
      group_by(Age_Group) %>%
      
      mutate(
        Percentage = percent(
          Children / sum(Children),
          accuracy = 0.1
        )
      ) %>%
      
      ungroup()
    
    make_table(tab)
    
  })
  
  # ==========================================================
  # DATA EXPLORER
  # ==========================================================
  
  output$data_table <- renderDT({
    
    datatable(
      screentimementalhealth,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      class = "stripe hover compact"
    )
    
  })
  
  output$data_summary <- renderPrint({
    
    summary(screentimementalhealth)
    
  })
  
}

# ============================================================
# 5. RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
