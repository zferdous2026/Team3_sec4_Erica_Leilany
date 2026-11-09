library(shiny)
library(dplyr)
library(ggplot2)

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
      theme_minimal()
  })
  
  output$graph2 <- renderPlot({
    data %>%
      group_by(Screentime) %>%
      summarise(Pct = mean(Needs_Treatment) * 100) %>%
      ggplot(aes(x = Screentime, y = Pct)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = paste0(round(Pct, 1), "%")), vjust = -0.4) +
      labs(x = "Daily screen time", y = "% needing treatment") +
      theme_minimal()
  })
  
  output$graph3 <- renderPlot({
    data %>%
      group_by(Age) %>%
      summarise(Pct = mean(Needs_Treatment) * 100) %>%
      ggplot(aes(x = Age, y = Pct)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(x = "Age (years)", y = "% needing treatment") +
      theme_minimal()
  })
  
  output$graph4 <- renderPlot({
    data %>%
      group_by(Screentime, Sex) %>%
      summarise(Pct = mean(Needs_Treatment) * 100, .groups = "drop") %>%
      ggplot(aes(x = Screentime, y = Pct, fill = Sex)) +
      geom_col(position = "dodge") +
      labs(x = "Daily screen time", y = "% needing treatment") +
      theme_minimal()
  })
  
  output$graph5 <- renderPlot({
    data %>%
      count(Age, Screentime) %>%
      group_by(Age) %>%
      mutate(Pct = n / sum(n) * 100) %>%
      ungroup() %>%
      ggplot(aes(x = Age, y = Pct, color = Screentime)) +
      geom_line(linewidth = 1) +
      labs(x = "Age (years)", y = "% of children at that age", color = "Screen time") +
      theme_minimal()
  })
  
  output$data_table <- renderTable({
    head(data %>% select(Screentime, Treatment, Age, Sex, Needs_Treatment), 20)
  })
}

shinyApp(ui = ui, server = server)