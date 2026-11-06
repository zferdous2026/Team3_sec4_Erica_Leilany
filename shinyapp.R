library(shiny)
library(dplyr)
library(ggplot2)
data <- read.csv("mentalhealth.csv")
ui <- navbarPage(
  title = "Our Data Project",
  tabPanel(
    "About",
    h2("Welcome to Our Project"),
    p("Here we explain what our project is about.")
  ),
  tabPanel(
    "Graph 1",
    h2("Our First Graph"),
    p("Here we will explain what our graph shows."),
    plotOutput("graph1")
  ),
  tabPanel(
    "Data",
    h2("Our Dataset"),
    tableOutput("data_table")
  )
)
server <- function(input, output) {
  output$graph1 <- renderPlot({
    # Our graph code will go here
  })
  output$data_table <- renderTable({
    head(data, 20)
  })
}

shinyApp(ui = ui, server = server)