library(shiny)
library(tidyverse)
library(summarytools)

# Load data
data <- read_csv("UCSF-PDGM-metadata_v2-BrainGlioma.csv")

ui <- fluidPage(
  titlePanel("Exploratory Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Select a variable to display:"),
      selectInput("variable", "Variable:", choices = names(data)),
      h4("Select a visualization type:"),
      selectInput("plotType", "Plot type:", 
                  choices = c("Histogram", "Density Plot", "Box Plot", "Bar Chart", "Scatter Plot"))
    ),
    mainPanel(
      h3("Descriptive Statistics:"),
      verbatimTextOutput("descr"),
      h3("Visualization:"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  # Render descriptive statistics
  output$descr <- renderPrint({
    descr(data[input$variable])
  })
  
  # Render selected plot
  output$plot <- renderPlot({
    if (input$plotType == "Histogram") {
      ggplot(data, aes(x = data[[input$variable]])) + 
        geom_histogram()
    } else if (input$plotType == "Density Plot") {
      ggplot(data, aes(x = data[[input$variable]])) + 
        geom_density()
    } else if (input$plotType == "Box Plot") {
      ggplot(data, aes(x = 1, y = data[[input$variable]])) + 
        geom_boxplot()
    } else if (input$plotType == "Bar Chart") {
      ggplot(data, aes(x = factor(data[[input$variable]]))) + 
        geom_bar()
    } else if (input$plotType == "Scatter Plot") {
      ggplot(data, aes(x = variable1, y = variable2)) + 
        geom_point()
    }
  })
}

shinyApp(ui, server)
