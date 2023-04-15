# Load the necessary libraries
library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(glmnet)
library(skimr)

# Load the data
data <- read.csv("MyData-BrainGlioma.csv")

# Exploring Dataset
head(data, 5)
dim(data)
glimpse(data)
summary(data)
skim(data)

# Cleaning Data
data$Sex[data$Sex == "M"] <- as.integer(1)
data$Sex[data$Sex == "F"] <- as.integer(2)
data <- mutate(data, Sex = as.integer(Sex))

data$MGMT.status[data$MGMT.status == "positive"] <- 1
data$MGMT.status[data$MGMT.status == "negative"] <- 2
data$MGMT.status[data$MGMT.status == "indeterminate"] <- 3
data <- mutate(data, MGMT.status = as.integer(MGMT.status))

data$Biopsy.prior.to.imaging[data$Biopsy.prior.to.imaging == "Yes"] <- 1
data$Biopsy.prior.to.imaging[data$Biopsy.prior.to.imaging == "No"] <- 2
data <- mutate(data, Biopsy.prior.to.imaging = as.integer(Biopsy.prior.to.imaging))

data$EOR[data$EOR == "biopsy"] <- 1
data$EOR[data$EOR == "STR"] <- 2
data$EOR[data$EOR == "GTR"] <- 3
data <- mutate(data, EOR = as.integer(EOR))

# Exploring Dataset
head(data, 5)
dim(data)
glimpse(data)
summary(data)
skim(data)

# Define UI
ui <- fluidPage(
  
  # Create input fields for predictor variables
  numericInput(inputId = "Sex", label = "Sex(1-Male, 2-Female):", value = 0),
  numericInput(inputId = "Age.at.MRI", label = "Age at MRI:", value = 0),
  numericInput(inputId = "MGMT.status", label = "MGMT Status(1-Positive, 2-Negative, 3-Indeterminate):", value = 0),
  numericInput(inputId = "Biopsy.prior.to.imaging", label = "Biopsy Prior to Imaging(1-Yes, 2-No):", value = 0),
  numericInput(inputId = "EOR", label = "EOR(1-biopsy, 2-STR, 3-GTR):", value = 0),
  
  # Create a button to submit input
  actionButton(inputId = "submit", label = "Submit"),
  # Create a placeholder for the output
  textOutput(outputId = "output")
)

# Define server
server <- function(input, output, session) {
  # Train a logistic regression model
  model <- glm(dead_0_alive ~ Sex + Age.at.MRI + MGMT.status + Biopsy.prior.to.imaging + EOR, data = data, family = binomial)
  
  # Define a function to predict the probability of the binary outcome
  predict_prob <- function(Sex, Age.at.MRI, MGMT.status, Biopsy.prior.to.imaging, EOR) {
    newdata <- data.frame(Sex = Sex, Age.at.MRI = Age.at.MRI, MGMT.status = MGMT.status, Biopsy.prior.to.imaging = Biopsy.prior.to.imaging, EOR = EOR)
    predict(model, newdata, type = "response")
  }
  
  # Observe the submit button click and make the prediction
  observeEvent(input$submit, {
    # Get the input values
    Sex <- input$Sex
    Age.at.MRI <- input$Age.at.MRI
    MGMT.status <- input$MGMT.status
    Biopsy.prior.to.imaging <- input$Biopsy.prior.to.imaging
    EOR = input$EOR
    
    # Make the prediction
    prediction <- predict_prob(Sex, Age.at.MRI, MGMT.status, Biopsy.prior.to.imaging, EOR)
    
    # Render the output
    output$output <- renderText({
      if(prediction > 0.5){
        paste0("Negative outcome : ", prediction)
      }
      else{
        paste0("Positive outcome : ", prediction)
      }
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)

