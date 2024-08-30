install.packages("shiny")
install.packages("randomForest")
library(shiny)
library(randomForest)

# Load the data
housing <- read.csv("housing.csv")

# Load the saved models
random_forest_model <- readRDS("random_forest_model.rds")
linear_model <- readRDS("linear_model.rds")

# Ensure the levels of the factor are the same as in the training data
housing$ocean_proximity <- factor(housing$ocean_proximity)

# UI
ui <- fluidPage(
  titlePanel("California Housing Prices Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model_choice", "Select Model:", 
                  choices = c("Random Forest", "Linear Regression")),
      numericInput("longitude", "Longitude:", value = -122),
      numericInput("latitude", "Latitude:", value = 37),
      numericInput("housing_median_age", "Housing Median Age:", value = 29),
      numericInput("total_rooms", "Total Rooms:", value = 2000),
      numericInput("total_bedrooms", "Total Bedrooms:", value = 400),
      numericInput("population", "Population:", value = 1500),
      numericInput("households", "Households:", value = 500),
      numericInput("median_income", "Median Income:", value = 3.5),
      selectInput("ocean_proximity", "Ocean Proximity:",
                  choices = levels(housing$ocean_proximity)),
      actionButton("predict_button", "Predict")
    ),
    mainPanel(
      plotOutput("model_plot"),
      textOutput("prediction_output")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Prediction
  output$prediction_output <- renderText({
    req(input$predict_button)
    
    isolate({
      # Prepare input data
      input_data <- data.frame(
        longitude = input$longitude,
        latitude = input$latitude,
        housing_median_age = input$housing_median_age,
        total_rooms = input$total_rooms,
        total_bedrooms = input$total_bedrooms,
        population = input$population,
        households = input$households,
        median_income = input$median_income,
        ocean_proximity = factor(input$ocean_proximity, levels = levels(housing$ocean_proximity))
      )
      
      # Make prediction based on selected model
      prediction <- if(input$model_choice == "Random Forest") {
        predict(random_forest_model, newdata = input_data)
      } else {
        predict(linear_model, newdata = input_data)
      }
      
      # Display prediction
      paste("The predicted median house value is $", round(prediction, 2), ".")
    })
  })
  
  # Plot output
  output$model_plot <- renderPlot({
    # For illustration purposes, you can plot any relevant data here
    # For example, a scatter plot of latitude vs. longitude
    plot(housing$longitude, housing$latitude, pch = 20, col = "blue", 
         xlab = "Longitude", ylab = "Latitude", main = "California Housing Prices")
    grid()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
