# Load necessary libraries
library(tidyverse)
library(shiny)

# Read and prepare the dataset
customer_churn_prep_tbl <- read_csv("data/BankChurners.csv")

# Run the shiny app
shinyApp(
  ui = fluidPage(
    titlePanel("Bank Customer Churn Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("x_axis", "X-axis:", choices = colnames(cust_churn_tbl)),
        selectInput("y_axis", "Y-axis:", choices = colnames(cust_churn_tbl)),
        selectInput("color", "Color By:", choices = c('None', colnames(cust_churn_tbl)))
      ),
      
      mainPanel(
        plotOutput("scatterPlot")
      )
    )
  ),
  
  server = function(input, output) {
    output$scatterPlot <- renderPlot({
      ggplot(cust_churn_tbl, aes_string(x = input$x_axis, y = input$y_axis)) +
        geom_point(aes_string(color = input$color)) +
        theme_minimal() +
        labs(title = "Scatter Plot", x = input$x_axis, y = input$y_axis)
    })
  }
)
