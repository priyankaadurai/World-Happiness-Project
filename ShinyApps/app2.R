library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(countrycode)

whr <- read_csv("whr-2023.csv") %>%
  mutate(
    Region = countrycode(`Country name`,
                         origin = "country.name",
                         destination = "region")
  ) %>%
  filter(!is.na(Region))

# Sample UI
ui <- fluidPage(
  titlePanel("Q-Q Plot of Residuals by Region(full model)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Region", "Select a Region:",
                  choices = unique(whr$Region))
    ),
    
    mainPanel(
      plotOutput("qqPlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  region_data <- reactive({
    if (input$region != "None") {
      Region_data <- whr %>%
        filter(Region == input$Region)
    }
  })
  
  output$qqPlot <- renderPlot({
    # Subset data based on selected Region
    Region_data <- whr %>%
      filter(Region == input$Region)
    
    Region_data <- Region_data %>% select(-c(`Country name`,`year`,`Region`))
    
    lm_model <- lm(`Life Ladder` ~., data = Region_data)
    
    # Extract residuals
    # Create data frame from model
    model_data <- data.frame(
      Fitted = fitted(lm_model),
      Residuals = residuals(lm_model)
    )
    
    # Plot: residuals as points, fitted values as a line at y=0
    ggplot(model_data, aes(x = Fitted, y = Residuals)) +
      geom_point(color = "red", alpha = 0.6) +                 # residuals as dots
      geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +  # fitted line
      geom_smooth(method = "loess", se = FALSE, color = "green") + 
      labs(title = "Residuals vs Fitted Values",
           x = "Fitted Values",
           y = "Residuals") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)