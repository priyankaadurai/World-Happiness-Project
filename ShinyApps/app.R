library(shiny)
library(tidyverse)
library(countrycode)

whr <- read_csv("whr-2023.csv") %>%
  mutate(
    Region = countrycode(`Country name`,
                         origin = "country.name",
                         destination = "region")
  ) %>%
  filter(!is.na(Region))

yearly_avg <- whr %>%
  group_by(year) %>%
  summarize(Global = mean(`Life Ladder`, na.rm = TRUE))

ui <- fluidPage(
  titlePanel("Life Ladder Trends Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region",
        label = "Add Region to Compare:",
        choices = c("None", unique(whr$Region)),
        selected = "None"
      ),
      checkboxInput(
        inputId = "show_points",
        label = "Show individual country data",
        value = FALSE
      )
    ),
    
    mainPanel(
      plotOutput("trend_plot"),
      p("Global average shown in blue. Selected region average shown in red.")
    )
  )
)


server <- function(input, output) {
  
  # Reactive data for selected region
  region_data <- reactive({
    if (input$region != "None") {
      whr %>%
        filter(Region == input$region) %>%
        group_by(year) %>%
        summarize(Regional = mean(`Life Ladder`, na.rm = TRUE))
    }
  })
  
  output$trend_plot <- renderPlot({
    # Start with global average
    p <- ggplot(yearly_avg, aes(x = year, y = Global)) +
      geom_line(color = "blue", linewidth = 1.5) +
      labs(title = "Life Ladder Trends Over Time",
           x = "Year",
           y = "Average Life Ladder Score") +
      theme_minimal()
    
    
    if (input$region != "None") {
      p <- p + 
        geom_line(data = region_data(), 
                  aes(x = year, y = Regional),
                  color = "red", linewidth = 1.5)
    }
    
    
    if (input$show_points) {
      p <- p + 
        geom_point(data = whr,
                   aes(x = year, y = `Life Ladder`, group = Region),
                   alpha = 0.3, color = "gray50")
    }
    
    p
  })
}

shinyApp(ui = ui, server = server)