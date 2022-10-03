library(shiny)
library(tidyverse)
library(plotly)

#######################
# Load Data
  gapminder <- read_csv("gapminder_clean.csv") %>%
    as_tibble()
# options
  years <- gapminder$Year %>% unique()
  gap_col <- names(gapminder)
  data_type <- gap_col[c(4:18,20)]
#######################

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
  # App title ----
  titlePanel("Interactive plotting of the gapminder data set, by Sam Hart"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose the year ----
      
      selectInput("year", "Choose a year:",
                  choices = years),
      
      br(),
      
      # Input: Select variable for X axis ----
      
      selectInput("x_ax", "Select variable for X axis:",
                  choices = data_type),      
      
      
      radioButtons("logX", "Log or linear scale (X axis):",
                   c("Log" = "log",
                     "Linear" = "lin")),
      
      br(),
      
      # Input: Select variable for Y axis ----
      
      selectInput("y_ax", "Select variable for Y axis:",
                  choices = data_type),       
      
      # Input: Select variable for Y axis ----
      radioButtons("logY", "Log or linear scale (Y axis):",
                   c("Log" = "log",
                     "Linear" = "lin"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotlyOutput(outputId = "gapPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$gapPlot <- renderPlotly({
    
    plot1 <- gapminder %>%
      filter(Year == input$year) %>%
      ggplot(aes(x = .data[[input$x_ax]], y = .data[[input$y_ax]], size = pop, color = continent, text = `Country Name`)) +
      geom_point()
    
    if(input$logX == "log"){
      plot1 <-  plot1 +
        scale_x_log10()
    }
    
    if(input$logY == "log"){
      plot1 <-  plot1 +
        scale_y_log10()
    }
    
    ggplotly(plot1)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
