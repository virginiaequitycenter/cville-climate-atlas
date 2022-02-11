# ....................................
# Begin Climate Equity Atlas Development
# Authors: Michele Claibourn, others
# Updated: February 3, 2022
# ....................................


# Phase 1: select indicators, output basic ggplot scatterplot


# ....................................
# Load libraries and data
library(shiny)
library(tidyverse)
library(plotly)

df <- readRDS('data/cvl_data_geo.RDS') 
# cvl_data_geo.RDS prepared in combine_data.R

# list of variables for selections
varlist <- geo %>% select(where(is.numeric), -pop) %>% names()


# ....................................
# Define User Interface ----
ui <- fluidPage(
  
  # Application title
  titlePanel("Charlottesville Regional Climate Equity"),
  
  fluidRow(
    
    # Sidebar for indicator 1
    column(2, 
           selectInput(inputId = "indicator1",
                       label = h4("Select X-Axis"),
                       choices = varlist,
                       selected = varlist[1])
    ),
    
    # Place scatterplot
    column(8, 
           plotOutput(outputId = "scatterplot")
    ),
    
    # Sidebar for indicator 2
    column(2,
           selectInput(inputId = "indicator2",
                       label = h4("Select Y-Axis"),
                       choices = varlist,
                       selected = varlist[2])
    )
  )
  
)


# ....................................
# Define Server Logic ----
server <- function(input, output) {
  
  # define data
  data <- reactive({
    df <- df %>% 
      dplyr::select(x = !!sym(input$indicator1), 
                    y = !!sym(input$indicator2), 
                    locality = locality,
                    pop = pop)
  })
  
  # produce scatterplot
  output$scatterplot <- renderPlot({
    
    ggplot(data(), aes(x = x, y = y, color = locality, size = pop)) +
      geom_point()
    
  })
}


# Run the application ----
shinyApp(ui = ui, server = server)