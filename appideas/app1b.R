# ....................................
# Begin Climate Equity Atlas Development
# Authors: Michele Claibourn, others
# Updated: February 5, 2022
# ....................................


# Phase 1a: select indicators, ouptput plotly scatterplot, add sample header
#    to do: look into line.width warnings
# Phase 1b: select localities to plot, add tabs for other components, add navbar
#    to do: add select/unselect all button

# Phase 1c:
#    add varname improvements, locality/tract names (use google sheet, meta names) (app1c)
#    add reactive variable information to information tab (use google sheet, meta info) (app1c)
#    add figure title/captions/information (code and google sheet, meta names/info) (app1c)

# To do anytime
#    add data and selector for geography ((county?, tract, block group, (block?)) (in fluid row 2)
#    start thinking about theme, aesthetic elements

# Phase 2: output for tercile graph

# Phase 3: output bichoropleth 
#    add selector for base map (in fluid row 2)
#    add layering points (parks, schools, food retailers?)

# Phase 4: add component-wise helper information, make aesthetic improvements, programming improvements 
#   add documentation info, about info; come up with better header image
#   consider pulling out/chunking up and source some stuff...

# Phase 5: select and add all variables (update google sheet, meta)


# ....................................
# Load libraries and data
library(shiny)
library(tidyverse)
library(plotly)

df <- readRDS("data/cvl_data.RDS")
# cvl_data.RDS prepared in combine_data.R

# list of variables for selections
varlist <- df %>% select(where(is.numeric)) %>% names() 


# ....................................
# Define User Interface ----
ui <- fluidPage(
  
  # Application title
  titlePanel(div(column(width = 3, h2("Regional Climate Equity Atlas", 
                                      style="background-color:#737373; color:White; padding: 20px;")),
                 column(width = 9, tags$img(src = "ec_climate_logo.png", height='200'))),
             windowTitle="Climate Equity Atlas"),
  
  # trying to force some division between a potential header image and app
  tags$hr(style="border-top: 1px solid #000000;"),
  HTML("<br>", "<br>", "<br>", "<br>"),
  
  # start initial tab: indicator selectors and main plot
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
           tabsetPanel(
             tabPanel(title = "Scatterplot",
               plotlyOutput(outputId = "scatterplot")
               ),
             tabPanel(title = "Bichoropleth"),
             tabPanel(title = "Terciles"),
             tabPanel(title = "Indicators")
           )
           ),
    
    # Sidebar for indicator 2
    column(2,
           selectInput(inputId = "indicator2",
                       label = h4("Select Y-Axis"),
                       choices = varlist,
                       selected = varlist[2])
    )
  ),
  
  tags$hr(),
  
  # start second row: county selector (geography selector; base map selector)
  fluidRow(
    
    column(4, 
           checkboxGroupInput(inputId = "locality",
                       label = h4("Select Localities"),
                       choices = unique(df$locality),
                       selected = unique(df$locality),
                       inline = TRUE)
    )
    
  )
  
)


# ....................................
# Define Server Logic ----
server <- function(input, output) {
  
  # define data
  data <- reactive({
    df <- df %>% 
      dplyr::filter(locality %in% input$locality) %>% 
      dplyr::select(x = !!sym(input$indicator1), 
                    y = !!sym(input$indicator2), 
                    locality = locality,
                    pop = totalpopE,
                    tract = tract)
  })
  
  # produce scatterplot
  output$scatterplot <- renderPlotly({
    
    xhist <- plot_ly(data = data(), x = ~x,
                     type = 'histogram', nbinsx = 20,
                     alpha =.75, color = I("grey")) %>%
      layout(yaxis = list(showgrid = FALSE,
                          showticklabels = FALSE),
             xaxis = list(showticklabels = FALSE))
    
    yhist <- plot_ly(data = data(), y = ~y, 
                     type = 'histogram', nbinsx = 20, 
                     alpha = .75, color = I("grey")) %>%
      layout(xaxis = list(showgrid = FALSE,
                          showticklabels = FALSE),
             yaxis = list(showticklabels = FALSE))
    
    xyscatter <- plot_ly(data = data(), x = ~x, y = ~y,
                         type = 'scatter',
                         size = ~pop, sizes = c(1, 500),
                         color = ~locality, colors = "Dark2",
                         alpha = .75,
                         text = paste0("County: ", data()$locality, "<br>",
                                       "Census tract: ", data()$tract, "<br>",
                                       "Population: ", data()$pop, "<br>",
                                       input$indicator1, ": ", data()$x, "<br>",
                                       input$indicator2, ": ", data()$y, "<br>"),
                         hoverinfo = "text") %>%
      layout(xaxis = list(title = input$indicator1),
             yaxis = list(title = input$indicator2),
             legend = list(orientation = "h", x = 0, y = -0.2))
    
    subplot(xhist, plotly_empty(), xyscatter, yhist,
            nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
            shareX = TRUE, shareY = TRUE) %>%
      style(showlegend = FALSE, traces = c(1,9)) %>%  # remove hist symbols from legend
      layout(xaxis = list(showgrid = TRUE),
             yaxis2 = list(showgrid = TRUE))
    
  })
}


# Run the application ----
shinyApp(ui = ui, server = server)