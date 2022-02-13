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
library(leaflet)
library(biscale)
library(sf)
# library(leafem)

df <- readRDS("data/cvl_data.RDS")
geo <- readRDS('data/cvl_data_geo.RDS') %>% select(locality, geoid, geometry)
varlist <- df %>% select(where(is.numeric), -pop) %>% names()
# cvl_data.RDS and cvl_data_geo.RDS prepared in combine_data.R

# create palette for use in bichoropleth palette function
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
           "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
           "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-3


# ....................................
# Define User Interface ----
ui <- navbarPage("Regional Climate Equity Atlas",
  
  # start initial tab: indicator selectors and main plot
  tabPanel("Main",
           # App title ----
           titlePanel(div(
             windowTitle = "Cville Climate Equity Atlas",
             img(src = "ec_climate_logo.png", height='200', class = "bg"),
           )),
           
           tags$br(),
           
           fluidRow(
             
             # Sidebar for indicator 1
             column(2, 
                    selectInput(inputId = "indicator1",
                                label = h4("Select Variable 1 (X)"),
                                choices = varlist,
                                selected = varlist[1])
             ),
             
             # Place figures
             column(8, 
                    tabsetPanel(type = "tabs",
                      tabPanel(title = "Scatterplot",
                               plotlyOutput(outputId = "scatterplot")
                      ),
                      tabPanel('Map', leafletOutput(outputId = 'leaf', width = '100%'),
                               div(style="text-align:center",
                                   img(src = "bivariate_legend_static.svg", height='200', class = "bg", align="center"))
                               ),
                      tabPanel(title = "Terciles"),
                      tabPanel(title = "Indicators")
                    )
             ),
             
             # Sidebar for indicator 2
             column(2,
                    selectInput(inputId = "indicator2",
                                label = h4("Select Variable 2 (Y)"),
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
           
           ),
  tabPanel("Documentation"),
  tabPanel("About")
  
)


# ....................................
# Define Server Logic ----
server <- function(input, output) {
  
  # define data
  data <- reactive({
    df <- df %>%
      dplyr::select(x = !!sym(input$indicator1),
                    y = !!sym(input$indicator2),
                    locality, tract, geoid,
                    pop = pop) %>% 
      dplyr::filter(locality %in% input$locality)
  })
  
  geo_data <- reactive({
    geo <- geo %>% 
      dplyr::filter(locality %in% input$locality)
  })
  
  # output scatterplot
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
  
  # output map; rebuild when input variables are updated
  output$leaf <- renderLeaflet({
    
    # create biclass
    to_map <- left_join(geo_data(), data(), by = c('locality',  'geoid'))
    to_map <- bi_class(to_map, x = x, y = y, style = "quantile", dim = 3)
    to_map <- st_transform(st_as_sf(to_map), 4326)
    factpal <- colorFactor(bipal, domain = to_map$bi_class)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addPolygons(data = to_map,
                  fillColor = ~factpal(bi_class),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 2,
                    fillOpacity = 0.8,
                    bringToFront = T),
                  popup = paste0(input$indicator1, ": ", data()$x,  "<br>",
                                 input$indicator2, ": ", data()$y, "<br>",
                                 paste0(data()$locality, ", ", data()$tract))
                  )
  })
  
}


# Run the application ----
shinyApp(ui = ui, server = server)