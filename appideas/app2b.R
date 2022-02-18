# ....................................
# Begin Climate Equity Atlas Development
# Authors: Michele Claibourn, others
# Updated: February 5, 2022
# ....................................


# Phase 1a: select indicators, output plotly scatterplot, add sample header
#    to do: look into line.width warnings (and no trace specified warnings)
# Phase 1b: select localities to plot, add tabs for other components, add navbar
#    to do? add select/unselect all button

# Phase 2a: integrate bichoropleth; add base map selector
#    to do: settle on legend solution
#    to do: can we make it default to zoom further in?
#    to do? add layering points (parks, schools, food retailers)

# Phase 3: output for tercile graph

# To do 
#    add varname improvements: (add meta as attribute to data)
#       locality/tract names (in selector, in hover, in popup)
#       good var names (in selector, in hover, in popup)
#       reactive variable definitions below var selector
#       ?figure title/captions/information? 

# To do 
#    add block group data and selector for geography (in fluid row 2)
#    get header logo/image; customize theming/color/aesthetic elements
#    add documentation info, about info; come up with better header image
#    select and add all variables (update google sheet, meta)
#    ?add county or block level?
#    ?add component-wise helper information?
# consider pulling out/chunking up and source some stuff...programming improvements


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
                                selected = varlist[1]),
                    # variable definitions
                    strong(textOutput("ind1_name")), 
                    textOutput("ind1_defn")
             ),
             
             # Place figures
             column(8, 
                    tabsetPanel(type = "tabs",
                                tabPanel('Map', actionButton("rend_map", "Render map"), 
                                         leafletOutput(outputId = 'leaf', width = '100%')
                                ),
                                tabPanel(title = "Scatterplot",
                                         plotlyOutput(outputId = "scatterplot")
                                         ),
                                tabPanel(title = "Terciles")
                                )
                    ),
             
             # Sidebar for indicator 2
             column(2,
                    selectInput(inputId = "indicator2",
                                label = h4("Select Variable 2 (Y)"),
                                choices = varlist,
                                selected = varlist[2]),
                    # variable definitions
                    strong(textOutput("ind2_name")), 
                    textOutput("ind2_defn")
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
             ),
             
            # base map selector
             column(4, 
                    radioButtons(inputId = "base_map",
                                 label = h4("Select a Base Map"),
                                 choices = c("Minimal" = "CartoDB.Positron",
                                             "Detailed" = "OpenStreetMap.Mapnik"),
                                 inline = TRUE)
             )
             
             
           )
           
           ),
  tabPanel("Documentation"),
  tabPanel("About"),
  
  singleton(tags$head(tags$script(src = "message-handler.js")))
  
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
                     type = "histogram", nbinsx = 20,
                     alpha =.75, color = I("grey")) %>%
      layout(yaxis = list(showgrid = FALSE,
                          showticklabels = FALSE),
             xaxis = list(showticklabels = FALSE))
    
    yhist <- plot_ly(data = data(), y = ~y, 
                     type = "histogram", nbinsx = 20, 
                     alpha = .75, color = I("grey")) %>%
      layout(xaxis = list(showgrid = FALSE,
                          showticklabels = FALSE),
             yaxis = list(showticklabels = FALSE))
    
    xyscatter <- plot_ly(data = data(), x = ~x, y = ~y,
                         type = "scatter",
                         mode = "markers",
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
  
  # build static parts of map, and display initial outline of region
  output$leaf <- renderLeaflet({
    leaflet() %>% addProviderTiles(input$base_map) %>%
      addPolygons(data = geo_data(), color = 'grey', opacity = 0) %>%
      setView(lng = -78.47668, lat = 38.02931, zoom = 9) %>% 
      addLogo('bivariate_legend.svg', src = "remote",
              position = "topleft", width = 100, height = 100, alpha = 0.8)
  })
  
  # when variables are selected and "Render map" is pressed, render the bichoropleth without losing the legend
  observeEvent(input$rend_map, {
    # `if()` check below will be expanded to check for all map-breaking variables
    if (input$indicator1 %in% 'indigE' | input$indicator2 %in% 'indigE') {
      session$sendCustomMessage(type = 'testmessage',
                                message = "One of your selected variables cannot be rendered in the choropleth (map); this is usually because there isn't enough variation in the variable to break its values up into meaningful categories")
    } else {
      to_map <- left_join(geo_data(), data(), by = c('locality',  'geoid'))
      to_map <- bi_class(to_map, x = x, y = y, style = "quantile", dim = 3)
      to_map <- st_transform(st_as_sf(to_map), 4326)
      factpal <- colorFactor(bipal, domain = to_map$bi_class)
      leafletProxy('leaf', data = to_map) %>% clearShapes() %>%
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
    }
  })
  
  
# indicator 1 info
output$ind1_name <- renderText({
    input$indicator1
#    attr(md()[[input$indicator1]], "goodname")
  })
  
  # output indicator 1 description, for Source & Definition box
  output$ind1_defn <- renderText({
    "add me"
#    attr(md()[[input$indicator1]], "about") 
  })
  
  # indicator 2 info
  output$ind2_name <- renderText({
    input$indicator2
#    attr(md()[[input$indicator2]], "goodname")
  })
  
  # output indicator 2 description, for Source & Definition box
  output$ind2_defn <- renderText({
    "add me"
#    attr(md()[[input$indicator2]], "about") 
  })
  
}


# Run the application ----
shinyApp(ui = ui, server = server)