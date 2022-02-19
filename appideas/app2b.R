# ....................................
# Begin Climate Equity Atlas Development
# Authors: Michele Claibourn, others
# Updated: February 5, 2022
# ....................................


# Phase 1a: select indicators, output plotly scatterplot, add sample header
#    to do: look into line.width warnings (and no trace specified warnings)
# Phase 1b: select localities to plot, add tabs for other components, add navbar

# Phase 2a: integrate bichoropleth; add base map selector; 
# Phase 2b: add popup info, fix legend! add setview to force zoom;
#    replace locality fips with names, replace varnames with good names,
#    add variable descriptions to sidebar
#    to do? add layering points (parks, schools, food retailers)

# Phase 3: output for tercile graph

# To do
#    update meta (short description on front, notes on new tab)
#    add block group data and selector for geography (in fluid row 2)
#    get better header logo/image; customize theming/color/aesthetic elements
#    add documentation info, about info, front page narrative for interpretation?;
#    select and add all variables (update google sheet, meta)
#    ?add county or block level data?
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
library(leafem)
library(stringi)

load("data/cvl_dat.RData")
# data prepared in combine_data.R
# geo <- geo %>% select(locality, geoid, geometry) 

# df <- readRDS("data/cvl_data.RDS")
# varlist <- df %>% select(where(is.numeric), -pop) %>% names()

# create palette for use in bichoropleth palette function
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
           "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
           "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-3


# ....................................
# Define User Interface ----
ui <- navbarPage("Regional Climate Equity Atlas",

  ## indicator selectors and plots ----
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
                                choices = ind_choices_ct,
                                selected = ind_choices_ct$`Demographic & Social`["Estimated Population"]),
                    # variable definitions
                    strong(textOutput("ind1_name")),
                    textOutput("ind1_defn")
             ),

             # Place figures
             column(8,
                    tabsetPanel(type = "tabs",
                                tabPanel(title = 'Map', align = 'center',
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
                                choices = ind_choices_ct,
                                selected = ind_choices_ct$`Jobs & Income`["Median Household Income"]),
                    # variable definitions
                    strong(textOutput("ind2_name")),
                    textOutput("ind2_defn")
             )
           ),

           tags$hr(),

           ## county/map/geography selector ----
           fluidRow(
             
             # base map selector
             column(3,
                    radioButtons(inputId = "base_map",
                                 label = h4("Select a Base Map"),
                                 choices = c("Minimal" = "CartoDB.Positron",
                                             "Detailed" = "OpenStreetMap.Mapnik"),
                                 inline = TRUE)
             ),
             
             # locality selector
             column(5,
                    checkboxGroupInput(inputId = "locality",
                                       label = h4("Select Localities"),
                                       choices = c("Albemarle" = "003", 
                                                   "Charlottesvile" = "540",
                                                   "Fluvanna" = "065",
                                                   "Greene" = "079",
                                                   "Lousia" = "109",
                                                   "Nelson" = "125"),
                                       selected = c("003", "540", "065",
                                                    "079", "109", "125"),
                                       inline = TRUE)
             )

           )

           ),
  
  ## information navbars ----
  tabPanel("Documentation"),
  tabPanel("About"),

  singleton(tags$head(tags$script(src = "message-handler.js")))

)


# ....................................
# Define Server Logic ----
server <- function(input, output, session) {

  ## define data ----
  # data <- reactive({
  #   df <- df %>%
  #     dplyr::select(x = !!sym(input$indicator1),
  #                   y = !!sym(input$indicator2),
  #                   locality, countyname, tract, geoid,
  #                   pop = pop) %>%
  #     dplyr::filter(locality %in% input$locality)
  # })

  geo_data <- reactive({
    geo <- geo %>%
      dplyr::select(x = !!sym(input$indicator1),
                    y = !!sym(input$indicator2),
                    locality, countyname, tract, geoid,
                    pop = pop) %>%
      dplyr::filter(locality %in% input$locality)
  })

  ## output scatterplot ----
  output$scatterplot <- renderPlotly({
    
    d <- st_drop_geometry(geo_data())
    xhist <- plot_ly(data = d, x = ~x,
                     type = "histogram", nbinsx = 20,
                     alpha =.75, color = I("grey")) %>%
      layout(yaxis = list(showgrid = FALSE,
                          showticklabels = FALSE),
             xaxis = list(showticklabels = FALSE))

    yhist <- plot_ly(data = d, y = ~y,
                     type = "histogram", nbinsx = 20,
                     alpha = .75, color = I("grey")) %>%
      layout(xaxis = list(showgrid = FALSE,
                          showticklabels = TRUE),
             yaxis = list(showticklabels = FALSE))

    xyscatter <- plot_ly(data = d, x = ~x, y = ~y,
                         type = "scatter",
                         mode = "markers",
                         size = ~pop, sizes = c(1, 500),
                         color = ~countyname, colors = "Dark2",
                         alpha = .75,
                         text = paste0("Locality: ", d$countyname, "<br>",
                                       "Census tract: ", d$tract, "<br>",
                                       "Population: ", d$pop, "<br>",
                                       attr(d$x, "goodname"), ": ", d$x, "<br>",
                                       attr(d$y, "goodname"), ": ", d$y, "<br>"),
                         hoverinfo = "text") %>%
      layout(xaxis = list(title = attr(d$x, "goodname"), showticklabels = TRUE),
             yaxis = list(title = attr(d$y, "goodname"), showticklabels = TRUE),
             legend = list(orientation = "h", x = 0, y = -0.2))

    subplot(xhist, plotly_empty(), xyscatter, yhist,
            nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
            shareX = TRUE, shareY = TRUE) %>%
      style(showlegend = FALSE, traces = c(1,9)) %>%  # remove hist symbols from legend
      layout(xaxis = list(showgrid = TRUE),
             yaxis2 = list(showgrid = TRUE))

  })

  ## output map ----
  #build static parts of map, and display initial outline of region
  output$leaf <- renderLeaflet({
    leaflet() %>% addProviderTiles(input$base_map) %>%
      addPolygons(data = geo, color = 'grey', opacity = 0) %>%
      setView(lng = -78.47668, lat = 38.02931, zoom = 9) %>%
      addLogo('bivariate_legend_static.svg', src = "remote",
              position = "topleft", width = 100, height = 100, alpha = 0.8)
  })
  # reactive function to detect when variable 1, variable 2, or locality selection changes
  listen_closely <- reactive({
    list(input$indicator1,input$indicator2, input$locality)
  })
  # when a variable or locality selection is changed, render the appropriate bichoropleth without losing the legend
  observeEvent(listen_closely(), {
    # `if()` check below will be expanded to check for all map-breaking variables
    if (input$indicator1 %in% 'indigE' | input$indicator2 %in% 'indigE') {
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("One of your selected variables cannot be",
                                                 " rendered in the map. This is usually because",
                                                 " there isn't enough variation in the variable",
                                                 " to break its values up into meaningful categories"))
    } else {
      # to_map <- left_join(geo_data(), data(), by = c('locality',  'geoid'))
      # to_map <- merge(data(), geo_data(), by = 'geoid')
      to_map <- bi_class(geo_data(), x = x, y = y, style = "quantile", dim = 3)
      to_map <- st_transform(st_as_sf(to_map), 4326)
      to_map$var1_tercile <- stri_extract(to_map$bi_class, regex = '^\\d{1}(?=-\\d)')
      to_map$var2_tercile <- stri_extract(to_map$bi_class, regex = '(?<=\\d-)\\d{1}$')
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
                    popup = paste0("Locality: ", to_map$countyname, ", ", to_map$tract, "<br>",
                                   attr(to_map$x, "goodname"), ": ", to_map$x,  "<br>",
                                   " ", "category (1-3): ", to_map$var1_tercile, "<br>",
                                   attr(to_map$y, "goodname"), ": ", to_map$y, "<br>",
                                   " ", "category (1-3): ", to_map$var2_tercile))
    }
  })

## indicator info ----
# indicator 1 info
output$ind1_name <- renderText({
    attr(geo_data()$x, "goodname")
  })

  # output indicator 1 description, for Source & Definition box
  output$ind1_defn <- renderText({
    attr(geo_data()$x, "description")
  })

  # indicator 2 info
  output$ind2_name <- renderText({
    attr(geo_data()$y, "goodname")
  })

  # output indicator 2 description, for Source & Definition box
  output$ind2_defn <- renderText({
    attr(geo_data()$y, "description")
  })

}


# Run the application ----
shinyApp(ui = ui, server = server)