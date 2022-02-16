# ....................................
# Begin Climate Equity Atlas Development
# Authors: Michele Claibourn, others
# Updated: February 3, 2022
#          February 8, 2022 - jacob-gg
#          February 14, 2022 - jacob-gg
#          February 16, 2022 - jacob-gg
# ....................................

# Phase 1: select indicators, output basic ggplot scatterplot

# ....................................
# Load libraries and data
library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(biscale)
library(sf)
library(leafem)

# read data
df <- readRDS('data/cvl_data.RDS')
geo_df <- readRDS('data/cvl_data_geo.RDS') %>% st_transform(4326)

# list of variables for selections
varlist <- df %>% select(where(is.numeric)) %>% names()

# create palette for use in bichoropleth palette function
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
           "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
           "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-3

# generate and save legend svg
#   (not needed each time the app is run; code left here in case we change strategies
#   and want to try and dynamically rerender the legend each time a variable is updated)
# palette object for legend generation
# bipal_logo <- tibble("3-3" = "#3b4994", "2-3" = "#8c62aa", "1-3" = "#be64ac",
#                      "3-2" = "#5698b9", "2-2" = "#a5add3", "1-2" = "#dfd0d6",
#                      "3-1" = "#5ac8c8", "2-1" = "#ace4e4", "1-1" = "#e8e8e8") %>%
#   gather("group", "fill")
# turn into plot and save
# bipal_ <- bipal_logo %>%
#   separate(group, into = c('x', 'y'), sep = "-") %>%
#   mutate(x = as.integer(x),
#          y = as.integer(y))
# legend_ <- ggplot() +
#   geom_tile(data = bipal_, mapping = aes(x = x, y = y, fill = fill)) +
#   scale_fill_identity() + theme_void() + coord_fixed() +
#   labs(x = paste0("Higher X variable \u2192"), # Check if these are right in terms of y/x axes
#        y = paste0("Higher Y variable \u2192")) +
#   theme(axis.title = element_text(size = 6), axis.title.y = element_text(angle = 90))
# ggsave(plot = legend_, filename = 'www/bivariate_legend.svg', width = 1, height = 1)

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
                       selected = varlist[1])),
    column(8, align = 'center',
           tabsetPanel(type = 'tabs',
           tabPanel('Data', plotOutput(outputId = "scatterplot")),
           tabPanel('Map', actionButton("rend_map", "Render map"), leafletOutput(outputId = 'leaf', width = '100%')))),

    # Sidebar for indicator 2
    column(2,
           selectInput(inputId = "indicator2",
                       label = h4("Select Y-Axis"),
                       choices = varlist,
                       selected = varlist[2]))
  ),

  singleton(tags$head(tags$script(src = "message-handler.js")))

)


# ....................................
# Define Server Logic ----
server <- function(input, output, session) {

  # define data
  data <- reactive({
    df <- df %>%
      dplyr::select(x = !!sym(input$indicator1),
                    y = !!sym(input$indicator2),
                    locality = locality,
                    pop = totalpopE,
                    geoid = geoid)
  })

  # produce scatterplot
  output$scatterplot <- renderPlot({
    ggplot(data(), aes(x = x, y = y, color = locality, size = pop)) + geom_point()
  })

  # build static parts of map, and display initial outline of region
  output$leaf <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = geo_df, color = 'white', opacity = 0) %>%
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
    to_map <- merge(data(), geo_df, by = 'geoid')
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
                    bringToFront = T))
    }
  })

}

# Run the application ----
shinyApp(ui = ui, server = server)