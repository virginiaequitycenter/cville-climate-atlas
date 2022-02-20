# ....................................
# Begin Climate Equity Atlas Development
# Authors: Michele Claibourn, others
# Updated: February 5, 2022
#          2022-02-20 jacob-gg
# ....................................

# some remaining issues (2022-02-20):
#   in the scatterplot, when a locality is turned off, it's replaced in the plotly legend by "trace [n]"
#   in the tercile plot, the x and y labels aren't the clean names (trying the "goodnames" attribute didn't happen to work for me)
#   this warning appears every time the app is run, although it's unclear if whatever it refers to is a real issue for us:
#         "Warning in classInt::classIntervals(bins_y, n = dim, style = "quantile"): var has missing values, omitted in finding classes"

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

# no-go variables for mapping
cant_map <- c('indigE', 'othraceE', 'bbmax_up', 'HWAV_AFREQ', 'RFLD_AFREQ')
cant_map_message <- c("One of your selected variables cannot be rendered in the map or in the tercile plot. This is usually because there isn't enough variation in the variable to break its values up into meaningful categories")

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
                    textOutput("ind1_defn"),
                    textOutput("ind1_source")

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
                                tabPanel(title = "Terciles",
                                          plotOutput(outputId = 'tercile_plot'),
                                tabPanel(title = "Variable Information",
                                         strong(textOutput("var1_name")),
                                         textOutput("var1_abt"),
                                         textOutput("var1_source"),
                                         tags$br(),
                                         strong(textOutput("var2_name")),
                                         textOutput("var2_abt"),
                                         textOutput("var2_source"))
                                )
                    ),

             # Sidebar for indicator 2
             column(2,
                    selectInput(inputId = "indicator2",
                                label = h4("Select Variable 2 (Y)"),
                                choices = ind_choices_ct,
                                selected = ind_choices_ct$`Jobs & Income`["Median Household Income"]),
                    # variable definitions
                    textOutput("ind2_defn"),
                    textOutput("ind2_source")

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

  geo_data <- reactive({
    if (input$indicator1 == input$indicator2) {
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("Please make sure that you've selected two different variables."))
    } else {
    geo <- geo %>%
      dplyr::select(x = !!sym(input$indicator1),
                    y = !!sym(input$indicator2),
                    locality, countyname, tract, geoid,
                    pop = pop) %>%
      dplyr::filter(locality %in% input$locality)
    }
  })

  ## output scatterplot ----
  output$scatterplot <- renderPlotly({
    if (input$indicator1 == input$indicator2) {
      plotly_empty()
    } else {
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
    }
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
    if (input$indicator1 == input$indicator2) {
      leafletProxy('leaf') %>% clearShapes()
    } else if (input$indicator1 %in% cant_map | input$indicator2 %in% cant_map) {
      session$sendCustomMessage(type = 'testmessage', message = cant_map_message)
      leafletProxy('leaf') %>% clearShapes()
    } else {
      to_map <- bi_class(geo_data(), x = x, y = y, style = "quantile", dim = 3)
      to_map$var1_tercile <- stri_extract(to_map$bi_class, regex = '^\\d{1}(?=-\\d)')
      to_map$var1_tercile_cat <- ifelse(to_map$var1_tercile == 1, 'Low', ifelse(to_map$var1_tercile == 2, 'Medium', ifelse(to_map$var1_tercile == 3, 'High', '')))
      to_map$var2_tercile <- stri_extract(to_map$bi_class, regex = '(?<=\\d-)\\d{1}$')
      to_map$var2_tercile_cat <- ifelse(to_map$var2_tercile == 1, 'Low', ifelse(to_map$var2_tercile == 2, 'Medium', ifelse(to_map$var2_tercile == 3, 'High', '')))
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
                                   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Relative to other tracts: ", to_map$var1_tercile_cat, "<br>",
                                   attr(to_map$y, "goodname"), ": ", to_map$y, "<br>",
                                   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Relative to other tracts: ", to_map$var2_tercile_cat))
    }
  })

  ## output tercile plot ----
  output$tercile_plot <- renderPlot({
    if (input$indicator1 %in% cant_map | input$indicator2 %in% cant_map | input$indicator1 == input$indicator2) {
      ""
    } else {
      to_tercile <- bi_class(geo_data(), x = x, y = y, style = "quantile", dim = 3)
      to_tercile$var1_tercile <- stri_extract(to_tercile$bi_class, regex = '^\\d{1}(?=-\\d)')
      to_tercile <- to_tercile %>% group_by(var1_tercile) %>% summarize(var2_mean = mean(y, na.rm = T))
      to_tercile <- to_tercile[to_tercile$var1_tercile %in% 1:3, ]
      ggplot(to_tercile, aes(x = var1_tercile, y = var2_mean, fill = var1_tercile)) +
        geom_bar(stat = 'identity') + scale_fill_manual(values = c('cornflowerblue', 'salmon', 'darkolivegreen3')) +
        scale_x_discrete(labels = paste0(c('Lowest ', 'Middle ', 'Highest '), 'third of tracts')) +
        theme(legend.position = 'none')
    }
  })

  # output indicator 1 description
output$ind1_defn <- renderText({
  attr(geo_data()$x, "description")
})

# output indicator 1 source
output$ind1_source <- renderText({
  attr(geo_data()$x, "source")
})

# output indicator 2 description
output$ind2_defn <- renderText({
  attr(geo_data()$y, "description")
})

# output indicator 2 description
output$ind2_source <- renderText({
  attr(geo_data()$y, "source")
})

# detailed var info
output$var1_name <- renderText({
  attr(geo_data()$x, "goodname")
})

output$var1_abt <- renderText({
  attr(geo_data()$x, "about")
})

output$var1_source <- renderText({
  attr(geo_data()$x, "source")
})

output$var2_name <- renderText({
  attr(geo_data()$y, "goodname")
})

output$var2_abt <- renderText({
  attr(geo_data()$y, "about")
})

output$var2_source <- renderText({
  attr(geo_data()$y, "source")
})

# Run the application ----
shinyApp(ui = ui, server = server)
