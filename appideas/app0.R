# ....................................
# Begin Climate Equity Atlas Development
# Authors: Michele Claibourn, others
# Updated: February 3, 2022
#          February 8, 2022 - jacob-gg
#          February 9, 2022 - jacob-gg
# ....................................


# Phase 1: select indicators, output basic ggplot scatterplot


# ....................................
# Load libraries and data
library(shiny)
library(tidyverse)
library(plotly)
library(biscale)
library(sf)
library(magick)

df <- readRDS("data/cvl_data.RDS")
# cvl_data.RDS prepared in combine_data.R

# list of variables for selections
varlist <- df %>% select(where(is.numeric)) %>% names()

# read geometry
geo <- readRDS('data/cvl_data_geo.RDS') %>% select(locality, geometry)

# create palette for use in bichoropleth palette function
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
           "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
           "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-3

# create palette object for logo generation
bipal_logo <- tibble("3-3" = "#3b4994", "2-3" = "#8c62aa", "1-3" = "#be64ac",
                     "3-2" = "#5698b9", "2-2" = "#a5add3", "1-2" = "#dfd0d6",
                     "3-1" = "#5ac8c8", "2-1" = "#ace4e4", "1-1" = "#e8e8e8") %>%
  gather("group", "fill")

# ....................................
# Define User Interface ----
ui <- fluidPage(
  # Application title
  titlePanel("Charlottesville Regional Climate Equity"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data',
                       fluidRow(
                         # Sidebar for indicator 1
                         column(2,
                                selectInput(inputId = "indicator1",
                                            label = h4("Select X-Axis"),
                                            choices = varlist,
                                            selected = varlist[1])),

                         # Place scatterplot
                         column(8,
                                plotOutput(outputId = "scatterplot")),

                         # Sidebar for indicator 2
                         column(2,
                                selectInput(inputId = "indicator2",
                                            label = h4("Select Y-Axis"),
                                            choices = varlist,
                                            selected = varlist[2]))
                         )),
              tabPanel('Map', plotlyOutput('map', width = '100%', height = '100%')))
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
                    pop = totalpopE)
  })

  # produce scatterplot
  output$scatterplot <- renderPlot({
    ggplot(data(), aes(x = x, y = y, color = locality, size = pop)) +
      geom_point()
  })

  # rebuild map whenever input variables are updated
  output$map <- renderPlotly({
    # generate and save new legend
    if (file.exists('www/bivariate_legend.png')) {file.remove('www/bivariate_legend.png')}
    bipal_ <- bipal_logo %>%
      separate(group, into = c(input$indicator1, input$indicator2), sep = "-") %>%
      mutate(x = as.integer(eval(sym(input$indicator1))),
             y = as.integer(eval(sym(input$indicator2))))
    legend_ <- ggplot(bipal_, mapping = aes(x = x, y = y, fill = fill)) +
      geom_tile() + scale_fill_identity() + theme_void() + coord_fixed() +
      labs(x = paste0("Higher ", input$indicator1, " \u2192"), # Check if these are right in terms of y/x axes
           y = paste0("Higher ", input$indicator2, " \u2192")) +
      theme(axis.title = element_text(size = 6), axis.title.y = element_text(angle = 90))
    ggsave(plot = legend_, filename = 'www/bivariate_legend.png', width = 1, height = 1, dpi = 320)

    # merge reactive data with geometry, get bi_class values, and generate palette for Leaflet
    to_map <- merge(data(), geo, by = 'locality')
    to_map <- bi_class(to_map, x = x, y = y, style = "quantile", dim = 3)
    to_map <- st_transform(st_as_sf(to_map), 4326)

    # make plot and pass to plotly
    mm <- ggplot(to_map) +
      geom_sf(aes(geometry = geometry, fill = bi_class), color = 'white', size = .1, show.legend = F) +
      theme_void() + labs(title = '') + theme(legend.position='none')
    mm <- ggplotly(mm)

    # embed reactive legend in the plot
    mm <- mm %>% layout(
      images = list(
        list(
          source =  raster2uri(as.raster(image_read('www/bivariate_legend.png'))),
          xref = "x domain",
          yref = "y domain",
          x = .7,
          y = .95,
          sizex = .25,
          sizey = .25,
          opacity = 1,
          layer = "below"
        )), margin = list(l = 50, r = 50, b = 50, t = 50),
      title = paste0(input$indicator1, ' x ', input$indicator2),
      xaxis = list(title = input$indicator1),
      yaxis = list(title = input$indicator2))
    mm
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)