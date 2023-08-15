# ....................................
# Begin Climate Equity Atlas Development
# Authors: Michele Claibourn, Jacob Goldstein-Greenwood, Lee LeBoeuf, Beth Mitchell
# Last updated: 2023-08-11 mpc, ll
# Last deployed: -
# ....................................

# ....................................
# Load libraries and data
library(shiny)
library(tidyverse)
library(plotly)
# library(ggthemes)
library(leaflet)
library(biscale) # for tercile plots
library(sf)
library(leafem)
library(stringi) # for tercile plots
library(knitr)
# library(htmltools)
library(DT)
library(bslib)

source("functions/utils.R")

load("data/cvl_dat.RData")
# data prepared in combine_data.R

# create palette for use in bichoropleth palette function
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
           "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
           "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-3

fewpal <- c("#7DC462", "#0D95D0", "#E72F52", "#774FA0", "#EFB743", "#D44627")
# fewpal <- c("#265dab", "#df5c24", "#c7b42e", "#059748", "#cb2027", "#9d722a")

# no-go variables for mapping
cant_map <- c('indigE', 'othraceE', 'bbmax_up', 'HWAV_AFREQ', 'RFLD_AFREQ', 'avg_prox_transit_rank')
cant_map_message <- c("One of your selected variables cannot be rendered in the map or in the tercile plot. This is usually because there isn't enough variation in the variable to break its values up into meaningful categories.")

# variables to create leaflet map bounding box
counties_geo <- st_transform(geo, 4326)
bbox <- st_bbox(counties_geo) %>% as.vector()
# ....................................
# Define User Interface ----

ui <- htmlTemplate(filename = "climate-app-template.html", main =
  navbarPage(title = img(src='ec_climate_logo.png',
                         height = 50,
                         alt = "Equity Center Climate Equity Atlas logo"),
            windowTitle = "Charlottesville Regional Climate Equity Dashboard",
            collapsible = TRUE,
            fluid = TRUE,
            theme = bs_theme(version = 5),
            ## indicator selectors and plots ----
            tabPanel(HTML("<center>DASHBOARD</center>"),
              fluidRow(
                column(
                  width = 12,
                  HTML("<center><h1>Charlottesville Regional Climate Equity Dashboard</h1>
                        <p>Visit 
                        <a href='https://virginiaequitycenter.github.io/picturing-climate-stories/' target='_blank'>Picturing Climate Justice: Data Stories</a>
                         for more explanation and analysis.</p></center>")
                )
              ), br(), # end fluidRow
              fluidRow(
                column(
                  width = 12,
                  cardComponent(
                    accordianComponent("intro", "Dashboard Instructions",
                                        "<p>Select any two measures from the dropdown menus below to see the relationship between the measures in census tracts across the Greater Charlottesville Region.</p>
                                       <p>Select the tabs below to see the relationship between the two selected measures in different ways: on a map, correlation plot, tercile plot, and data table.</p>",
                                        "intro-1", "intro-2")
                  )
                )
              ), br(), # end fluidRow
              fluidRow(
                column(
                  width = 4,
                    cardComponentSelect(
                    selectInput(inputId = "indicator1",
                                label = "Select Variable 1:",
                                choices = ind_demfirst_ct,
                                selected = ind_demfirst_ct$`Demographic & Social`["Estimated Population"]),
                    accordianComponentSource("ind1", "Show Selected Variable Definition & Source", textOutput("ind1_defn", inline = TRUE), textOutput("ind1_source", inline = TRUE),"var-def-1", "map-ind-1")
                    )
                ) %>% tagAppendAttributes(class="mb-3 mb-sm-0"),
                column(
                  width = 4,
                    cardComponentSelect(
                    selectInput(inputId = "indicator2",
                                label = "Select Variable 2:",
                                choices = ind_climfirst_ct,
                                selected = ind_climfirst_ct$`Climate Measures`["Average Land Surface Temperature"]),
                    accordianComponentSource("ind2", "Show Selected Variable Definition & Source", textOutput("ind2_defn", inline = TRUE),textOutput("ind2_source", inline = TRUE),"var-def-2", "map-ind-2")
                    )
                ) %>% tagAppendAttributes(class="mb-3 mb-sm-0"),
                column(
                  width = 4,
                  cardComponentSelectOne(
                  checkboxGroupInput(inputId = "locality",
                                    label = "Select Localities:",
                                    choices = c("Albemarle" = "003",
                                                "Charlottesvile" = "540",
                                                "Fluvanna" = "065",
                                                "Greene" = "079",
                                                "Louisa" = "109",
                                                "Nelson" = "125"),
                                    selected = c("003", "540", "065",
                                                  "079", "109", "125"),
                                    inline = TRUE)
                  # actionButton(inputId = "refresh", "Refresh Atlas") # removed and replaced with reset map button
                  )
                ) %>% tagAppendAttributes(class="mb-3 mb-sm-0")
      
              ), br(),  # end fluidRow
              fluidRow(
                column(
                  width = 12,
                  tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      title = 'Map',
                      icon = icon('map'),
                      h2(textOutput("maptitle", inline = TRUE)),
                      cardComponent(
                      accordianComponent("map-desc", "Map Instructions",
                                         "<p>This map shows how each census tract is ranked from Low to High on the measures selected above (Variable 1 & Variable 2). The legend in the upper left corner of the map provides a key for what each color represents.</p>
                                       <p>For example, if the values of both variables are Low, the census tract appears gray. When the value of Variable 1 is Low and the value of Variable 2 is High the track will be pink; when Variable 1 is High and Variable 2 is Low the tract will be green; When both variables have a High value, the tract appears dark blue.</p>
                                         <p>Click on the tracts to see the values for each measure and how the tract ranks (Low, Medium, High) relative to others in the selected region. Zoom in to see specific areas more closely; click the reset button on the map to see the full region.</p>",
                                         "mapdesc-1", "mapdesc-2")
                      ), br(),
                      leafletOutput(outputId = 'leaf', width = '100%', height = 600)
                    ),
                    tabPanel(
                      title = "Correlation",
                      icon = icon('chart-line'),
                      h2(textOutput("comptitle", inline = TRUE)),
                      cardComponent(
                      accordianComponent("comp-desc", "Correlation Plot Instructions",
                                         "<p>This plot shows the correlation, or relationship, between the two selected variables for the localities selected.</p>
                                       <p>Each census tract is represented by a circle, plotted by the values of the measures selected above. The size of each circle is based on the population of that tract so that tracts with more people appear larger and those with less people appear smaller. The color of the circle is based on the locality.</p>
                                         <p>The gray figures on the top and right show how frequently high and low values of the selected variables occur in the region; taller bars mean that range of values is more common.</p>",
                                         "compdesc-1", "compdesc-2")
                      ), br(),

                      # p("This plot shows the correlation, or relationship, between the two selected variables for the localities selected."), 
                      # p("Each census tract is represented by a circle, plotted by the values of the measures selected above. The size of each circle is based on the population of that tract so that tracts with more people appear larger and those with less people appear smaller. The color of the circle is based on the locality."),
                      # p("The gray figures on the top and right show how frequently high and low values of the selected variables occur in the region; taller bars mean that range of values is more common."),
                      plotlyOutput(outputId = "scatterplot", width = '100%', height = 600)
                    ),
                    tabPanel(
                      title = "Differences",
                      icon = icon('chart-simple'),
                      h2(textOutput("difftitle", inline = TRUE)),
                      p("This plot divides the census tracts in the selected localities into three groups, representing the Low, Middle, and High values of the measure selected for Variable 1."),  
                      p("The height of the bar shows the average value of the measure selected for Variable 2 within that group of tracts. Hover over each bar to see the average value of Variable 2."),
                      p("For Example, when Estimated Population (Variable 1) and Average Land Surface Temperature (Variable 2) are selected, this tercile plot shows that the tracts with the Lowest populations have an average value of the Average Land Surface Temperature of 96.7, and the tracts with the Highest populations have an average value of the Average Land Surface Temperature of 92.5."),
                      plotlyOutput(outputId = 'tercile_plot', width = '100%', height = 500)
                    ),
                    tabPanel(title = "Data Table",
                      icon = icon('table-cells-large'),
                      h2(textOutput("tbltitle", inline = TRUE)),
                      p("The data table below shows the values for the selected variables and the tercile group (Low, Medium, High) for the census tracts in the selected localities."),
                      p("To download the data for all available measures and localities in this app, see the Download Data section at the bottom of the page."),
                      DTOutput("tbl")
                    )
                  )
                )
                  ), br(), hr(),  # end fluidRow
                  fluidRow(
                    column(
                      width = 12,
                      h3("Download Data"),
                      p("Data in this Atlas is provided as a compressed folder, which includes CSVs for data at the census tract level and a data dictionary."),
                      downloadButton("downloadBtn", "Download"),
                      br(), hr(), 
                      h4("Citation"),
                      p("The Equity Center, Democratization of Data Initiative; \"Charlottesville Regional Climate Equity Dashboard\"; An Initiative of the UVA Karsh Institute of Democracy Center for the Redress of Inequity Through Community-Engaged Scholarship; Accessed ", Sys.Date(), "; https://equityatlas.virginiaequitycenter.org/dashboards/climate-dashboard/.")
                      
                    )
                  ) # end fluidRow

                 ),
                 singleton(tags$head(tags$script(src = "message-handler.js")))
)) # end navbarPage / end HTML template



# ....................................
# Define Server Logic ----
server <- function(input, output, session) {

  geo_data <- reactive({
    if (input$indicator1 == input$indicator2) {
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("Please make sure that you've selected two different variables."))
    } else if (length(input$locality) == 0) {
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("At least one locality must be selected."))
    } else {
      geo <- geo %>%
        dplyr::select(x = !!sym(input$indicator1),
                      y = !!sym(input$indicator2),
                      locality, countyname, tract, geoid,
                      pop = pop) %>%
        dplyr::filter(locality %in% input$locality) %>%
        drop_na()
    }
  })

  ## Build scatterplot ----

  ## scatterplot title
  output$comptitle <- renderText({
    if (input$indicator1 == input$indicator2 | length(input$locality) == 0) {
      paste0("Please make sure that you've selected two different variables and/or at least one locality.")
    } else {
      paste(attr(geo_data()$x, "goodname"), " vs. ", 
            attr(geo_data()$y, "goodname"))
    }
  })

  output$scatterplot <- renderPlotly({
    if (input$indicator1 == input$indicator2 | length(input$locality) == 0) {
      plotly_empty()
    } else {
      d <- st_drop_geometry(geo_data())
      xhist <- plot_ly(data = d, x = ~x,
                       type = "histogram", nbinsx = 20,
                       alpha =.75, color = I("grey")) %>%
        layout(yaxis = list(showgrid = FALSE,
                            showticklabels = FALSE,
                            fixedrange = T),
               xaxis = list(showticklabels = FALSE,
                            fixedrange = T))

      yhist <- plot_ly(data = d, y = ~y,
                       type = "histogram", nbinsx = 20,
                       alpha = .75, color = I("grey")) %>%
        layout(xaxis = list(showgrid = FALSE,
                            showticklabels = TRUE,
                            fixedrange = T),
               yaxis = list(showticklabels = FALSE,
                            fixedrange = T))

      xyscatter <- plot_ly(data = d, x = ~x, y = ~y,
                           type = "scatter",
                           mode = "markers", # to remove mode warning
                           fill = ~"", # to remove line.width error
                           size = ~pop,
                           sizes = c(1, 500),
                           color = ~countyname,
                           colors = fewpal,
                           alpha = .75,
                           text = paste0("Locality: ", d$countyname, "<br>",
                                         "Census tract: ", d$tract, "<br>",
                                         "Population: ", d$pop, "<br>",
                                         attr(d$x, "goodname"), ": ", round(d$x, 2), "<br>",
                                         attr(d$y, "goodname"), ": ", round(d$y, 2), "<br>"),
                           hoverinfo = "text") %>%
        layout(xaxis = list(title = attr(d$x, "goodname"), showticklabels = TRUE, fixedrange = T),
               yaxis = list(title = attr(d$y, "goodname"), showticklabels = TRUE, fixedrange = T),
               legend = list(orientation = "h", x = 0, y = -0.2))

      # note: in the legend, we hide trace 1 (the xhist) and trace (3 + length(input$locality)), which is the yhist;
      #       the yhist's trace # changes as a user selects different localities to map, but it can be dynamically
      #       referenced as... yhist trace number = 1 (xhist) + 1 (plotly_empty) + n_localities + 1 (to reach the yhist)
      subplot(xhist, plotly_empty(), xyscatter, yhist,
              nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
              shareX = TRUE, shareY = TRUE) %>%
        style(showlegend = FALSE, traces = c(1, sum(3 + length(input$locality)))) %>%
        layout(xaxis = list(showgrid = TRUE),
               yaxis2 = list(showgrid = TRUE))
    }
  })

  ## Build map ----
  # map tab title
  output$maptitle <- renderText({
    if (input$indicator1 == input$indicator2 | length(input$locality) == 0) {
      paste0("Please make sure that you've selected two different variables and/or at least one locality.")
    } else {
      paste(attr(geo_data()$x, "goodname"), " and ", 
            attr(geo_data()$y, "goodname"))
    }
  })
  
  #build static parts of map, and display initial outline of region
  output$leaf <- renderLeaflet({
    leaflet() %>% addProviderTiles('CartoDB.Positron') %>%
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      addPolygons(data = geo, color = 'grey', opacity = 0) %>%
      # setView(lng = -78.47668, lat = 38.02931, zoom = 9) %>%
      addLogo('bivariate_legend_static.svg', src = "remote",
              position = "topleft", width = 140, height = 140, alpha = 1) %>%
      addResetMapButton()
  })

    ## Add Map Reset button function ----
  addResetMapButton <- function(leaf) {
    leaf %>%
      addEasyButton(
        easyButton(
          icon = "ion-arrow-expand",
          title = "Reset View", 
          onClick = JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }")
        )
      ) %>% 
      htmlwidgets::onRender(
        JS(
          "function(el, x){ 
            var map = this; 
            map.whenReady(function(){
              map._initialCenter = map.getCenter(); 
              map._initialZoom = map.getZoom();
            });
          }"
        )
      )
  }
  
  # reactive function to detect when variable 1, variable 2, or locality selection changes
  listen_closely <- reactive({
    list(input$indicator1,input$indicator2, input$locality)
  })
  # when a variable or locality selection is changed, render the appropriate bichoropleth without losing the legend
  observeEvent(listen_closely(), {
    if (input$indicator1 == input$indicator2 | length(input$locality) == 0) {
      leafletProxy('leaf') %>% clearShapes()
    } else if (input$indicator1 %in% cant_map | input$indicator2 %in% cant_map) {
      session$sendCustomMessage(type = 'testmessage', message = cant_map_message)
      leafletProxy('leaf') %>% clearShapes()
    } else {
      to_map <- bi_class(geo_data(), x = x, y = y, style = "quantile", dim = 3) %>% 
        mutate(var1_tercile = stri_extract(bi_class, regex = '^\\d{1}(?=-\\d)'),
               var1_tercile_cat = case_when(var1_tercile == 1 ~ 'Low',
                                       var1_tercile == 2 ~ 'Medium',
                                       var1_tercile == 3 ~ 'High'),
               var2_tercile = stri_extract(bi_class, regex = '(?<=\\d-)\\d{1}$'),
               var2_tercile_cat = case_when(var2_tercile == 1 ~ 'Low',
                                            var2_tercile == 2 ~ 'Medium',
                                            var2_tercile == 3 ~ 'High'),
               goodname_x = attr(geo_data()$x, "goodname"),
               goodname_y = attr(geo_data()$y, "goodname")) 
      factpal <- colorFactor(bipal, domain = to_map$bi_class)
      leafletProxy('leaf', data = to_map) %>% 
        clearShapes() %>%
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
                    popup = paste0("<b>Locality:</b> ", to_map$countyname, ", tract ", to_map$tract, "<br><b>",
                                   to_map$goodname_x, ":</b> ", round(to_map$x, 2),  "<br>",
                                   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Relative to other tracts: <b>", to_map$var1_tercile_cat, "</b><br><b>",
                                   to_map$goodname_y, ":</b> ", round(to_map$y, 2), "<br>",
                                   "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Relative to other tracts: <b>", to_map$var2_tercile_cat, "</b>"))
    }
  })

  ## Build tercile plot ----
  
  output$difftitle <- renderText({
    if (input$indicator1 == input$indicator2 | length(input$locality) == 0) {
      paste0("Please make sure that you've selected two different variables and/or at least one locality.")
    } else {
      paste("Tercile Plot:", attr(geo_data()$x, "goodname"), "rank by ", 
            attr(geo_data()$y, "goodname"), "averages")
    }
  })

  output$tercile_plot <- renderPlotly({
    if (input$indicator1 %in% cant_map | input$indicator2 %in% cant_map | input$indicator1 == input$indicator2 | length(input$locality) == 0) {
      plotly_empty()
    } else {
      to_tercile <- bi_class(geo_data(), x = x, y = y, style = "quantile", dim = 3) %>%
        mutate(var1_tercile = stri_extract(bi_class, regex = '^\\d{1}(?=-\\d)'),
                var_1_group = case_when(var1_tercile == 1 ~ 'Low',
                                        var1_tercile == 2 ~ 'Medium',
                                        var1_tercile == 3 ~ 'High'),
                goodname_x = attr(geo_data()$x, "goodname"),
                goodname_y = attr(geo_data()$y, "goodname")) %>% 
        group_by(var1_tercile) %>% 
        mutate(var_2_mean = mean(y, na.rm = T)) %>% 
        slice(1)
      to_tercile <- st_drop_geometry(to_tercile)
      t <- ggplot(to_tercile, aes(x = var1_tercile, y = var_2_mean,
                                  fill = var1_tercile, label = var_1_group,
                                  text = paste0('Mean of ', goodname_y, ': ', round(var_2_mean, digits = 2)))) +
        geom_bar(stat = 'identity', width = 0.66) +
        scale_fill_manual(values = c('#dfb0d6', '#a5add3', '#569ab9')) +
        scale_x_discrete(labels = paste0(c('Lowest ', 'Middle ', 'Highest '), 'third of tracts')) +
        theme_minimal()

      ggplotly(t, tooltip = c('text')) %>%
        layout(showlegend = FALSE, 
                xaxis = list(title = list(text = attr(geo_data()$x, "goodname"), font = list(size=14)), showticklabels = TRUE, fixedrange = TRUE),
               yaxis = list(title = list(text = attr(geo_data()$y, "goodname"), font = list(size=14)),showticklabels = TRUE, fixedrange = TRUE)
                )
    }
  })

  # Build Data Table -------------------------------------------------------
  
  ## output data table ----
  ## Only shows currenly selected variables - added for accessibility purposes
  
  output$tbl <- renderDT({
    tble_data <- st_drop_geometry(geo_data()) 
    tble_data <- bi_class(tble_data, x = x, y = y, style = "quantile", dim = 3) %>% 
      mutate(var1_tercile = stri_extract(bi_class, regex = '^\\d{1}(?=-\\d)'),
             var1_tercile_cat = case_when(var1_tercile == 1 ~ 'Low',
                                          var1_tercile == 2 ~ 'Medium',
                                          var1_tercile == 3 ~ 'High'),
             var2_tercile = stri_extract(bi_class, regex = '(?<=\\d-)\\d{1}$'),
             var2_tercile_cat = case_when(var2_tercile == 1 ~ 'Low',
                                          var2_tercile == 2 ~ 'Medium',
                                          var2_tercile == 3 ~ 'High'),
             y = round(y, digits = 2)) %>%
      select(locality, countyname, tract, x, var1_tercile_cat, y, var2_tercile_cat, pop)
    tble_data <- tble_data[with(tble_data, order(countyname, x)), ]
    datatable(tble_data,
              colnames=c("Locality", "County", "Tract", attr(geo_data()$x, "goodname"), "Tercile Group Var 1", attr(geo_data()$y, "goodname"), "Tercile Group Var 2", "Est. Population"),
              rownames = FALSE,
              options = list(pageLength = 15, list(1, 'desc'), scrollX = TRUE))
  })
  
  ## data table title ----
  output$tbltitle <- renderText({
    if (input$indicator1 == input$indicator2 | length(input$locality) == 0) {
      paste0("Please make sure that you've selected two different variables and/or at least one locality.")
    } else {
    paste("Data by Census Tract: ", attr(geo_data()$x, "goodname"), "(Variable 1) and ", 
          attr(geo_data()$y, "goodname"), "(Variable 2)")
    }
  })
  
  ## data download button function ----
  output$downloadBtn <- downloadHandler(
    filename = paste0("data_download.csv"),
    content = function(file) {
      write.csv(st_drop_geometry(geo_data()), file)
    }
  )
  ## data download button function ----
  output$downloadBtn <- downloadHandler(
    filename = "data_download.zip",
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("climate_atlas_census_tract.csv", "variable_dictionary_1.csv", "variable_dictionary_2.csv")
      write.csv(st_drop_geometry(geo), file = "climate_atlas_census_tract.csv", sep = ",")
      write.csv(group_df_clim, file = "variable_dictionary_1.csv", sep = ",")
      write.csv(group_df_dem, file = "variable_dictionary_2.csv", sep = ",")
      print (fs)
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )


  ## refresh app ----
  observeEvent(input$refresh, session$reload())
  
  ## output variable information ----

  # by selector
  # indicator 1
  output$ind1_defn <- renderText({
    attr(geo_data()$x, "description")
  })

  output$ind1_source <- renderText({
    paste("Source: ", attr(geo_data()$x, "source"))
  })

  # indicator 2
  output$ind2_defn <- renderText({
    attr(geo_data()$y, "description")
  })

  # indicator 2 description by selector
  output$ind2_source <- renderText({
    paste("Source: ", attr(geo_data()$y, "source"))
  })

  # detailed var info on var info tab
  # indicator 1
  output$var1_name <- renderText({
    attr(geo_data()$x, "goodname")
  })

  output$var1_abt <- renderText({
    attr(geo_data()$x, "about")
  })

  output$var1_source <- renderText({
    paste("Source: ", attr(geo_data()$x, "source"))
  })

  # indicator 2
  output$var2_name <- renderText({
    attr(geo_data()$y, "goodname")
  })

  output$var2_abt <- renderText({
    attr(geo_data()$y, "about")
  })

  output$var2_source <- renderText({
    paste("Source: ", attr(geo_data()$y, "source"))
  })

  ## about page ----
  # output$documentation <- renderUI(htmltools::includeHTML("cville_climate_update.html"))

  }
# Run the application ----
shinyApp(ui = ui, server = server)
