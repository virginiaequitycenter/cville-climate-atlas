library(tidyverse)
library(plotly)
library(leaflet)

df <- readRDS("data/cvl_data.RDS")


# In parts
xhist <- plot_ly(data = df, x = ~tree_can, type = 'histogram', 
                 nbinsx = 20, alpha =.75, color = I("grey")) %>% 
  layout(yaxis = list(showgrid = FALSE,
                      showticklabels = FALSE),
         xaxis = list(showticklabels = FALSE))
xhist

yhist <- plot_ly(data = df, y = ~whiteE, type = 'histogram', 
                 nbinsx = 20, alpha = .75, color = I("grey")) %>% 
  layout(xaxis = list(showgrid = FALSE,
                      showticklabels = FALSE),
         yaxis = list(showticklabels = TRUE))
yhist

xyscatter <- plot_ly(data = df, 
                     x = ~tree_can, 
                     y = ~whiteE, 
                     type = 'scatter',
                     size = ~totalpopE, 
                     sizes = c(1, 500),
                     color = ~countyname, 
                     colors = "Dark2", 
                     alpha = .75,
                     text = paste0("County: ", df$countyname, "<br>",
                                   "Population: ", df$totalpopE, "<br>",
                                   "Census tract: ", df$tract, "<br>",
                                   "X: ", df$tree_can, "<br>",
                                   "Y: ", df$whiteE, "<br>"),
                     hoverinfo = "text") %>% 
  layout(xaxis = list(title = "Tree Canopy", showticklabels = TRUE),
         yaxis = list(title = "Percent White", showtickLabels = TRUE),
         legend = list(orientation = "h", x = 0, y = -0.2))
xyscatter

subplot(xhist, plotly_empty(), xyscatter, yhist,
        nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
        shareX = TRUE, shareY = TRUE) %>% 
  style(showlegend = FALSE, traces = c(1,9)) %>%  # remove hist symbols from legend  
  layout(xaxis = list(showgrid = TRUE),
         yaxis2 = list(showgrid = TRUE))


# get shiny input into plotly variables
# https://stackoverflow.com/questions/36980999/change-plotly-chart-y-variable-based-on-selectinput

# plotly hover code?
# text = paste0("County: ", df$locality, "<br>",
#               "Population: ", df$totalpopE, "<br>",
#               "Census tract: ", df$tract, "<br>",
#               "X: ", df[get(input$indicator1)], "<br>",
#               "Y: ", df[get(input$indicator2)], "<br>"),
# hoverinfo = "text"


# https://stackoverflow.com/questions/39798042/r-shiny-how-to-use-multiple-inputs-from-selectinput-to-pass-onto-select-optio
# https://shiny.rstudio.com/reference/shiny/1.6.0/varSelectInput.html
# https://community.rstudio.com/t/use-shiny-to-choose-column-equality-and-value-to-filter-by-conditions/47449

library(sf)
library(leaflet)
library(leafem)
library(biscale)
library(stringi)

geo <- readRDS("data/cvl_data_geo.RDS")

varlist <- geo %>% select(where(is.numeric), -pop) %>% names()

# create palette for use in bichoropleth palette function
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
           "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
           "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-3

# create palette object for logo generation
bipal_logo <- tibble("3-3" = "#3b4994", "2-3" = "#8c62aa", "1-3" = "#be64ac",
                     "3-2" = "#5698b9", "2-2" = "#a5add3", "1-2" = "#dfd0d6",
                     "3-1" = "#5ac8c8", "2-1" = "#ace4e4", "1-1" = "#e8e8e8") %>%
  gather("group", "fill")

# generate and save new legend svg
bipal_logo2 <- bipal_logo %>%
  separate(group, into = c("var1", "var2"), sep = "-") 

legend_ <- ggplot() +
  geom_tile(data = bipal_logo2, mapping = aes(x = var1, y = var2, fill = fill)) +
  scale_fill_identity() + theme_void() + coord_fixed() +
  labs(x = paste0("Higher ", "Variable 1", " \u2192"), # Check if these are right in terms of y/x axes
       y = paste0("Higher ", "Variable 2", " \u2192")) +
  theme(axis.title = element_text(size = 6), axis.title.y = element_text(angle = 90))
ggsave(plot = legend_, filename = 'appideas/www/bivariate_legend_static.svg', width = 1, height = 1)

# make diagonals of legend for sidebar?


tmp <- df %>%
  dplyr::select(x = tree_can,
                y = whiteE,
                locality, countyname, tract, geoid,
                pop = pop) %>% 
  dplyr::filter(locality %in% c("540", "003", "065", "079", "109", "125"))

geo2 <- geo %>% select(locality, geoid, geometry)

# to_map <- left_join(geo2, tmp, by = "geoid")
# to_map <- bi_class(to_map, x = x, y = y, style = "quantile", dim = 3)
# to_map <- st_transform(st_as_sf(to_map), 4326)
# factpal <- colorFactor(bipal, domain = to_map$bi_class)

to_map <- merge(tmp, geo2, by = 'geoid')
to_map <- bi_class(to_map, x = x, y = y, style = "quantile", dim = 3)
to_map <- st_transform(st_as_sf(to_map), 4326)
to_map$var1_tercile <- stri_extract(to_map$bi_class, regex = '^\\d{1}(?=-\\d)')
to_map$var2_tercile <- stri_extract(to_map$bi_class, regex = '(?<=\\d-)\\d{1}$')
factpal <- colorFactor(bipal, domain = to_map$bi_class)

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = geo2, color = 'grey', opacity = 0) %>%
  setView(lng = -78.47668, lat = 38.02931, zoom = 9) %>%
  addLogo('bivariate_legend_static.svg', src = "remote",
          position = "topleft", width = 100, height = 100, alpha = 0.8) %>% 

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


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

    # addLogo() will look in www/ by default if src is `remote`
  addLogo('appideas/www/bivariate_legend.svg', src = "local",
          position = "topleft", width = 100, height = 100, alpha = 0.8) %>% 
  
  addPolygons(data = to_map,
              fillColor = ~factpal(bi_class),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T)) %>% 
  #setView(lng = mean(geo_df$Longitude), lat = mean(geo_df$Latitude), zoom = 12) 
  setView(lng = -78.47668, lat = 38.02931, zoom = 9)

  
# From app version ----
library(tidyverse)
library(plotly)
library(biscale)

geo <- readRDS("appideas/data/cvl_data_geo.RDS")

tmp <- geo %>%
  dplyr::select(x = whiteE,
                y = tree_can,
                locality, countyname, tract, geoid,
                pop = pop) %>% 
  dplyr::filter(locality %in% c("540", "003", "065", "079", "109", "125")) %>% 
  drop_na()

to_tercile <- bi_class(tmp, x = x, y = y, style = "quantile", dim = 3)
to_tercile$var1_tercile <- stri_extract(to_tercile$bi_class, regex = '^\\d{1}(?=-\\d)')
to_tercile$`Var 1 Group` <- ifelse(to_tercile$var1_tercile == 1, 'Low', ifelse(to_tercile$var1_tercile == 2, 'Medium', ifelse(to_tercile$var1_tercile == 3, 'High', '')))
#to_tercile <- to_tercile %>% group_by(var1_tercile) %>% summarize(var2_mean = mean(tree_can, na.rm = T))
to_tercile <- to_tercile %>% group_by(var1_tercile) %>% 
  mutate(`Var 2 Mean` = mean(y, na.rm = T)) %>% 
  slice(1)
# to_tercile <- to_tercile[to_tercile$var1_tercile %in% 1:3, ]

t <- ggplot(to_tercile, aes(x = var1_tercile, y = `Var 2 Mean`, fill = var1_tercile,
                            label = `Var 1 Group`)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c('#dfb0d6', '#a5add3', '#569ab9')) +
  scale_x_discrete(labels = paste0(c('Lowest ', 'Middle ', 'Highest '), 'third of tracts')) +
  # scale_y_continuous(position = "right") +
  labs(x = attr(to_tercile$x, "goodname"), 
       y = attr(to_tercile$y, "goodname")) +
  # theme(legend.position = "none") +
  theme_minimal()

ggplotly(t, tooltip = c("label", "y")) %>% 
  layout(showlegend = FALSE, yaxis = list(side = "right"))
  
