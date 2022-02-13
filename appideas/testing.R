library(tidyverse)
library(plotly)

df <- readRDS("appideas/data/cvl_data.RDS")


# In parts
xhist <- plot_ly(data = df, x = ~percentile_2016, type = 'histogram', 
                 nbinsx = 20, alpha =.75, color = I("grey")) %>% 
  layout(yaxis = list(showgrid = FALSE,
                      showticklabels = FALSE),
         xaxis = list(showticklabels = FALSE))
xhist

xyscatter <- plot_ly(data = df, 
                     x = ~percentile_2016, 
                     y = ~current_asthma2018, 
                     type = 'scatter',
                     size = ~totalpopE, 
                     sizes = c(1, 500),
                     color = ~locality, 
                     colors = "Dark2", 
                     alpha = .75,
                     text = paste0("County: ", df$locality, "<br>",
                                   "Population: ", df$totalpopE, "<br>",
                                   "Census tract: ", df$tract, "<br>",
                                   "X: ", df$percentile_2016, "<br>",
                                   "Y: ", df$current_asthma2018, "<br>"),
                     hoverinfo = "text") %>% 
  layout(xaxis = list(title = "PM2.5 Percentile, 2016"),
         yaxis = list(title = "Percent with Asthma"),
         legend = list(orientation = "h", x = 0, y = -0.2))
xyscatter

yhist <- plot_ly(data = df, y = ~current_asthma2018, type = 'histogram', 
                 nbinsx = 20, alpha = .75, color = I("grey")) %>% 
  layout(xaxis = list(showgrid = FALSE,
                      showticklabels = FALSE),
         yaxis = list(showticklabels = FALSE))
yhist

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

geo <- readRDS("appideas/data/cvl_data_geo.RDS")

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


tmp <- df %>%
  dplyr::filter(locality %in% c("540", "003", "065")) %>% 
  dplyr::select(x = totalpopE,
                y = whiteE,
                locality = locality, 
                tract = tract,
                pop = pop,
                geoid = geoid)

geo2 <- geo %>% select(geoid, geometry)

to_map <- left_join(tmp, geo2, by = "geoid")
to_map <- bi_class(tmp, x = x, y = y, style = "quantile", dim = 3)
to_map <- st_transform(st_as_sf(to_map), 4326)
factpal <- colorFactor(bipal, domain = to_map$bi_class)

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
                bringToFront = T))

