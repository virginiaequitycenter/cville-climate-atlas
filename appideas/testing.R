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