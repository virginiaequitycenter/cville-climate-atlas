

library(tidyverse)
library(ggplot2)
library(ggExtra)
library(plotly)
library(RColorBrewer)

lead <- read_csv("data/lead_cville_tract.csv")
exposure <- read_csv("data/leadexposure_cville_tract.csv")
lai <- read_csv("data/locationaffordability_cville_tract.csv")
places <- read_csv("data/cdcplaces_cville_tract.csv")
airquality <- read_csv("data/airquality_cville_tract.csv")
population <- read_csv("data/acs_tract_cville.csv")

lead <- lead %>% rename(GEOID = FIP)
places <- places %>% rename(GEOID = locationname)
airquality <- airquality %>% rename(GEOID = gid)
lead_and_exposure <- merge(lead, exposure, by = "GEOID")
lead_lai_exposure <- merge(lead_and_exposure, lai, by = "GEOID")
lead_lai_exposure_places <- merge(lead_lai_exposure, places, by = "GEOID")
allminuspopulation <- merge(lead_lai_exposure_places, airquality, by = "GEOID")
all <- merge(allminuspopulation, population, by = "GEOID")

p7 <- ggplot(all, aes(x = percentile_2016, y = Current_Asthma2018, size = totalpopE, alpha = 1/10, color = county)) +
  geom_point() +
  labs(x = "PM2.5 Percentile, 2016", y = "Percent with Asthma") +
  guides(size = "none", alpha = "none") +
  theme(legend.position = "bottom")
ggMarginal(p7, type = "histogram")

ggplotly(p7)


# Take 2

subplot(
  plot_ly(data = all, x = ~percentile_2016, type = 'histogram', # upper left
          alpha =.75, color = I("grey")),
  plotly_empty(), # upper right
  plot_ly(data = all, x = ~percentile_2016, y = ~Current_Asthma2018, type = 'scatter',
          size = ~totalpopE, sizes = c(1, 500),
          color = ~county, alpha = .75), # lower left
  plot_ly(data = all, y = ~Current_Asthma2018, type = 'histogram', 
          alpha = .75, color = I("grey")), # lower right
  nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0)

# In parts
xhist <- plot_ly(data = all, x = ~percentile_2016, type = 'histogram', 
                 nbinsx = 20, alpha =.75, color = I("grey")) %>% 
  layout(yaxis = list(showgrid = FALSE,
                      showticklabels = FALSE),
         xaxis = list(showticklabels = FALSE))
xhist


xhist2 <- plot_ly(data = all, x = ~percentile_2016, type = 'histogram', 
                 nbinsx = 20, alpha =.75,
                 color = ~Current_Asthma2018, colors = "Blues") %>% 
  layout(yaxis = list(showgrid = FALSE,
                      showticklabels = FALSE),
         xaxis = list(showticklabels = FALSE))
xhist2


xyscatter <- plot_ly(data = all, 
                     x = ~percentile_2016, 
                     y = ~Current_Asthma2018, 
                     type = 'scatter',
                     size = ~totalpopE, 
                     sizes = c(1, 750),
                     color = ~county, 
                     colors = "Dark2", 
                     alpha = .75,
                     text = paste0("County: ", all$county, "<br>",
                                   "Population: ", all$totalpopE, "<br>",
                                   "Census tract: ", all$tractfips, "<br>",
                                   "X: ", all$percentile_2016, "<br>",
                                   "Y: ", all$Current_Asthma2018, "<br>"),
                     hoverinfo = "text") %>% 
  layout(xaxis = list(title = "PM2.5 Percentile, 2016"),
         yaxis = list(title = "Percent with Asthma"),
         legend = list(orientation = "h", x = 0.0, y = -0.2))
xyscatter


pal2 <- brewer.pal(6, "Dark2")

xyscatter2 <- plot_ly(data = all, 
                     x = ~percentile_2016, 
                     y = ~Current_Asthma2018, 
                     type = 'scatter',
                     marker = list(size = ~totalpopE, sizeref = 250, sizemode = "diameter"),
                     color = ~county,
                     colors = pal2,
                     alpha = 0.75,
                     text = paste0(all$county, "<br>",
                                   "Tract: ", all$tractfips),
                     hovertemplate = paste(
                       "<b>%{text}</b><br>",
                       "Population: %{marker.size}<br>",
                       "%{yaxis.title.text}: %{y}<br>",
                       "%{xaxis.title.text}: %{x}<br>",
                       "<extra></extra>")
                     ) %>% 
  layout(xaxis = list(title = "PM2.5 Percentile"),
         yaxis = list(title = "Percent with Asthma"),
         legend = list(orientation = "h", x = 0.0, y = -0.2))
xyscatter2


yhist <- plot_ly(data = all, y = ~Current_Asthma2018, type = 'histogram', 
                 nbinsx = 20, alpha = .75, color = I("grey")) %>% 
  layout(xaxis = list(showgrid = FALSE,
                      showticklabels = FALSE),
         yaxis = list(showticklabels = FALSE))
yhist


yhist2 <- plot_ly(data = all, y = ~Current_Asthma2018, type = 'histogram', 
                 nbinsx = 20, alpha = .75,
                 color = ~Current_Asthma2018, colors = "Blues") %>% 
  layout(xaxis = list(showgrid = FALSE,
                      showticklabels = FALSE),
         yaxis = list(showticklabels = FALSE))
yhist2


subplot(xhist, plotly_empty(), xyscatter2, yhist,
        nrows = 2, heights = c(.2, .8), widths = c(.8,.2), margin = 0,
        shareX = TRUE, shareY = TRUE) %>% 
  style(showlegend = FALSE, traces = c(1,9)) %>%  # remove hist symbols from legend  
  layout(xaxis = list(showgrid = TRUE),
         yaxis2 = list(showgrid = TRUE))

# get edges/box around primary scatterplot?

# alternative hover control
# hovertemplate = paste('County: %{county}', # this one doesn't work
#                       '<br>Population: %{marker.size:,}',
#                       '<br>%{xaxis.title.text}: %{x}',
#                       '<br>%{yaxis.title.text}: %{y}')) %>% 

