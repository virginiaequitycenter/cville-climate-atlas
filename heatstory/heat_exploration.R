# Heat/Land surface temperature
# Exploration
# lsat data: blockgroup 
# lsat data: with tree canopy/imp surface/forest/developed
# lsat data: with income, homeownership, race 
# NOPE: lsat data: poverty or life expectancy at tract level?
# NOPE: lsat data: with commute?
# NOPE: lsat data: with daymet
# ...........................................

library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(scales)
library(ggthemes)
library(patchwork)

# ...........................................
# read data ----

# locality names
localities <- data.frame(
  cntyfips = c("003", "540", "065", "079", "109", "125"),
  cntyname = c("Albemarle", "Charlottesville", "Fluvanna",
               "Greene", "Louisa", "Nelson"))

# lsat
ls8 <- read_csv("data-csv/landsat8_cville_blkgps.csv")
ls8 <- ls8 %>% 
  mutate(GEOID = as.character(GEOID))

ls8_jul4 <- ls8 %>% 
  filter(start_date == as.Date("2020-07-04")) %>% 
  mutate(med_dev = median - mean(median)) 
# July 4 2020: min/max = 22/33, 71.6/91.4
ls8_jul13 <- ls8 %>% 
  filter(start_date == as.Date("2020-07-13"))  %>% 
  mutate(med_dev = median - mean(median, na.rm = TRUE))
# July 13 2020: min/max = 18/31, 64.4/87.8
ls8_jul20 <- ls8 %>% 
  filter(start_date == as.Date("2020-07-20"))  %>% 
  mutate(med_dev = median - mean(median, na.rm = TRUE))
# July 20 2020: min/max = 23/37, 73.4/98.6

# block group geo
blkgp <- readRDS("data-csv/cville_blkgps.RDS")
blkgp <- st_transform(blkgp, crs = 4326)

ls8_jul4_geo <- left_join(blkgp, ls8_jul4, by = "GEOID") %>% 
  left_join(localities, by = c("COUNTYFP" = "cntyfips"))
ls8_jul20_geo <- left_join(blkgp, ls8_jul20, by = "GEOID")


# ...........................................
# map july 4 and july 20 ----
# Num bins?
ggplot(ls8_jul4, aes(x = med_dev)) +
  geom_histogram()

pal <- colorBin(
  #palette = heat.colors(10),
  #palette = viridis_pal(option = "turbo", direction = -1)(10),
  #palette = colorRampPalette(rev(brewer.pal(8, "Spectral")))(7),
  palette = brewer_pal(palette = "Spectral", direction = -1)(7),
  domain = ls8_jul20_geo$med_dev, 
  bins = 7)

leaflet(data = ls8_jul20_geo) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = ls8_jul20_geo,
              fillColor = ~pal(med_dev),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("Degres above Averaege: ", ls8_jul20_geo$med_dev)) %>% 
  addLegend(position = "bottomright", 
            pal = pal, values = ls8_jul20_geo$med_dev,
            title = "Deviation from Average")

# compare to Hannah's: cville and alb only
ls8_jul4_geo_cvlalb <- ls8_jul4_geo %>% 
  filter(COUNTYFP %in% c("003", "540"))

pal <- colorNumeric(
  #palette = heat.colors(10),
  #palette = viridis_pal(option = "turbo", direction = -1)(10),
  palette = brewer_pal(palette = "Spectral", direction = -1)(11),
  domain = ls8_jul4_geo_cvlalb$med_dev)

leaflet(data = ls8_jul4_geo_cvlalb) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = ls8_jul4_geo_cvlalb,
              fillColor = ~pal(med_dev),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("Degrees above Average: ", ls8_jul4_geo_cvlalb$med_dev)) %>% 
  addLegend(position = "bottomright", 
            pal = pal, values = ls8_jul4_geo_cvlalb$med_dev,
            title = "Deviation from Average")

# ...........................................
# contol bins and mapping ----
## july 4 ----
# see here: https://medium.com/ibm-data-ai/center-diverging-colors-on-leaflet-map-515e69d7f81f
summary(ls8_jul4_geo$med_dev)
# range -50 to 30
# interval options: (30- -50)/10 = 8 bins
ggplot(ls8_jul4_geo, aes(x = med_dev)) + 
  geom_histogram(binwidth = 10)

ls8_jul4_geo <- ls8_jul4_geo %>% 
  mutate(dev_breaks = cut(med_dev, 
                          breaks = c(-50, -20, -15, -10, -5, -2.5, 2.5, 5, 10, 20, 50),
                          include.lowest = TRUE,
                          labels = c("-25 or less", "-20 to -15",
                                     "-15 to -10", "-10 to -5",
                                     "-5 to -2.5", "-2.5 to 2.5",
                                     "2.5 to 5", "5 to 10",
                                     "10 to 20", "20 or more")),
         break_colors = case_when(
           dev_breaks == "-25 or less" ~ "#5e4fa2",
           dev_breaks == "-20 to -15" ~ "#3288bd",
           dev_breaks == "-15 to -10" ~ "#66c2a5",
           dev_breaks == "-10 to -5" ~ "#abdda4",
           dev_breaks == "-5 to -2.5" ~ "#e6f598",
           dev_breaks == "-2.5 to 2.5" ~ "#ffffbf",
           dev_breaks == "2.5 to 5" ~ "#fee08b",
           dev_breaks == "5 to 10" ~ "#fdae61",
           dev_breaks == "10 to 20" ~ "#f46d43",
           dev_breaks == "20 or more" ~ "#d53e4f"
         ),
         break_colors = factor(break_colors, 
                               levels = c("#5e4fa2", "#3288bd", 
                                          "#66c2a5", "#abdda4",
                                          "#e6f598", "#ffffbf",
                                          "#fee08b", "#fdae61", 
                                          "#f46d43", "#d53e4f")))

table(ls8_jul4_geo$dev_breaks)

leaflet(data = ls8_jul4_geo) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = ls8_jul4_geo,
              fillColor = ~break_colors,
              label = ~paste(cntyname, round(med_dev,2), sep = ", "),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
#              popup = paste0("Degrees above Average: ", round(ls8_jul4_geo$med_dev,2))
              ) %>% 
  addLegend(position = "topright", 
            labels = levels(ls8_jul4_geo$dev_breaks),
            colors = levels(ls8_jul4_geo$break_colors), 
            title = "Compared to Average",
            group = "Hide/Show Legend") %>% 
  addLayersControl(overlayGroups = c("Hide/Show Legend"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright")


## july 20 ----
summary(ls8_jul20_geo$med_dev)
# range -100 to 25
# interval options: (25- -100)/10 = 12 bins
# remove outlier: (25 - -80)/10 = 10 bins
# or bigger interval: (25- -100)/15 = 8 bins
ggplot(ls8_jul20_geo, aes(x = med_dev)) + 
  geom_histogram(binwidth = 10)

ls8_jul20_geo <- ls8_jul20_geo %>% 
  mutate(dev_breaks = cut(med_dev, 
                          breaks = c(-100, -20, -15, -10, -5, -2.5, 2.5, 5, 10, 20, 50),
                          include.lowest = TRUE,
                          labels = c("-25 or less", "-20 to -15",
                                     "-15 to -10", "-10 to -5",
                                     "-5 to -2.5", "-2.5 to 2.5",
                                     "2.5 to 5", "5 to 10",
                                     "10 to 20", "20 or more")),
         break_colors = case_when(
           dev_breaks == "-25 or less" ~ "#5e4fa2",
           dev_breaks == "-20 to -15" ~ "#3288bd",
           dev_breaks == "-15 to -10" ~ "#66c2a5",
           dev_breaks == "-10 to -5" ~ "#abdda4",
           dev_breaks == "-5 to -2.5" ~ "#e6f598",
           dev_breaks == "-2.5 to 2.5" ~ "#ffffbf",
           dev_breaks == "2.5 to 5" ~ "#fee08b",
           dev_breaks == "5 to 10" ~ "#fdae61",
           dev_breaks == "10 to 20" ~ "#f46d43",
           dev_breaks == "20 or more" ~ "#d53e4f"
         ),
         break_colors = factor(break_colors, 
                               levels = c("#5e4fa2", "#3288bd", 
                                          "#66c2a5", "#abdda4",
                                          "#e6f598", "#ffffbf",
                                          "#fee08b", "#fdae61", 
                                          "#f46d43", "#d53e4f")))

table(ls8_jul20_geo$dev_breaks)

leaflet(data = ls8_jul20_geo) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = ls8_jul20_geo,
              fillColor = ~break_colors,
              label = ~round(med_dev,2),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              #              popup = paste0("Degrees above Average: ", round(ls8_jul4_geo$med_dev,2))
  ) %>% 
  addLegend(position = "topright", 
            labels = levels(ls8_jul20_geo$dev_breaks),
            colors = levels(ls8_jul20_geo$break_colors), 
            title = "Compared to Average",
            group = "Hide/Show Legend") %>% 
  addLayersControl(overlayGroups = c("Hide/Show Legend"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright")

# ...........................................
# Show relationship to other variables ----
## nlcd ----
nlcd <- read_csv("data-csv/nlcd_cville_blkgps.csv")
nlcd <- nlcd %>% mutate(GEOID = as.character(GEOID))

# join to jul 4
ls8_jul4_nlcd <- ls8_jul4_geo %>% 
  left_join(nlcd, by = "GEOID")

tree <- ls8_jul4_nlcd %>% 
  ggplot(aes(x = tree_can, y = med_dev, color = cntyname)) +
  geom_point() +
  labs(x = "% Tree Canopy", y = "Land Surface Temp (Temp - Region Median)") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

surf <- ls8_jul4_nlcd %>% 
  ggplot(aes(x = imp_surf, y = med_dev, color = cntyname)) +
  geom_point() +
  labs(x = "% Impervious Surface", y = "Land Surface Temp (Temp - Region Median)") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

tree + surf + plot_layout(guides='collect') &
  theme(legend.position='bottom')

ls8_jul4_nlcd %>% 
  ggplot(aes(x = percent_for, y = med_dev)) +
  geom_point()

ls8_jul4_nlcd %>% 
  ggplot(aes(x = percent_dev, y = med_dev)) +
  geom_point()

# join to jul20
ls8_jul20_nlcd <- ls8_jul20 %>% 
  left_join(nlcd, by = "GEOID")

# scatterplot
ls8_jul20_nlcd %>% 
  ggplot(aes(x = tree_can, y = med_dev)) +
  geom_point()

ls8_jul20_nlcd %>% 
  ggplot(aes(x = percent_for, y = med_dev)) +
  geom_point()

ls8_jul20_nlcd %>% 
  ggplot(aes(x = imp_surf, y = med_dev)) +
  geom_point()

ls8_jul20_nlcd %>% 
  ggplot(aes(x = percent_dev, y = med_dev)) +
  geom_point()

# ## daymet? ----
# daym <- read_csv("data-csc/daymet_cville_blkgrps.csv")
# daym <- daym %>% mutate(GEOID = as.character(GEOID))
# daym2020 <- daym %>% filter(year == 2020)
# 
# # join to jul 4
# ls8_jul4_daym <- ls8_jul4 %>% 
#   left_join(daym2020, by = "GEOID")
# 
# # scatterplot
# ls8_jul4_daym %>% 
#   ggplot(aes(x = July_AvgMaxTF, y = med_dev)) + 
#   geom_point()
# 
# # join to jul 2-
# ls8_jul20_daym <- ls8_jul20 %>% 
#   left_join(daym2020, by = "GEOID")
# 
# # scatterplot
# ls8_jul20_daym %>% 
#   ggplot(aes(x = July_AvgMaxTF, y = med_dev)) + 
#   geom_point()

## income, race ----
acs <- read_csv("data-csv/acs_cville_blkgp.csv")
acs <- acs %>% mutate(GEOID = as.character(GEOID))

# join to jul 4
ls8_jul4_acs <- ls8_jul4_geo %>% 
  left_join(acs, by = "GEOID")

# scatterplot
p1 <- ls8_jul4_acs %>% 
  ggplot(aes(x = hhincE, y = med_dev, color = cntyname)) + 
  geom_point() +
  labs(x = "Median HH Income", y = "Land Surface Temp (Temp - Region Median)") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

p2 <- ls8_jul4_acs %>% 
  ggplot(aes(x = homeownE, y = med_dev, color = cntyname)) + 
  geom_point() +
  labs(x = "% Owning Home", y = "") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

p3 <- ls8_jul4_acs %>% 
  ggplot(aes(x = age17E, y = med_dev, color = cntyname)) + 
  geom_point() +
  labs(x = "% Age 17 or Younger", y = "") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

p4 <- ls8_jul4_acs %>% 
  ggplot(aes(x = age65E, y = med_dev, color = cntyname)) + 
  geom_point() +
  labs(x = "% Age 65 or Older", y = "") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

paxis <- ggplot(data.frame(l = p1$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")

p1$labels$y <- ""

paxis + ((p1 + p2) / (p3 + p4)) + plot_layout(guides='collect',
                                        widths = c(1,25)) &
  theme(legend.position='bottom')

paxis + wrap_plots(p1, p2, p3, p4) + plot_layout(guides='collect', widths = c(1,25)) &
  theme(legend.position='bottom')


p1 <- ls8_jul4_acs %>% 
  ggplot(aes(x = whiteE, y = med_dev, color = cntyname)) + 
  geom_point() +
  labs(x = "% White", y = "") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

p2 <- ls8_jul4_acs %>% 
  ggplot(aes(x = blackE, y = med_dev, color = cntyname)) + 
  geom_point() +
  labs(x = "% Black", y = "") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

p3 <- ls8_jul4_acs %>% 
  ggplot(aes(x = asianE, y = med_dev, color = cntyname)) + 
  geom_point() +
  labs(x = "% Asian", y = "") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

p4 <- ls8_jul4_acs %>% 
  ggplot(aes(x = ltnxE, y = med_dev, color = cntyname)) + 
  geom_point() +
  labs(x = "% Hispanic", y = "") +
  scale_color_few(name = "") +
  theme(legend.position = "bottom")

paxis + wrap_plots(p2, p4, p1, p3) + plot_layout(guides='collect', widths = c(1,25)) &
  theme(legend.position='bottom')


# ## life exp: tract ----
# life <- read_csv("data-csv/lifeexp_cville_tract.csv")
# life <- life %>% mutate(GEOID11 = as.character(GEOID))
# 
# # join to july 4
# ls8_jul4_life <- ls8_jul4 %>% 
#   mutate(GEOID11 = str_sub(GEOID, 1, 11)) %>% 
#   left_join(life, by = "GEOID11")
# 
# # plot
# ls8_jul4_life %>% 
#   ggplot(aes(x = life_expectancy, y = med_dev, color = county_fips)) + 
#   geom_jitter(width = .5, height = 0)

# lsat city data ----
ls_cville <- read_csv("data-csv/landsat8_cville_city.csv")

ls_cville_jul4 <- ls_cville %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  filter(spatial_unit == "blocks", 
         `Date Acquired` == as.Date("2020-07-04")) %>% 
  mutate(fips5 = str_sub(GEOID, 1,5),
         tract = str_sub(GEOID, 6,11),
         block = str_sub(GEOID, 12,15)) %>% 
  mutate(med_dev = median - mean(median, na.rm = TRUE))

# join to geography
cville_blocks <- readRDS("data-csv/cville_blocks.RDS")
cville_blocks <- cville_blocks %>% 
  mutate(GEOID = as.character(GEOID10)) %>% 
  filter(COUNTYFP10 == "540")

cville_blocks <- st_transform(cville_blocks, 4326)

ls_cvl_jul4_geo <- cville_blocks %>% 
  left_join(ls_cville_jul4, by = "GEOID")

# make breaks
summary(ls_cvl_jul4_geo$med_dev)
ls_cvl_jul4_geo %>% 
  ggplot(aes(x = med_dev)) +
  geom_histogram()

ls_cvl_jul4_geo <- ls_cvl_jul4_geo %>% 
  mutate(dev_breaks = cut(med_dev, 
                          breaks = c(-100, -20, -15, -10, -5, -2.5, 2.5, 5, 10, 20, 50),
                          include.lowest = TRUE,
                          labels = c("-25 or less", "-20 to -15",
                                     "-15 to -10", "-10 to -5",
                                     "-5 to -2.5", "-2.5 to 2.5",
                                     "2.5 to 5", "5 to 10",
                                     "10 to 20", "20 or more")),
         break_colors = case_when(
           dev_breaks == "-25 or less" ~ "#5e4fa2",
           dev_breaks == "-20 to -15" ~ "#3288bd",
           dev_breaks == "-15 to -10" ~ "#66c2a5",
           dev_breaks == "-10 to -5" ~ "#abdda4",
           dev_breaks == "-5 to -2.5" ~ "#e6f598",
           dev_breaks == "-2.5 to 2.5" ~ "#ffffbf",
           dev_breaks == "2.5 to 5" ~ "#fee08b",
           dev_breaks == "5 to 10" ~ "#fdae61",
           dev_breaks == "10 to 20" ~ "#f46d43",
           dev_breaks == "20 or more" ~ "#d53e4f"
         ),
         break_colors = factor(break_colors, 
                               levels = c("#5e4fa2", "#3288bd", 
                                          "#66c2a5", "#abdda4",
                                          "#e6f598", "#ffffbf",
                                          "#fee08b", "#fdae61", 
                                          "#f46d43", "#d53e4f")))

table(ls_cvl_jul4_geo$dev_breaks)

leaflet(data = ls_cvl_jul4_geo) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = ls_cvl_jul4_geo,
              fillColor = ~break_colors,
              label = ~round(med_dev,2),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              #              popup = paste0("Degrees above Average: ", round(ls8_jul4_geo$med_dev,2))
  ) %>% 
  addLegend(position = "topright", 
            labels = levels(ls_cvl_jul4_geo$dev_breaks),
            colors = levels(ls_cvl_jul4_geo$break_colors), 
            title = "Compared to Average",
            group = "Hide/Show Legend") %>% 
  addLayersControl(overlayGroups = c("Hide/Show Legend"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright")

## nlcd city blocks ----
ncld_cvl <- read_csv("data-csv/nlcd_cville_blocks.csv")
ncld_cvl <- ncld_cvl %>% 
  mutate(GEOID = as.character(GEOID10)) %>% 
  filter(COUNTYFP == "540")

# join
nlcd_cvl_geo <- cville_blocks %>% 
  left_join(ncld_cvl, by = "GEOID")

pal <- colorBin(palette = "Greens",
                domain = nlcd_cvl_geo$tree_can,
                bins = 9)

leaflet(data = nlcd_cvl_geo) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = nlcd_cvl_geo,
              fillColor = ~pal(tree_can),
              label = ~round(tree_can,2),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              #              popup = paste0("Degrees above Average: ", round(ls8_jul4_geo$med_dev,2))
  ) %>% 
  addLegend(position = "topright", 
            pal = pal, values = nlcd_cvl_geo$tree_can, 
            title = "Percent Canopy",
            group = "Hide/Show Legend") %>% 
  addLayersControl(overlayGroups = c("Hide/Show Legend"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright")

## join block data ----
cvl_ls8_nlcd <- cville_blocks %>% 
  left_join(ls_cville_jul4, by = "GEOID") %>% 
  left_join(ncld_cvl, by = "GEOID")

# scatterplots
ggplot(cvl_ls8_nlcd, aes(x = tree_can, y = med_dev, color = tree_can)) +
  geom_point() +
  scale_color_gradient(low = "#ffffe5", high = "#00441b")

ggplot(cvl_ls8_nlcd, aes(x = imp_surf, y = med_dev, color = imp_surf)) +
  geom_point() +
  scale_color_gradient(low = "#ffffcc", high = "#a50f15")

# STORY OUTLINE
# carmen slides 2,3
# CVILLE CITY
#   TABS: cville land surface: blocks
#   TABS: cville tree canopy: blocks
#   TABS: cville imp surf: blocks
# correlations: blocks (side by side)
# REGION
#   TABS: region land surface: blockgroups
#   TABS: region tree canopy: blockgroups
#   TABS: region imp surf: blockgroups
# correlations: blockgroups
#   TABS: w/trees and surface - side by sde
#   TABS: w/income and homeownership - side by side
#   TABS: w/white, w/black, w/asian, w/latinx - facet
# kids slides

# city (county?) block groups only ----
ls8_jul4_geo_cvl <-
  ls8_jul4_geo %>% 
  filter(COUNTYFP %in% c("540")) %>% 
  mutate(med_dev = median - mean(median)) 

summary(ls8_jul4_geo_cvl$med_dev)

# range -30 to 30
ggplot(ls8_jul4_geo_cvl, aes(x = med_dev)) + 
  geom_histogram(binwidth = 10)

ls8_jul4_geo_cvl <- ls8_jul4_geo_cvl %>% 
  mutate(dev_breaks = cut(med_dev, 
                          breaks = c(-30, -25, -20, -15, -10, -5, -2.5, 2.5, 5, 10, 15, 20),
                          include.lowest = TRUE,
                          labels = c("-25 or less", "-25 to -20",
                                     "-20 to -15", "-15 to -10",
                                     "-10 to -5", "-5 to -2.5", 
                                     "-2.5 to 2.5", "2.5 to 5", 
                                     "5 to 10", "10 to 15",
                                     "15 or more")),
         break_colors = case_when(
           dev_breaks == "-25 or less" ~ "#5e4fa2",
           dev_breaks == "-25 to -20" ~ "#3288bd",
           dev_breaks == "-20 to -15" ~ "#66c2a5",
           dev_breaks == "-15 to -10" ~ "#abdda4",
           dev_breaks == "-10 to -5" ~ "#e6f598",
           dev_breaks == "-5 to -2.5" ~ "#ffffbf",
           dev_breaks == "-2.5 to 2.5" ~ "#fee08b",
           dev_breaks == "2.5 to 5" ~ "#fdae61",
           dev_breaks == "5 to 10" ~ "#f46d43",
           dev_breaks == "10 to 15" ~ "#d53e4f",
           dev_breaks == "15 or more" ~ "#9e0142"
         ),
         break_colors = factor(break_colors, 
                               levels = c("#5e4fa2", "#3288bd", 
                                          "#66c2a5", "#abdda4",
                                          "#e6f598", "#ffffbf",
                                          "#fee08b", "#fdae61", 
                                          "#f46d43", "#d53e4f",
                                          "#9e0142"))
         )
table(ls8_jul4_geo_cvl$dev_breaks)

leaflet(data = ls8_jul4_geo_cvl) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = ls8_jul4_geo_cvl,
              fillColor = ~break_colors,
              label = ~paste(cntyname, round(med_dev,2), sep = ", "),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              #              popup = paste0("Degrees above Average: ", round(ls8_jul4_geo$med_dev,2))
  ) %>% 
  addLegend(position = "topright", 
            labels = levels(ls8_jul4_geo_cvl$dev_breaks),
            colors = levels(ls8_jul4_geo_cvl$break_colors), 
            title = "Compared to Average",
            group = "Hide/Show Legend") %>% 
  addLayersControl(overlayGroups = c("Hide/Show Legend"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright")

# join to jul 4
nlcd <- read_csv("data-csv/nlcd_cville_blkgps.csv")
nlcd <- nlcd %>% mutate(GEOID = as.character(GEOID))

ls8_jul4_nlcd <- ls8_jul4_geo_cvl %>% 
  left_join(nlcd, by = "GEOID")

ls8_jul4_nlcd %>% 
  ggplot(aes(x = tree_can, y = med_dev, color = med_dev)) +
  geom_point() +
  labs(x = "% Tree Canopy", y = "Land Surface Temp (Temp - Region Median)") +
  scale_color_distiller(name = "", palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "bottom")

ls8_jul4_nlcd %>% 
  ggplot(aes(x = imp_surf, y = med_dev, color = med_dev)) +
  geom_point() +
  labs(x = "% Impervious Surface", y = "Land Surface Temp (Temp - Region Median)") +
  scale_color_distiller(name = "", palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "bottom")


acs <- read_csv("data-csv/acs_cville_blkgp.csv")
acs <- acs %>% mutate(GEOID = as.character(GEOID))

# join to jul 4
ls8_jul4_acs <- ls8_jul4_nlcd %>% 
  left_join(acs, by = "GEOID")

# scatterplot
p1 <- ls8_jul4_acs %>% 
  ggplot(aes(x = hhincE, y = med_dev, color = med_dev)) + 
  geom_point() +
  labs(x = "Median HH Income", y = "Land Surface Temp (Temp - Region Median)") +
  scale_color_distiller(name = "", palette = "Spectral") +
  scale_x_continuous(labels = label_dollar(), breaks = c(25000, 50000, 75000, 100000, 125000)) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- ls8_jul4_acs %>% 
  ggplot(aes(x = homeownE, y = med_dev, color = med_dev)) + 
  geom_point() +
  labs(x = "% Owning Home", y = "") +
  scale_color_distiller(name = "", palette = "Spectral") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

p3 <- ls8_jul4_acs %>% 
  ggplot(aes(x = age17E, y = med_dev, color = med_dev)) + 
  geom_point() +
  labs(x = "% Age 17 or Younger", y = "") +
  scale_color_distiller(name = "", palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "bottom")

p4 <- ls8_jul4_acs %>% 
  ggplot(aes(x = age65E, y = med_dev, color = med_dev)) + 
  geom_point() +
  labs(x = "% Age 65 or Older", y = "") +
  scale_color_distiller(name = "", palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "bottom")

p1 <- ls8_jul4_acs %>% 
  ggplot(aes(x = whiteE, y = med_dev, color = med_dev)) + 
  geom_point() +
  labs(x = "% White", y = "") +
  scale_color_distiller(name = "", palette = "Spectral") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- ls8_jul4_acs %>% 
  ggplot(aes(x = blackE, y = med_dev, color = med_dev)) + 
  geom_point() +
  labs(x = "% Black", y = "") +
  scale_color_distiller(name = "", palette = "Spectral") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

p3 <- ls8_jul4_acs %>% 
  ggplot(aes(x = asianE, y = med_dev, color = med_dev)) + 
  geom_point() +
  labs(x = "% Asian", y = "") +
  scale_color_distiller(name = "", palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "bottom")

p4 <- ls8_jul4_acs %>% 
  ggplot(aes(x = ltnxE, y = med_dev, color = med_dev)) + 
  geom_point() +
  labs(x = "% Hispanic", y = "") +
  scale_color_distiller(name = "", palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "bottom")
