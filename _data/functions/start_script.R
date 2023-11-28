library(plotly)
library(sf)
library(tidyverse)
library(highcharter)
library(here)
library(leaflet)
library(rnaturalearth)

ohi_scores <- st_read(here("_data/output/scores.shp"), quiet=TRUE)

land <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(sov_a3 %in% c("DJI", "EGY", "ERI", "SOM", "SOL", "ISR", "JOR" , "SAU", "SDN", "YEM"))


reds   <- colorRampPalette(c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090"), space = "Lab")(66)
blues  <- colorRampPalette(c("#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"), space = "Lab")(35)
colors <- c(reds, blues)

### Color Breaks
col.brks     <- seq(0,100, by = 1)
