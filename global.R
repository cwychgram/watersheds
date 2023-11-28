library(dplyr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(sf)
library(shiny)
library(shinyWidgets)
library(stringr)
library(tidyr)

ws <- st_read("data/watersheds_RFSA_4326.shp")

ws_df <- ws %>% 
  as.data.frame() %>% 
  select(-c(geometry))

ws_df$YEAR <- "2010 2015 2020"

ws_df <- ws_df %>%
  separate_longer_delim(YEAR, delim = " ")

ws_df$LU_CLASS <- "Water Developed Vegetation"

ws_df <- ws_df %>% 
  rowwise() %>%
  mutate(LU_PCT = list(100 * diff(c(0, sort(runif(2)), 1))),
         LU_PCT = paste(LU_PCT, collapse = " ")) %>%
  as.data.frame()

ws_df <- ws_df %>%
  separate_longer_delim(c(LU_CLASS, LU_PCT), delim = " ")

ws_df$LU_CLASS <- factor(ws_df$LU_CLASS, 
                         levels = c("Water", "Developed", "Vegetation"))

ws_df$LU_PCT <- round(as.numeric(ws_df$LU_PCT), 1)

lulc <- st_read("data/lulc_RFSA_4326.shp")
lulc$LULC <- factor(lulc$LULC, levels = c("Trees",
                                          "Grass",
                                          "Crops",
                                          "Shrub/Scrub",
                                          "Built",
                                          "Bare"))

# ws <- st_make_valid(ws)
# crops <- read.csv("data/crop_point_mapping.csv") %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
#   mutate(Crop = str_replace(Crop, "_", " ")) %>%
#   st_join(ws) %>%
#   st_write("data/crops_RFSA_4326.shp")

crops <- st_read("data/crops_RFSA_4326.shp")
crops$Crop <- factor(crops$Crop, levels = unique(crops$Crop))
