library(dplyr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(sf)
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
