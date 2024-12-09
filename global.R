library(dplyr)
library(echarts4r)
library(leafem)
library(leaflet)
library(leaflet.extras)
library(scales)
library(sf)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(stars)
library(stringr)
library(tidyr)

ws <- st_read("data/ws.shp")
# ws$WATERSHED[duplicated(ws$WATERSHED) == TRUE]
# ws[ws$WATERSHED == "Ali-Elemo", ]
# ws[ws$WATERSHED == "Urji", ]
ws$WATERSHED[ws$WATERSHED == "Ali-Elemo" & ws$WOREDA == "Chinakson"] <- "Ali-Elemo (Chinakson)"
ws$WATERSHED[ws$WATERSHED == "Ali-Elemo" & ws$WOREDA == "Jarso"] <- "Ali-Elemo (Jarso)"
ws$WATERSHED[ws$WATERSHED == "Urji" & ws$WOREDA == "Chinakson"] <- "Urji (Chinakson)"
ws$WATERSHED[ws$WATERSHED == "Urji" & ws$WOREDA == "Midhega Tola"] <- "Urji (Midhega Tola)"
ws$LABEL <- paste(ws$WOREDA, ws$WATERSHED, sep = " ")
ws$LABEL <- str_replace(ws$LABEL, '\\s*[(][^)]+[)]', "")

lc_filenames <- read.csv("data/lc_filenames.csv")

ndvi_filenames <- read.csv("data/ndvi_filenames.csv")

df <- read.csv("data/final_data_20unclass.csv")
df$WATERSHED[df$WATERSHED == "Ali-Elemo" & df$WOREDA == "Chinakson"] <- "Ali-Elemo (Chinakson)"
df$WATERSHED[df$WATERSHED == "Ali-Elemo" & df$WOREDA == "Jarso"] <- "Ali-Elemo (Jarso)"
df$WATERSHED[df$WATERSHED == "Urji" & df$WOREDA == "Chinakson"] <- "Urji (Chinakson)"
df$WATERSHED[df$WATERSHED == "Urji" & df$WOREDA == "Midhega Tola"] <- "Urji (Midhega Tola)"

did <- read.csv("data/did_estimators.csv")
