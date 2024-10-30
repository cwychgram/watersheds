library(dplyr)
library(leafem)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(stars)
library(stringr)

ws <- st_read("data/ws.shp")
# ws$WATERSHED[duplicated(ws$WATERSHED) == TRUE]
# ws[ws$WATERSHED == "Ali-Elemo", ]
# ws[ws$WATERSHED == "Urji", ]
ws$WATERSHED[ws$WATERSHED == "Ali-Elemo" & ws$WOREDA == "Chinakson"] <- "Ali-Elemo (Chinakson)"
ws$WATERSHED[ws$WATERSHED == "Ali-Elemo" & ws$WOREDA == "Jarso"] <- "Ali-Elemo (Jarso)"
ws$WATERSHED[ws$WATERSHED == "Urji" & ws$WOREDA == "Chinakson"] <- "Urji (Chinakson)"
ws$WATERSHED[ws$WATERSHED == "Urji" & ws$WOREDA == "Midhega Tola"] <- "Urji (Midhega Tola)"

lc_filenames <- read.csv("data/lc_filenames.csv")

ndvi_filenames <- read.csv("data/ndvi_filenames.csv")
