ui_db <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("select_ws",
                  label = "Select a watershed:",
                  choices = ws$MERGE_SRC,
                  width = "100%"
      ),
      pickerInput("select_lulc", 
                  label = "Overlay land use/land cover type(s):", 
                  choices = levels(lulc$LULC), 
                  options = list(`actions-box` = TRUE), 
                  multiple = TRUE
      ),
      pickerInput("select_crop", 
                  label = "Overlay crop type(s):", 
                  choices = levels(crops$Crop),
                  options = list(`actions-box` = TRUE), 
                  multiple = TRUE
      )
    ),
    mainPanel(
      width = 9,
      fluidRow(
        leafletOutput("map")
      ),
      fluidRow(
        column(width = 6,
               plotlyOutput("pie")
        ),
        column(width = 6,
               plotlyOutput("graph")
        )
      ),
      br()
    )
  )
)