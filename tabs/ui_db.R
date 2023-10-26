ui_db <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("select_ws",
                  label = "Select a watershed:",
                  choices = ws$MERGE_SRC,
                  width = "100%"
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