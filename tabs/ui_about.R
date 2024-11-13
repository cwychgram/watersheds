ui_about <- fluidPage(
  fluidRow(
    column(width = 3
    ),
    column(width = 6,
           align = "center",
           h3(textOutput("about_title"))
    ),
    column(width = 3)
  ),
  br(),
  fluidRow(
    column(width = 3
    ),
    column(width = 6,
           align = "justify",
           uiOutput("about_text")
    ),
    column(width = 3)
  )
)