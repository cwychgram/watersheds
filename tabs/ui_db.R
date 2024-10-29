ui_db <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons("select_type", 
                   label = "Select an intervention type:",
                   choices = c("All", "Learning", "Control"), 
                   selected = "All",
                   width = "100%",
                   inline = TRUE),
      radioButtons("select_agro", 
                   label = "Select an agroecology type:",
                   choices = c("All", "Lowland", "Midland", "Highland"), 
                   selected = "All",
                   width = "100%",
                   inline = TRUE),
      selectInput("select_woreda",
                  label = "Select a woreda:",
                  choices = c("All", "Babile", "Chinakson", 
                              "Deder", "Fedis", "Gursum", 
                              "Jarso", "Melkabelo", "Meta", 
                              "Midhega Tola"),
                  selected = "All",
                  width = "100%"
      ),
      selectInput("select_ws",
                  label = "Select a watershed:",
                  choices = NULL,
                  width = "100%"
      ),
      sliderTextInput("select_yr",
                      label = "Select a year:",
                      choices = 2018:2023,
                      selected = 2023,
                      grid = TRUE,
                      width = "100%"
      ),
      uiOutput("select_mo_opt")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        h4(textOutput("ws_name")),
        hr(),
        column(
          width = 6,
          h5("Land Cover"),
          leafletOutput("map_lc")
        ),
        column(
          width = 6,
          h5("NDVI"),
          leafletOutput("map_ndvi")
        ),
      )
    )
  )
)