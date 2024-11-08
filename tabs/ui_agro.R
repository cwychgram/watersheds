ui_agro <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons("select_agro_4agro", 
                   label = "Select an agroecology zone type:",
                   choices = c("Lowland", "Midland", "Highland"), 
                   selected = "Lowland",
                   width = "100%",
                   inline = TRUE),
      radioButtons("select_mo_4agro",
                   label = "Select a month:",
                   choices = c("April",
                               "June", 
                               "October",
                               "November",
                               "December"),
                   width = "100%",
                   inline = TRUE
      )
    ),
    mainPanel(
      width = 9,
      fluidRow(
        h4(textOutput("agro_name")),
        hr(),
        column(
          width = 6,
          h5("Vegetation"),
          echarts4rOutput("plot_agro_veg")
        ),
        column(
          width = 6,
          h5("NDVI"),
          echarts4rOutput("plot_agro_ndvi")
        )
      )
    )
  )
)