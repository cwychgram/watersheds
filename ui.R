source("tabs/ui_db.R", local = TRUE)
source("tabs/ui_agro.R", local = TRUE)
source("tabs/ui_about.R", local = TRUE)

ui <- tagList(
  includeCSS("www/styles.css"),
  navbarPage(
    theme = shinytheme("flatly"),
    windowTitle = tags$head(
      tags$link(rel = "shortcut icon", 
                href = "favicon.png",
                type = "image/x-icon"),
      tags$title("RFSA-JHU Watershed Project")
    ),
    title = "RFSA-JHU Watershed Project",
    collapsible = TRUE,
    fluid = TRUE,
    tabPanel(title = "Watershed Plots", ui_db, value = "ui_db"),
    tabPanel(title = "Agroecology Zone Plots", ui_agro, value = "ui_agro"),
    tabPanel(title = "About", ui_about, value = "ui_about")
  )
)