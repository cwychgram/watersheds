source("tabs/ui_db.R", local = TRUE)

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
    tabPanel(title = "Dashboard", ui_db, value = "ui_db"),
    tabPanel(title = "About", value = "ui_about")
  )
)