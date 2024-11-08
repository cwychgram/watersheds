server <- function(input, output, session) {
  source("tabs/svr_db.R", local = TRUE)
  source("tabs/svr_agro.R", local = TRUE)
  source("tabs/svr_about.R", local = TRUE)
}