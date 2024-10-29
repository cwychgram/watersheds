ws_choices <- reactive({
  if (input$select_type == "All") {
    ws <- ws
  } else {
    ws <- ws %>%
      filter(TYPE == input$select_type)
  }
  if (input$select_agro == "All") {
    ws <- ws
  } else {
    ws <- ws %>%
      filter(AGRO == input$select_agro)
  }
  if (input$select_woreda == "All") {
    ws <- ws
  } else {
    ws <- ws %>%
      filter(WOREDA == input$select_woreda)
  }
  if (nrow(ws) != 0) {
    return(ws$WATERSHED)
  } else {
    return("No watersheds!")
  }
})

observe({
  req(ws_choices())
  updateSelectInput(session = session,
                    inputId = "select_ws",
                    choices = sort(ws_choices()))
})

observeEvent(input$select_ws, {
  agro <- ws$AGRO[ws$WATERSHED == input$select_ws]
  if (agro == "Lowland") {
    output$select_mo_opt <- renderUI({
      radioButtons("select_mo",
                   label = "Select a month:",
                   choices = c("April", "October"),
                   selected = "April",
                   width = "100%",
                   inline = TRUE)
    })
  } else if (agro == "Midland") {
    output$select_mo_opt <- renderUI({
      radioButtons("select_mo",
                   label = "Select a month:",
                   choices = c("June", "November"),
                   selected = "June",
                   width = "100%",
                   inline = TRUE)
    })
  } else {
    output$select_mo_opt <- renderUI({
      radioButtons("select_mo",
                   label = "Select a month:",
                   choices = c("June", "December"),
                   selected = "June",
                   width = "100%",
                   inline = TRUE)
    })
  }
  output$ws_name <- renderText({ 
    if(input$select_ws != "No watersheds!") {
      if (input$select_ws == "Ali-Elemo (Chinakson)") {
        paste("Chinakson Ali-Elemo")
      } else if (input$select_ws == "Ali-Elemo (Jarso)") {
        print("Jarso Ali-Elemo")
      } else if (input$select_ws == "Urji (Chinakson)") {
        print("Chinakson Urji")
      } else if (input$select_ws == "Urji (Midhega Tola)") {
        print("Midhega Tola Urji")
      } else {
        woreda <- ws$WOREDA[ws$WATERSHED == input$select_ws]
        paste(woreda, input$select_ws)
      }
    } else {
      paste("No watershed selected.")
    }
  })
  ws2map <- ws %>%
    filter(WATERSHED == input$select_ws)
  if (nrow(ws2map) != 0) {
    output$map_lc <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addScaleBar() %>%
        addResetMapButton() %>%
        clearShapes() %>%
        fitBounds(lng1 = unname(st_bbox(ws2map)$xmin),
                  lat1 = unname(st_bbox(ws2map)$ymin),
                  lng2 = unname(st_bbox(ws2map)$xmax),
                  lat2 = unname(st_bbox(ws2map)$ymax)
        )
    })
    output$map_ndvi <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addScaleBar() %>%
        addResetMapButton() %>%
        clearShapes() %>%
        fitBounds(lng1 = unname(st_bbox(ws2map)$xmin),
                  lat1 = unname(st_bbox(ws2map)$ymin),
                  lng2 = unname(st_bbox(ws2map)$xmax),
                  lat2 = unname(st_bbox(ws2map)$ymax)
        )
    })
  } else {
    # showModal(modalDialog(
    #   "No watersheds!",
    #   size = "s",
    #   easyClose = TRUE
    # )) 
  }
}, ignoreInit = TRUE)