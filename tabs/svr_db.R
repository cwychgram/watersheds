output$map_loc <- renderLeaflet({
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    clearGroup("selected_ws") %>%
    fitBounds(lng1 = unname(st_bbox(ws)$xmin),
              lat1 = unname(st_bbox(ws)$ymin),
              lng2 = unname(st_bbox(ws)$xmax),
              lat2 = unname(st_bbox(ws)$ymax)
    ) %>%
    addPolygons(
      data = ws,
      label = paste(ws$WOREDA, ws$WATERSHED, sep = " "),
      color = "#1a242f",
      stroke = FALSE,
      weight = 1.0,
      smoothFactor = 1,
      opacity = 1.0,
      fillOpacity = 1.0,
      fillColor = "#18bc9c",
    )
})

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

observe({
  agro <- ws$AGRO[ws$WATERSHED == input$select_ws]
  if (length(agro) != 0) {
    if (agro == "Lowland") {
      updateRadioButtons(session, "select_mo",
                         choices = c("April", "October"),
                         inline = TRUE
      )
    } else if (agro == "Midland") {
      updateRadioButtons(session, "select_mo",
                         choices = c("June", "November"),
                         inline = TRUE
      )
    } else {
      updateRadioButtons(session, "select_mo",
                         choices = c("June", "December"),
                         inline = TRUE
      )
    }
  } else {
  } 
})

observeEvent(c(input$select_ws, input$select_mo, input$select_yr), {
  output$ws_name <- renderText({
    if(input$select_ws != "No watersheds!") {
      if (input$select_ws == "Ali-Elemo (Chinakson)") {
        paste("Chinakson Ali-Elemo (Midland)")
      } else if (input$select_ws == "Ali-Elemo (Jarso)") {
        paste("Jarso Ali-Elemo (Lowland)")
      } else if (input$select_ws == "Urji (Chinakson)") {
        paste("Chinakson Urji (Midland)")
      } else if (input$select_ws == "Urji (Midhega Tola)") {
        paste("Midhega Tola Urji (Lowland)")
      } else {
        paste(ws$WOREDA[ws$WATERSHED == input$select_ws], 
              " ",
              input$select_ws,
              " ",
              "(",
              ws$AGRO[ws$WATERSHED == input$select_ws],
              ")",
              sep = ""
        )
      }
    } else {
      paste("No watershed selected.")
    }
  })
  if (input$select_ws == "No watersheds!") {
    leafletProxy("map_loc", session) %>%
      clearGroup("selected_ws") %>%
      fitBounds(lng1 = unname(st_bbox(ws)$xmin),
                lat1 = unname(st_bbox(ws)$ymin),
                lng2 = unname(st_bbox(ws)$xmax),
                lat2 = unname(st_bbox(ws)$ymax)
      ) 
    output$map_lc <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addScaleBar() %>%
        addResetMapButton() %>%
        clearShapes() %>%
        fitBounds(lng1 = unname(st_bbox(ws)$xmin),
                  lat1 = unname(st_bbox(ws)$ymin),
                  lng2 = unname(st_bbox(ws)$xmax),
                  lat2 = unname(st_bbox(ws)$ymax)
        )
    })
    output$map_ndvi <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addScaleBar() %>%
        addResetMapButton() %>%
        clearShapes() %>%
        fitBounds(lng1 = unname(st_bbox(ws)$xmin),
                  lat1 = unname(st_bbox(ws)$ymin),
                  lng2 = unname(st_bbox(ws)$xmax),
                  lat2 = unname(st_bbox(ws)$ymax)
        )
    })
  } else {
    ws2map <- ws %>%
      filter(WATERSHED == input$select_ws)
    leafletProxy("map_loc", session) %>%
      clearGroup("selected_ws") %>%
      fitBounds(lng1 = unname(st_bbox(ws)$xmin),
                lat1 = unname(st_bbox(ws)$ymin),
                lng2 = unname(st_bbox(ws)$xmax),
                lat2 = unname(st_bbox(ws)$ymax)
      ) %>%
      addPolygons(
        data = ws2map,
        label = paste(ws2map$WOREDA, ws2map$WATERSHED, sep = " "),
        color = "#1a242f",
        stroke = FALSE,
        weight = 1,
        smoothFactor = 1,
        opacity = 1.0,
        fillOpacity = 1.0,
        fillColor = "#1a242f",
        group = "selected_ws"
      )
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
        ) %>%
        addPolygons(
          data = ws2map,
          color = "#1a242f",
          stroke = TRUE,
          weight = 2.0,
          smoothFactor = 1,
          opacity = 1.0,
          fillOpacity = 0.0,
          fillColor = NA,
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
        ) %>%
        addPolygons(
          data = ws2map,
          color = "#1a242f",
          stroke = TRUE,
          weight = 2.0,
          smoothFactor = 1,
          opacity = 1.0,
          fillOpacity = 0.0,
          fillColor = NA,
        )
    })
    wsname <- ws$WSNAME[ws$WATERSHED == input$select_ws]
    month <- input$select_mo
    month[month == "April"] <- 4
    month[month == "June"] <- 6
    month[month == "October"] <- 10
    month[month == "November"] <- 11
    month[month == "December"] <- 12
    
    ndvi_filenames <- ndvi_filenames %>%
      filter(WSNAME == wsname,
             MONTHNUM == month,
             YEAR == input$select_yr)
    if (nrow(ndvi_filenames) != 0) {
      ndvi_tif <- paste("data/ndvi/", ndvi_filenames$FILENAME, sep = "")
      ndvi2map <- read_stars(ndvi_tif)
      pal_ndvi <- colorNumeric(palette = "Greens",
                               na.color = "transparent",
                               domain = ndvi2map[[1]])
      leafletProxy("map_ndvi", session) %>%
        clearGroup(group = "NDVI") %>%
        removeControl(layerId = "NDVI_legend") %>%
        addStarsImage(ndvi2map, colors = pal_ndvi, group = "NDVI") %>%
        leaflet::addLegend(pal = pal_ndvi,
                           values = ndvi2map[[1]],
                           opacity = 1,
                           title = "NDVI",
                           layerId = "NDVI_legend")
    } else {
      leafletProxy("map_ndvi", session) %>%
        clearGroup(group = "NDVI") %>%
        removeControl(layerId = "NDVI_legend")
    }
  }
}, ignoreInit = TRUE)