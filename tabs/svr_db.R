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
      labelOptions = labelOptions(textsize = "15px"),
      color = "#1a242f",
      stroke = TRUE,
      weight = 0.25,
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
        paste("Chinakson Ali-Elemo (Learning - Midland)")
      } else if (input$select_ws == "Ali-Elemo (Jarso)") {
        paste("Jarso Ali-Elemo (Learning - Lowland)")
      } else if (input$select_ws == "Urji (Chinakson)") {
        paste("Chinakson Urji (Learning - Midland)")
      } else if (input$select_ws == "Urji (Midhega Tola)") {
        paste("Midhega Tola Urji (Control - Lowland)")
      } else {
        paste(ws$WOREDA[ws$WATERSHED == input$select_ws], 
              " ",
              input$select_ws,
              " ",
              "(",
              ws$TYPE[ws$WATERSHED == input$select_ws],
              " - ",
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
        clearGroup(group = "LC") %>%
        removeControl(layerId = "LC_legend") %>%
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
        clearGroup(group = "NDVI") %>%
        removeControl(layerId = "NDVI_legend") %>%
        fitBounds(lng1 = unname(st_bbox(ws)$xmin),
                  lat1 = unname(st_bbox(ws)$ymin),
                  lng2 = unname(st_bbox(ws)$xmax),
                  lat2 = unname(st_bbox(ws)$ymax)
        )
    })
  } else {
    ws2map <- ws %>%
      filter(WATERSHED == input$select_ws)
    lng <- as.numeric(st_coordinates(st_centroid(ws2map))[ , 1])
    lat <- as.numeric(st_coordinates(st_centroid(ws2map))[ , 2])
    leafletProxy("map_loc", session) %>%
      clearGroup("selected_ws") %>%
      setView(
        lng, 
        lat,
        zoom = 9
        ) %>%
      addPolygons(
        data = ws2map,
        label = paste(ws2map$WOREDA, ws2map$WATERSHED, sep = " "),
        labelOptions = labelOptions(textsize = "15px"),
        color = "#1a242f",
        stroke = TRUE,
        weight = 0.25,
        smoothFactor = 1,
        opacity = 1.0,
        fillOpacity = 1.0,
        fillColor = "#e74c3c",
        group = "selected_ws"
      )
    output$map_lc <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addScaleBar() %>%
        addResetMapButton() %>%
        clearShapes() %>%
        clearGroup(group = "LC") %>%
        removeControl(layerId = "LC_legend") %>%
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
        clearGroup(group = "NDVI") %>%
        removeControl(layerId = "NDVI_legend") %>%
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
    
    lc_filenames <- lc_filenames %>%
      filter(WSNAME == wsname,
             MONTHNUM == month,
             YEAR == input$select_yr)
    if (nrow(lc_filenames) != 0) {
      lc_tif <- paste("data/lc/", lc_filenames$FILENAME, sep = "")
      lc2map <- read_stars(lc_tif)
      pal_lulc <- colorFactor(
        palette = c("#38814e", "#e49635", "#dfc35a", "#b50000", "#d2cdc0"),
        levels = c(1, 3, 4, 5, 6),
        na.color = "transparent",
        domain = lc2map[[1]]
      )
      leafletProxy("map_lc", session) %>%
        clearGroup(group = "LC") %>%
        removeControl(layerId = "LC_legend") %>%
        addStarsImage(lc2map, colors = pal_lulc, group = "LC") %>%
        leaflet::addLegend(colors = c("#38814e", "#e49635", "#dfc35a", "#b50000", "#d2cdc0"),
                           labels = c("Trees", "Crops", "Shrub/Scrub", "Built", "Bare"),
                           opacity = 1,
                           position = "bottomright",
                           title = "Land Cover",
                           layerId = "LC_legend")
    } else {
      leafletProxy("map_lc", session) %>%
        clearGroup(group = "LC") %>%
        removeControl(layerId = "LC_legend")
    }
    
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
                           position = "bottomright",
                           title = "NDVI",
                           layerId = "NDVI_legend")
    } else {
      leafletProxy("map_ndvi", session) %>%
        clearGroup(group = "NDVI") %>%
        removeControl(layerId = "NDVI_legend")
    }
  }
}, ignoreInit = TRUE)

observeEvent(input$select_ws, {
  if (input$select_ws != "No watersheds!") {
    ws2plot <- df %>%
      filter(WATERSHED == input$select_ws)
    if (unique(ws2plot$AGRO) == "Lowland") {
      ws2plot <- ws2plot %>%
        filter(MONTHNAME %in% c("April", "October"))
    } else if (unique(ws2plot$AGRO) == "Midland") {
      ws2plot <- ws2plot %>%
        filter(MONTHNAME %in% c("June", "November"))
    } else {
      ws2plot <- ws2plot %>%
        filter(MONTHNAME %in% c("June", "December"))
    }
    min_mo <- ws2plot$MONTHNAME[min(ws2plot$MONTHNUM)]
    max_mo <- ws2plot$MONTHNAME[max(ws2plot$MONTHNUM)]
    output$plot_lc<-renderPlot({
      ws2plot %>%
        ggplot(aes(x = YEAR, y = VEGETATION_PCT, fill = MONTHNAME, width = .5)) +
        geom_bar(stat = "identity", position=position_dodge(), color = "black") +
        ylim(0, 100) +
        labs(
          x = "Year", y = "Vegetation Cover (%)", fill = "") +
        scale_fill_manual(labels = c(min_mo, max_mo), values = c("#44aa99","#ddcc77")) +
        theme_classic(text = element_text(size = 15, family = "Gentona Book")) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
              legend.position = "bottom")
    })
  } else {
  }
}, ignoreInit = TRUE)