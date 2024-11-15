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
      label = ~LABEL,
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
        return("Chinakson Ali-Elemo")
      } else if (input$select_ws == "Ali-Elemo (Jarso)") {
        return("Jarso Ali-Elemo")
      } else if (input$select_ws == "Urji (Chinakson)") {
        return("Chinakson Urji")
      } else if (input$select_ws == "Urji (Midhega Tola)") {
        return("Midhega Tola Urji")
      } else {
        paste(ws$WOREDA[ws$WATERSHED == input$select_ws], 
              " ",
              input$select_ws,
              sep = "")
      }
    } else {
      return("No watershed selected.")
    }
  })
    output$ws_type_agro <- renderText({
      if(input$select_ws != "No watersheds!") {
        paste(ws$TYPE[ws$WATERSHED == input$select_ws],
              " - ",
              ws$AGRO[ws$WATERSHED == input$select_ws],
              sep = ""
        )
      } else {
      }
    })
  output$ws_area <- renderText({
    if(input$select_ws != "No watersheds!") {
      paste("Area: ", comma(round(ws$AREA_HA[ws$WATERSHED == input$select_ws], 2)), " ha", sep = "")
    } else {
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
        zoom = 10
        ) %>%
      addPolygons(
        data = ws2map,
        label = ~LABEL,
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
          fillColor = NA
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
      filter(WATERSHED == input$select_ws,
             YEAR > 2017) 
    if (unique(ws2plot$AGRO) == "Lowland") {
      ws2plot <- ws2plot %>%
        filter(MONTHNAME %in% c("April", "October"))
      min_mo <- "April"
      max_mo <- "October"
    } else if (unique(ws2plot$AGRO) == "Midland") {
      ws2plot <- ws2plot %>%
        filter(MONTHNAME %in% c("June", "November"))
      min_mo <- "June"
      max_mo <- "November"
    } else {
      ws2plot <- ws2plot %>%
        filter(MONTHNAME %in% c("June", "December"))
      min_mo <- "June"
      max_mo <- "December"
    }
    output$plot_veg <- renderEcharts4r({
      veg <- ws2plot %>%
        dplyr::select(YEAR, MONTHNAME, VEGETATION_PCT) %>%
        mutate(VEGETATION_PCT = round(VEGETATION_PCT, 2)) %>%
        pivot_wider(names_from = MONTHNAME, values_from = VEGETATION_PCT) %>%
        mutate(YEAR = as.factor(YEAR),
               mo_1 = .[[2]],
               mo_2 = .[[3]]) 
      veg %>%
        e_charts(YEAR) %>%
        e_bar(mo_1,
              name = min_mo) %>%
        e_bar(mo_2,
              name = max_mo) %>%
        e_x_axis(
          name = "Year",
          nameLocation = "center",
          nameGap = 28,
          axisLine = list(lineStyle = list(color = "#2c3e50"))) %>%
        e_y_axis(
          name = "Vegetation Cover (%)",
          nameLocation = "center",
          nameGap = 35,
          splitLine = list(lineStyle = list(color = "#ecf0f1"))) %>%
        e_color(
          c("#2c3e50", "#7bbce8")
        ) %>%
        e_text_style(
          color = "#2c3e50",
          fontFamily = "Gentona-Book",
          fontSize = 15
        ) %>%
        e_tooltip(trigger = "axis",
                  textStyle = list(color = "#2c3e50",
                                   fontFamily = "Gentona-Book")
        ) %>%
        e_legend(textStyle = list(color = "#2c3e50",
                                  fontFamily = "Gentona-Book",
                                  fontSize = 15)) %>%
        e_grid(left = "20%", right = "0%") %>%
        e_axis_pointer(lineStyle = list(color = "#bac4c5"))
    })
    output$plot_ndvi <- renderEcharts4r({
      ndvi <- ws2plot %>%
        dplyr::select(YEAR, MONTHNAME, NDVI_MEAN) %>%
        mutate(NDVI_MEAN = round(NDVI_MEAN, 2)) %>%
        pivot_wider(names_from = MONTHNAME, values_from = NDVI_MEAN) %>%
        mutate(YEAR = as.factor(YEAR),
               mo_1 = .[[2]],
               mo_2 = .[[3]]) 
      ndvi %>%
        e_charts(YEAR) %>%
        e_bar(mo_1,
              name = min_mo) %>%
        e_bar(mo_2,
              name = max_mo) %>%
        e_x_axis(
          name = "Year",
          nameLocation = "center",
          nameGap = 28,
          axisLine = list(lineStyle = list(color = "#2c3e50"))) %>%
        e_y_axis(
          name = "NDVI",
          nameLocation = "center",
          nameGap = 35,
          splitLine = list(lineStyle = list(color = "#ecf0f1"))) %>%
        e_color(
          c("#2c3e50", "#7bbce8")
        ) %>%
        e_text_style(
          color = "#2c3e50",
          fontFamily = "Gentona-Book",
          fontSize = 15
        ) %>%
        e_tooltip(trigger = "axis",
                  textStyle = list(color = "#2c3e50",
                                   fontFamily = "Gentona-Book")
        ) %>%
        e_legend(textStyle = list(color = "#2c3e50",
                                  fontFamily = "Gentona-Book",
                                  fontSize = 15)) %>%
        e_grid(left = "20%", right = "0%") %>%
        e_axis_pointer(lineStyle = list(color = "#bac4c5"))
    })
    output$plot_rain <- renderEcharts4r({
      rain <- ws2plot %>%
        dplyr::select(YEAR, MONTHNAME, PRECIP_MEAN) %>%
        mutate(PRECIP_MEAN = round(PRECIP_MEAN, 2)) %>%
        pivot_wider(names_from = MONTHNAME, values_from = PRECIP_MEAN) %>%
        mutate(YEAR = as.factor(YEAR),
               mo_1 = .[[2]],
               mo_2 = .[[3]]) 
      rain %>%
        e_charts(YEAR) %>%
        e_bar(mo_1,
              name = min_mo) %>%
        e_bar(mo_2,
              name = max_mo) %>%
        e_x_axis(
          name = "Year",
          nameLocation = "center",
          nameGap = 28,
          axisLine = list(lineStyle = list(color = "#2c3e50"))) %>%
        e_y_axis(
          name = "Rainfall (mm)",
          nameLocation = "center",
          nameGap = 35,
          splitLine = list(lineStyle = list(color = "#ecf0f1"))) %>%
        e_color(
          c("#2c3e50", "#7bbce8")
        ) %>%
        e_text_style(
          color = "#2c3e50",
          fontFamily = "Gentona-Book",
          fontSize = 15
        ) %>%
        e_tooltip(trigger = "axis",
                  textStyle = list(color = "#2c3e50",
                                   fontFamily = "Gentona-Book")
        ) %>%
        e_legend(textStyle = list(color = "#2c3e50",
                                  fontFamily = "Gentona-Book",
                                  fontSize = 15)) %>%
        e_grid(left = "20%", right = "0%") %>%
        e_axis_pointer(lineStyle = list(color = "#bac4c5"))
    })
  } else {
  }
}, ignoreInit = TRUE)