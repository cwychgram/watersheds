observeEvent(input$select_ws, {
  ws2map <- ws %>%
    filter(MERGE_SRC == input$select_ws)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      addScaleBar() %>%
      addResetMapButton() %>%
      clearShapes() %>%
      fitBounds(lng1 = unname(st_bbox(ws2map)$xmin),
                lat1 = unname(st_bbox(ws2map)$ymin),
                lng2 = unname(st_bbox(ws2map)$xmax),
                lat2 = unname(st_bbox(ws2map)$ymax)
      ) %>%
      addPolygons(data = ws2map,
                  label = ~MERGE_SRC,
                  color = "#000000", 
                  opacity = 1,
                  weight = 2, 
                  fillOpacity = 0)
      
  })
  
  observeEvent(input$select_lulc, {
    lulc_type <-  reactive({
      lulc %>%
        filter(LULC %in% input$select_lulc & CW %in% input$select_ws)
    })
    
    pal_lulc <- colorFactor(
      palette = c("#397d49","#88B053","#e49635","#dfc35a","#c4281b","#a59b8f"),
      domain = lulc$LULC,
    )
    
    if(!is.null(input$select_lulc) & nrow(lulc_type()) > 0) {
      leafletProxy("map", session) %>%
        clearGroup(group = "LULC") %>%
        removeControl(layerId = "LULCLegend") %>%
        addPolygons(data = lulc_type(),
                    label = ~LULC,
                    color = ~pal_lulc(LULC),
                    fillOpacity = 1,
                    stroke = FALSE,
                    group = "LULC") %>%
        addLegend(position = "topright",
                  pal = pal_lulc,
                  values = lulc_type()$LULC,
                  title = "LULC Type",
                  opacity = 1,
                  layerId = "LULCLegend"
        )
    } else {
      leafletProxy("map", session) %>%
        clearGroup(group = "LULC") %>%
        removeControl(layerId = "LULCLegend")
    }
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$select_crop, {
    crop_type <-  reactive({
      crops %>%
        filter(Crop %in% input$select_crop 
               # & MERGE_SRC %in% input$select_ws
               )  
    })
    
    pal_crops <- colorFactor(
      palette = topo.colors(8),
      domain = crops$Crop,
    )
    
    if(!is.null(input$select_crop) & nrow(crop_type()) > 0) {
      leafletProxy("map", session) %>%
        clearGroup(group = "Crops") %>%
        removeControl(layerId = "CropLegend") %>%
        addCircleMarkers(data = crop_type(),
                    label = ~Crop,
                    radius = 5,
                    color = ~pal_crops(Crop),
                    fillOpacity = 1,
                    stroke = FALSE,
                    group = "Crops") %>%
        addLegend(position = "topright",
                  pal = pal_crops,
                  values = crop_type()$Crop,
                  title = "Crop Type",
                  opacity = 1,
                  layerId = "CropLegend"
        )
    } else {
      leafletProxy("map", session) %>%
        clearGroup(group = "Crops") %>%
        removeControl(layerId = "CropLegend")
    }
    
  }, ignoreNULL = FALSE)
  
  output$pie <- renderPlotly({
    ws2pie <- ws_df %>%
      filter(MERGE_SRC == input$select_ws & YEAR == 2020)
    
    p <- plot_ly(ws2pie, 
                 labels = ~LU_CLASS, 
                 values = ~LU_PCT, 
                 textinfo = "percent",
                 hoverinfo = "text",
                 text = paste(ws2pie$LU_CLASS,
                              "<br>",
                              ws2pie$LU_PCT, "%"),
                 marker = list(colors = c("#5475a8", "#b50000", "#38814e")),
                 sort = FALSE,
                 type = "pie")
    
    p
  })
  
  output$graph <- renderPlotly({
    ws2graph <- ws_df %>%
      filter(MERGE_SRC == input$select_ws & LU_CLASS == "Vegetation") 
    
    x <- list(
      title = "<b>Year</b>"
    )
    y <- list(
      title = "<b>% Vegetation</b>",
      tickformat = "digits"
    )

    p <- plot_ly(ws2graph, 
                 x = ~YEAR,
                 y = ~LU_PCT,
                 line = list(color = c("#38814e")),
                 type = "scatter",
                 mode = "lines") %>%
      layout(xaxis = x, yaxis = y)
    p
  })
})

