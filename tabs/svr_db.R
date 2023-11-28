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
    
    pal <- colorFactor(
      palette = c("#397d49","#88B053","#e49635","#dfc35a","#c4281b","#a59b8f"),
      domain = lulc$LULC,
    )
    
    if(!is.null(input$select_lulc)) {
      leafletProxy("map", session) %>%
        clearGroup(group = "LULC") %>%
        addPolygons(data = lulc_type(),
                    label = ~LULC,
                    color = ~pal(LULC),
                    fillOpacity = 1,
                    stroke = FALSE,
                    group = "LULC")
    } else {
      leafletProxy("map", session) %>%
        clearGroup(group = "LULC")
    }
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$select_crop, {
    crop_type <-  reactive({
      crops %>%
        filter(Crop %in% input$select_crop 
               # & MERGE_SRC %in% input$select_ws
               )  
    })
    
    pal <- colorFactor(
      palette = topo.colors(8),
      domain = crops$Crop,
    )
    
    if(!is.null(input$select_crop)) {
      leafletProxy("map", session) %>%
        clearGroup(group = "Crops") %>%
        addCircleMarkers(data = crop_type(),
                    label = ~Crop,
                    radius = 5,
                    color = ~pal(Crop),
                    fillOpacity = 1,
                    stroke = FALSE,
                    group = "Crops")
    } else {
      leafletProxy("map", session) %>%
        clearGroup(group = "Crops")
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

