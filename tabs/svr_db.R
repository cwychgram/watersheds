output$map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>% 
    addScaleBar()
})

observeEvent(input$select_ws, {
  ws2map <- ws %>%
    filter(MERGE_SRC == input$select_ws)
  
  leafletProxy("map", session) %>%
    clearShapes() %>%
    fitBounds(lng1 = unname(st_bbox(ws2map)$xmin),
              lat1 = unname(st_bbox(ws2map)$ymin),
              lng2 = unname(st_bbox(ws2map)$xmax),
              lat2 = unname(st_bbox(ws2map)$ymax)
    ) %>%
    addPolygons(data = ws2map,
                label = ~MERGE_SRC)
  
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

