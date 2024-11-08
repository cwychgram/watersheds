observeEvent(input$select_agro_4agro, {
  if (input$select_agro_4agro == "Lowland") {
    updateRadioButtons(session, "select_mo_4agro",
                       choices = c("April", "October"),
                       inline = TRUE
    )
  } else if (input$select_agro_4agro == "Midland") {
    updateRadioButtons(session, "select_mo_4agro",
                       choices = c("June", "November"),
                       inline = TRUE
    )
  } else {
    updateRadioButtons(session, "select_mo_4agro",
                       choices = c("June", "December"),
                       inline = TRUE
    )
  }
  output$agro_name <- renderText({
    paste(input$select_agro_4agro, " (", input$select_mo_4agro, ")", sep = "")
  })
})
observeEvent(input$select_mo_4agro, {
  agro2plot <- df %>%
    filter(AGRO == input$select_agro_4agro,
           YEAR > 2017) 
  if (unique(agro2plot$AGRO) == "Lowland") {
    agro2plot <- agro2plot %>%
      filter(MONTHNAME == input$select_mo_4agro)
  } else if (unique(agro2plot$AGRO) == "Midland") {
    agro2plot <- agro2plot %>%
      filter(MONTHNAME == input$select_mo_4agro)
  } else {
    agro2plot <- agro2plot %>%
      filter(MONTHNAME == input$select_mo_4agro)
  }
  agro2plot <- agro2plot%>%
    group_by(TYPE, YEAR, MONTHNAME) %>%
    summarise(VEG = mean(VEGETATION_PCT, na.rm = TRUE),
              NDVI = mean(NDVI_MEAN, na.rm = TRUE)) %>%
    ungroup() %>%
    as.data.frame()
  output$plot_agro_veg <- renderEcharts4r({
    agro_veg <- agro2plot %>%
      dplyr::select(YEAR, TYPE, VEG) %>%
      mutate(VEG = round(VEG, 2)) %>%
      pivot_wider(names_from = TYPE, values_from = VEG) %>%
      mutate(YEAR = as.factor(YEAR)) 
    agro_veg %>%
      e_charts(YEAR) %>%
      e_line(Control,
             name = "Control Watersheds") %>%
      e_line(Learning,
             name = "Learning Watersheds") %>%
      e_x_axis(
        name = "Year",
        nameLocation = "center",
        nameGap = 28,
        axisLine = list(lineStyle = list(color = "#2c3e50"))) %>%
      e_y_axis(
        name = "Vegetation Cover (%)",
        max = 100,
        nameLocation = "center",
        nameGap = 35,
        splitLine = list(lineStyle = list(color = "#ecf0f1"))) %>%
      e_color(
        c( "#f39c12", "#18bc9c")
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
      e_grid(left = "10%", right = "0%") %>%
      e_axis_pointer(lineStyle = list(color = "#bac4c5")) %>%
      e_mark_line(data = list(xAxis = "2022"), 
                  title = "Intervention Start Year",
                  lineStyle = list(color = "#69d3bf"))
  })
  output$plot_agro_ndvi <- renderEcharts4r({
    agro_ndvi <- agro2plot %>%
      dplyr::select(YEAR, TYPE, NDVI) %>%
      mutate(NDVI = round(NDVI, 2)) %>%
      pivot_wider(names_from = TYPE, values_from = NDVI) %>%
      mutate(YEAR = as.factor(YEAR)) 
    agro_ndvi %>%
      e_charts(YEAR) %>%
      e_line(Control,
             name = "Control Watersheds") %>%
      e_line(Learning,
             name = "Learning Watersheds") %>%
      e_x_axis(
        name = "Year",
        nameLocation = "center",
        nameGap = 28,
        axisLine = list(lineStyle = list(color = "#2c3e50"))) %>%
      e_y_axis(
        name = "NDVI",
        max = .6,
        nameLocation = "center",
        nameGap = 35,
        splitLine = list(lineStyle = list(color = "#ecf0f1"))) %>%
      e_color(
        c("#f39c12", "#18bc9c")
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
      e_grid(left = "10%", right = "0%") %>%
      e_axis_pointer(lineStyle = list(color = "#bac4c5")) %>%
      e_mark_line(data = list(xAxis = "2022"), 
                  title = "Intervention Start Year",
                  lineStyle = list(color = "#69d3bf"))
  })
})