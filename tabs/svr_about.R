output$about_title <- renderText({
  return("About the Project")
})

output$about_text <- renderUI({
  
  HTML(paste0(
    "The Ifaa Project is a five-year Resilience and Food Security Activity (RFSA) implemented by a Catholic Relief Services (CRS)-led consortium in nine districts (woredas) of Eastern Hararghe Zone in the Oromia Region of Ethiopia.",
    "<hr>",
    "Johns Hopkins University (JHU) is the learning partner for the Ifaa Project and supports the project in conducting implementation research to inform program decision making/planning and evaluate the effectiveness of Ifaa interventions.",
    "<hr>",
    "In 2022, the Ifaa Project implemented watershed interventions as part of a multi-sectoral approach to improve food security and resilience in vulnerable rural populations. Integrated Watershed Management+ (IWM+) was carried out in 34 learning watersheds.",
    "<hr>",
    "Using geographic information systems (GIS) and publicly available satellite imagery, vegetation land cover and NDVI (a proxy for soil moisture) were assessed before and after intervention (2018-2023). Control watersheds were identified to conduct a difference-in-difference analysis, which was stratified by agroecology zone to control for agricultural differences based on climate and topography."
    ))
})