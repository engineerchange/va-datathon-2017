#FIPS Maps



fipsMapServer <- function(input, output, session){
  ns <- session$ns
  all.drugs.pal <- makePalette(maps.df[["2016"]]$`All Drugs`)
  all.opioids.pal <- makePalette(maps.df[["2016"]]$`All Opioids`)
  heroin.pal <- makePalette(maps.df[["2016"]]$`Heroin`)
  prescription.pal <- makePalette(maps.df[["2016"]]$`Prescription`)
  
  # fatalPalette <- reactive({
  #   req(input$fipsMap_groups)
  #   makePalette(maps.df[["2016"]][,c(input$fipsMap_groups)])
  #   
  # })
  
  
  fipsSelection <- eventReactive(input$fipsMap_shape_click,{
    req(input$fipsMap_shape_click)
    click <- input$fipsMap_shape_click
    
    if (is.null(click))
      return()

    click.id <- click$id
    #take out the 3 letters added to the start of GEOID to create the layerID
    tmp <- gsub("[a-z][a-z][a-z]","",click.id)

    tmp
  })

output$fipsMap <- renderLeaflet({
  print("inside fipsMap")
  # fatal.pal <- fatalPalette()
  # alldrugs.pal <- fatalPalette(maps.df[["2016"]]$`All Drugs`)
  # allopioids.pal <- fatalPalette(maps.df[["2016"]]$`All Opioids`)
  
  print(names(DATASETS$maps.df))
  tmp <- DATASETS$maps.df[[as.character(input$yearFIPS)]] %>%
    leaflet() %>%
    # addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(
      layerId=~paste0("fad",GEOID),
      group = "All Drugs",
      weight = 1,
      color = "#b2aeae",
      fillOpacity = 0.6,
      fillColor = ~all.drugs.pal$pal(`All Drugs`),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 3,
        # bringToFront = TRUE,
        sendToBack = TRUE)) %>%
    # addLegend(pal = all.drugs.pal$pal,
    #           values = maps.df[["2016"]]$`All Drugs`,
    #           position = "bottomright",
    #           title = "All Drugs",
    #           layerId = "deathLegend",
    #           labels = all.drugs.pal$label
    # ) %>%
    addPolygons(
      layerId=~paste0("fao",GEOID),
      group = "All Opioids",
      weight = 1,
      color = "#b2aeae",
      fillOpacity = 0.6,
      fillColor = ~all.opioids.pal$pal(`All Opioids`),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 3,
        # bringToFront = TRUE,
        sendToBack = TRUE)) %>%
    addPolygons(
      layerId=~paste0("fhe",GEOID),
      group = "Heroin",
      weight = 1,
      color = "#b2aeae",
      fillOpacity = 0.6,
      fillColor = ~heroin.pal$pal(`Heroin`),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 3,
        # bringToFront = TRUE,
        sendToBack = TRUE)) %>%
    addPolygons(
      layerId=~paste0("fpr",GEOID),
      group = "Prescriptions",
      weight = 1,
      color = "#b2aeae",
      fillOpacity = 0.6,
      fillColor = ~prescription.pal$pal(`Prescription`),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 3,
        # bringToFront = TRUE,
        sendToBack = TRUE)) %>%
    addLayersControl(
      baseGroups = c("All Drugs", "All Opioids","Heroin","Prescriptions"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
    
  # %>%
  # addLegend(pal = colorFactor(~CSBName,DATASETS$csb.ogr@data$CSBName),value = ~CSBName)
  
  tmp
  
})






# Changes in the group selection trigger a change in the legend
observeEvent(input$fipsMap_groups,{
  
  if (is.null(input$fipsMap_groups))
    return()
  
  
  
  fipsMap <- leafletProxy("fipsMap", session=session) %>% 
    removeControl("fipsLegend")
  
  if (input$fipsMap_groups == 'All Drugs'){
    fipsMap <- fipsMap %>%
      addLegend(pal = all.drugs.pal$pal,
                values = maps.df[["2016"]]$`All Drugs`,
                position = "bottomright",
                title = "All Drugs",
                layerId = "fipsLegend",
                labels = all.drugs.pal$label
      ) 
  }
  else if (input$fipsMap_groups == 'All Opioids'){
    fipsMap <- fipsMap %>%
      addLegend(pal = all.opioids.pal$pal,
                values = maps.df[["2016"]]$`All Opioids`,
                position = "bottomright",
                title = "All Opioids",
                layerId = "fipsLegend",
                labels = all.opioids.pal$label
      ) 
  }
  else if (input$fipsMap_groups == 'Heroin'){
    fipsMap <- fipsMap %>%
      addLegend(pal = heroin.pal$pal,
                values = maps.df[["2016"]]$`Heroin`,
                position = "bottomright",
                title = "Heroin",
                layerId = "fipsLegend",
                labels = heroin.pal$label
      )   }
  else if (input$fipsMap_groups == 'Prescriptions'){
    fipsMap <- fipsMap %>%
      addLegend(pal = prescription.pal$pal,
                values = maps.df[["2016"]]$`Prescription`,
                position = "bottomright",
                title = "Prescriptions",
                layerId = "fipsLegend",
                labels = prescription.pal$label
      ) 
    
  }
  
})

# Output for printing the stripped click$id (i.e. the GEOCODE of the selected county) (Testing)
output $ clickText <- renderPrint({
  fipsSelection()
})



list(
  
  
  "fipsSelection" = fipsSelection
)


}




fipsMapUI <- function(id){
  ns <- NS(id)
  
  radioButtons(
    inputId = ns("yearFIPS"),
    choices = 2012:2016,
    label = "Select Year:",
    selected = 2015)
  
  
  
}
