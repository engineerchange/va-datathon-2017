#FIPS Maps



strategyMapServer <- function(input, output, session){
  ns <- session$ns
  all.drugs.pal <- makePalette(DATASETS$maps.df[["2016"]]$`All Drugs`)
  # all.opioids.pal <- makePalette(DATASETS$maps.df[["2016"]]$`All Opioids`)
  # heroin.pal <- makePalette(DATASETS$maps.df[["2016"]]$`Heroin`)
  # prescription.pal <- makePalette(DATASETS$maps.df[["2016"]]$`Prescription`)
  # fentanyl.pal <- makePalette(DATASETS$maps.df[["2016"]]$Fentanyl)

  
  strategyClustering <- eventReactive(input$queryAPI,{
    req(input$queryAPI)
    # click <- input$strategyMap_shape_click
    # 
    # if (is.null(click))
    #   return()
    # 
    # click.id <- click$id
    #take out the 3 letters added to the start of GEOID to create the layerID
    # tmp <- gsub("[a-z][a-z][a-z]","",click.id)
    
    feature.array <- isolate(input$selectFeatures)
    cluster.number <- isolate(input$clusterNumber)
    
    ##DO API CALL / GET and return result below
    
  })
  
  strategy.pal <- reactive({
    makePalette(strategyClustering()$COLUMN)
  })
  
  
  strategySelection <- eventReactive(input$strategyMap_shape_click,{
    req(input$strategyMap_shape_click)
    click <- input$strategyMap_shape_click
    
    if (is.null(click))
      return()

    click.id <- click$id
    #take out the 3 letters added to the start of GEOID to create the layerID
    tmp <- gsub("[a-z][a-z][a-z]","",click.id)

    tmp
  })

output$strategyMap <- renderLeaflet({
  print("inside strategyMap")
  # fatal.pal <- fatalPalette()
  # alldrugs.pal <- fatalPalette(maps.df[["2016"]]$`All Drugs`)
  # allopioids.pal <- fatalPalette(maps.df[["2016"]]$`All Opioids`)
  
  fips.pop <- paste0("<strong>County: </strong>", 
                     DATASETS$maps.df[["2015"]]$NAME,
         "<br><strong>FIPS: </strong>",
         DATASETS$maps.df[["2015"]]$GEOID,
         "<br><strong>CSB: </strong>",
         DATASETS$maps.df[["2015"]]$CSBName)

  
  tmp <- DATASETS$maps.df[[as.character(input$yearFIPS)]] %>%
    leaflet() %>%
    # addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(
      layerId=~paste0("fad",GEOID),
      group = "All Drugs",
      weight = 1,
      color = "#b2aeae",
      fillOpacity = 0.8,
      popup = fips.pop,
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
      popup = fips.pop,
      color = "#b2aeae",
      fillOpacity = 0.8,
      fillColor = ~all.opioids.pal$pal(`All Opioids`),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 3,
        # bringToFront = TRUE,
        sendToBack = TRUE)) %>%

    addLayersControl(
      baseGroups = c("All Drugs", "All Opioids","Heroin","Prescriptions","Fentanyl"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
    
  # %>%
  # addLegend(pal = colorFactor(~CSBName,DATASETS$csb.ogr@data$CSBName),value = ~CSBName)
  
  tmp
  
})






# Changes in the group selection trigger a change in the legend
observeEvent(input$strategyMap_groups,{
  
  if (is.null(input$strategyMap_groups))
    return()
  
  
  
  strategyMap <- leafletProxy("strategyMap", session=session) %>% 
    removeControl("fipsLegend")
  
  if (input$strategyMap_groups == 'All Drugs'){
    strategyMap <- strategyMap %>%
      addLegend(pal = all.drugs.pal$pal,
                values = DATASETS$maps.df[["2016"]]$`All Drugs`,
                position = "bottomright",
                title = "All Drugs",
                layerId = "fipsLegend",
                labels = all.drugs.pal$label
      ) 
  }
  else if (input$strategyMap_groups == 'All Opioids'){
    strategyMap <- strategyMap %>%
      addLegend(pal = all.opioids.pal$pal,
                values = DATASETS$maps.df[["2016"]]$`All Opioids`,
                position = "bottomright",
                title = "All Opioids",
                layerId = "fipsLegend",
                labels = all.opioids.pal$label
      ) 
  }


  
})

# Output for printing the stripped click$id (i.e. the GEOCODE of the selected county) (Testing)
output $ clickText <- renderPrint({
  strategySelection()
})



list(
  
  
  "strategySelection" = strategySelection
)


}




strategyMapUI <- function(id){
  ns <- NS(id)
  ui.input <- tagList()
  
  ui.input$clusterNumber <- numericInput(
    inputId = ns("clusterNumber"),
    min = 1,
    max=12,
    value = 3,
    step = 1,
    label = "Select Number of Clusters:")
  
  ui.input$selectFeatures <- 
    selectizeInput(
      inputId = ns("selectFeatures"),
      multiple = TRUE,
      selected = NULL,
      choices = c("Cat","Hat","Communist"),
      label = "Select Features for Clustering:"
      
    )
  ui.input$queryAPI <- actionButton(
    inputId = ns("queryAPI"),label = "Cluster!",icon = icon("hurricane",lib = "glyphicon")
    
  )
    
  return(ui.input)
  
  
  
}
