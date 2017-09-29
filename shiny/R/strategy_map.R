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
    # print("inside")
    feature.array <- isolate(input$selectFeatures)
    cluster.number <- isolate(input$clusterNumber)
    
    ##DO API CALL / GET and return result below
    path <- 'http://localhost:5000'
    if(length(feature.array)>1 & !is.numeric(cluster.number)){
      path <- paste0(path,"?","subset=['",glue::collapse(feature.array,sep="' '"),"']")
    
    } else   if(length(feature.array)>1){
      path <- paste0(path,
                     "?","subset=['",glue::collapse(feature.array,sep="' '"),"']",
                     "&n_clusters=",cluster.number)

      
  } else {
      path <- paste0(path,"?n_clusters=",cluster.number)
    }
    
    # clustering <- simplifyDataFrame(tmp)
    print(path)
    clustering  <- fromJSON(path)
    # print(clustering)
    clustering
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
  
  output$test <- renderTable({
    print(strategyClustering())
  })
  
  
  mapDf <- reactive({
    req(strategyClustering())
    
    
    map.df <- merge(DATASETS$maps.df[["2016"]],
                    strategyClustering(),
                    by.x="GEOID",
                    by.y="fips")
    map.df
  })

output$strategyMap <- renderLeaflet({
  input$queryAPI
  print("inside strategyMap")

# 
#   
#   map.df <- merge(DATASETS$maps.df[["2016"]],
#                   strategyClustering(),
#                   by.x="GEOID",
#                   by.y="fips")
  

    #Function to construct a risk palette with specific shared style choices
  cluster.number <- input$clusterNumber
    strategy.pal <- colorFactor(palette = "Dark2",domain = 0:cluster.number) 

  
  
   # strategy.pal <- makePalette(map.df@data$cluster) 
  # fatal.pal <- fatalPalette()
  # alldrugs.pal <- fatalPalette(maps.df[["2016"]]$`All Drugs`)
  # allopioids.pal <- fatalPalette(maps.df[["2016"]]$`All Opioids`)
  
  fips.pop <- paste0("<strong>County: </strong>", 
                     mapDf()$NAME,
         "<br><strong>FIPS: </strong>",
         mapDf()$GEOID,
         "<br><strong>CSB: </strong>",
         mapDf()$CSBName)

  
  tmp <- mapDf() %>%
    leaflet() %>% 
    # addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(
      # layerId=~paste0("fad",GEOID),
      # group = "Clustering",
      weight = 1,
      color = "#b2aeae",
      fillOpacity = 0.8,
      popup = fips.pop,
      fillColor = ~strategy.pal(`cluster`),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 3,
        # bringToFront = TRUE,
        sendToBack = TRUE))  %>%
    addLegend(pal = strategy.pal,
              values = mapDf()$cluster,
              position = "bottomright",
              title = "Cluster",
              layerId = "deathLegend"
    )
    # addPolygons(
    #   layerId=~paste0("fao",GEOID),
    #   group = "All Opioids",
    #   weight = 1,
    #   popup = fips.pop,
    #   color = "#b2aeae",
    #   fillOpacity = 0.8,
    #   fillColor = ~all.opioids.pal$pal(`All Opioids`),
    #   highlightOptions = highlightOptions(
    #     color = "white",
    #     weight = 3,
    #     # bringToFront = TRUE,
    #     sendToBack = TRUE)) %>%
    # 
    # addLayersControl(
    #   baseGroups = c("All Drugs", "All Opioids","Heroin","Prescriptions","Fentanyl"),
    #   options = layersControlOptions(collapsed = FALSE)
    # )
  
    
  # %>%
  # addLegend(pal = colorFactor(~CSBName,DATASETS$csb.ogr@data$CSBName),value = ~CSBName)
  
  tmp
  
})
# 
# observe({
#   mapDf()
#   cluster.number <- isolate(input$clusterNumber)
#   strategy.pal <- colorFactor(palette = "Dark2",domain = 0:cluster.number) 
#    leafletProxy("stategyMap") %>% clearShapes() %>%
#      addPolygons(
#        layerId=~paste0("fxx",GEOID),
#        group = "Clustering",
#        weight = 1,
#        color = "#b2aeae",
#        fillOpacity = 0.8,
#        popup = fips.pop,
#        fillColor = ~strategy.pal(`cluster`),
#        highlightOptions = highlightOptions(
#          color = "white",
#          weight = 3,
#          # bringToFront = TRUE,
#          sendToBack = TRUE))  %>%
#      addLegend(pal = strategy.pal,
#                values = mapDf()$cluster,
#                position = "bottomright",
#                title = "Cluster",
#                layerId = "deathLegend"
#      )
#  
# 
# 
# 
# # strategy.pal <- makePalette(map.df@data$cluster) 
# # fatal.pal <- fatalPalette()
# # alldrugs.pal <- fatalPalette(maps.df[["2016"]]$`All Drugs`)
# # allopioids.pal <- fatalPalette(maps.df[["2016"]]$`All Opioids`)
# 
# })
# 


# 
# # Changes in the group selection trigger a change in the legend
# observeEvent(input$strategyMap_groups,{
#   
#   if (is.null(input$strategyMap_groups))
#     return()
#   
#   
#   
#   strategyMap <- leafletProxy("strategyMap", session=session) %>% 
#     removeControl("fipsLegend")
#   
#   if (input$strategyMap_groups == 'All Drugs'){
#     strategyMap <- strategyMap %>%
#       addLegend(pal = all.drugs.pal$pal,
#                 values = DATASETS$maps.df[["2016"]]$`All Drugs`,
#                 position = "bottomright",
#                 title = "All Drugs",
#                 layerId = "fipsLegend",
#                 labels = all.drugs.pal$label
#       ) 
#   }

  


  
# })

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
