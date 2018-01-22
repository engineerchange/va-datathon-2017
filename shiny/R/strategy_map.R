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
    if(length(feature.array)>0 & !is.numeric(cluster.number)){
      path <- paste0(path,"?","subset=%5B'",glue::collapse(gsub(" ","%20",feature.array),sep="','"),"'%5D")
    
    } else   if(length(feature.array)>0){
      path <- paste0(path,
                     "?",
                     "n_clusters=",cluster.number,"&","subset=%5B'",glue::collapse(gsub(" ","%20",feature.array),sep="','"),"'%5D")

      
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

strategyMapPresentation <- function(id){
  ns <- NS(id)
  ui.input <- strategyMapUI(id)
  presentation <- fluidRow(
    column(width=2,
           ui.input$clusterNumber),
    column(width=8,
           ui.input$selectFeatures),
    column(width=2,
           ui.input$queryAPI)
  )
  presentation
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
      choices = c('Opioid Rate per 100 per year',
                  'Diagnosed Hiv', 'ED Heroin Overdose', 'ED Opioid Overdose',
                  'Ems Narcan', 'Death Fentanylheroin', 'Death Prescription',
                  'Neonatal Abstinance', 'Reported Hepatitis C',
                  'Opioid Cost per Claim', 'Opioid Daily Supply per Claim',
                  'Poverty Status (Last 12 months)', 'Disability (Percent Household)',
                  'Food Stamps(Percent Household)',
                   'Percent Below Poverty Level Estimate Population For Whom Poverty Status Is Determined',
                   #'Percent Below Poverty Level Estimate White Alone, Not Hispanic Or Latino',
                   #'Total Estimate All Individuals With Income Below The Following Poverty Ratios 50 Percent Of Poverty Level',
                   #'Total Estimate All Individuals With Income Below The Following Poverty Ratios 125 Percent Of Poverty Level',
                   #'Total Estimate All Individuals With Income Below The Following Poverty Ratios 150 Percent Of Poverty Level',
                   #'Total Estimate All Individuals With Income Below The Following Poverty Ratios 185 Percent Of Poverty Level',
                   #'Total Estimate All Individuals With Income Below The Following Poverty Ratios 200 Percent Of Poverty Level',
                   #'Total Estimate All Individuals With Income Below The Following Poverty Ratios 300 Percent Of Poverty Level',
                   #'Total Estimate All Individuals With Income Below The Following Poverty Ratios 400 Percent Of Poverty Level',
                   #'Total Estimate All Individuals With Income Below The Following Poverty Ratios 500 Percent Of Poverty Level',
                   'Percent Below Poverty Level Estimate Unrelated Individuals For Whom Poverty Status Is Determined',
                   'Percent Below Poverty Level Estimate Worked Fulltime, Yearround In The Past 12 Months',
                   'Percent Below Poverty Level Estimate Worked Less Than Fulltime, Yearround In The Past 12 Months',
                   'Percent Below Poverty Level Estimate Did Not Work',
                   'Percent With A Disability Estimate Total Civilian Noninstitutionalized Population',
                   #'Percent With A Disability Estimate White Alone, Not Hispanic Or Latino',
                   'Percent Households Receiving Food Stamps/SNAP Estimate Households',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Households',
                   'Percent Estimate Household Type Marriedcouple Family',
                   'Percent Households Receiving Food Stamps/SNAP Estimate Household Type Married couple Family',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Household Type Marriedcouple Family',
                   'Percent Estimate Household Type Other Family:',
                   #'Percent Households Receiving Food Stamps/SNAP Estimate Household Type Other Family:',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Household Type Other Family:',
                   'Percent Estimate Household Type Nonfamily Households',
                   'Percent Households Receiving Food Stamps/SNAP Estimate Household Type Nonfamily Households',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Household Type Nonfamily Households',
                   'Percent Estimate Poverty Status In The Past 12 Months Below Poverty Level',
                   'Percent Households Receiving Food Stamps/SNAP Estimate Poverty Status In The Past 12 Months Below Poverty Level',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Poverty Status In The Past 12 Months Below Poverty Level',
                   'Percent Estimate Poverty Status In The Past 12 Months At Or Above Poverty Level',
                   'Percent Households Receiving Food Stamps/SNAP Estimate Poverty Status In The Past 12 Months At Or Above Poverty Level',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Poverty Status In The Past 12 Months At Or Above Poverty Level',
                   'Percent Estimate Disability Status With One Or More People With A Disability',
                   'Percent Households Receiving Food Stamps/SNAP Estimate Disability Status With One Or More People With A Disability',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Disability Status With One Or More People With A Disability',
                   'Percent Estimate Disability Status With No Persons With A Disability',
                   'Percent Households Receiving Food Stamps/SNAP Estimate Disability Status With No Persons With A Disability',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Disability Status With No Persons With A Disability',
                   #'Percent Estimate White Alone, Not Hispanic Or Latino',
                   #'Percent Households Receiving Food Stamps/SNAP Estimate White Alone, Not Hispanic Or Latino',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate White Alone, Not Hispanic Or Latino',
                   #'Percent Households Receiving Food Stamps/SNAP Estimate Work Status Families No Workers In Past 12 Months',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Work Status Families No Workers In Past 12 Months',
                   #'Percent Households Receiving Food Stamps/SNAP Estimate Work Status Families 1 Worker In Past 12 Months',
                   #'Percent Households Not Receiving Food Stamps/SNAP Estimate Work Status Families 1 Worker In Past 12 Months',
                   #'Percent Estimate Work Status Families 2 Or More Workers In Past 12 Months',
                   'Percent Households Receiving Food Stamps/SNAP Estimate Work Status Families 2 Or More Workers In Past 12 Months',
                   'Percent Households Not Receiving Food Stamps/SNAP Estimate Work Status Families 2 Or More Workers In Past 12 Months',
                   #'Labor Force Participation Rate Estimate White Alone, Not Hispanic Or Latino',
                   #'Unemployment Rate Estimate White Alone, Not Hispanic Or Latino',
                   'Labor Force Participation Rate Estimate Poverty Status In The Past 12 Months Below Poverty Level',
                   'Unemployment Rate Estimate Poverty Status In The Past 12 Months Below Poverty Level',
                   'Labor Force Participation Rate Estimate Poverty Status In The Past 12 Months At Or Above The Poverty Level',
                   'Unemployment Rate Estimate Poverty Status In The Past 12 Months At Or Above The Poverty Level',
                   'Labor Force Participation Rate Estimate Disability Status With Any Disability',
                   'Unemployment Rate Estimate Disability Status With Any Disability'),
      label = "Select Features for Clustering:"
      
    )
  ui.input$queryAPI <- actionButton(
    inputId = ns("queryAPI"),
    label = "Cluster!",
    #icon = icon("hurricane",lib = "glyphicon")
    icon = icon("puzzle-piece"),
    style="height:150px;width:150px;color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:200%"
  )
    
  return(ui.input)
  
  
  
}
