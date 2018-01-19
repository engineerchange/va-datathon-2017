print("loading csb_map")
# 
# makePalette <- function(domain,...){
#   #Function to construct a risk palette with specific shared style choices
#   risk.label <- round(unname(quantile(domain,na.rm = T,probs=c(0,.25,.5,.75,1))),digits=1)
#   
#   risk.pal <- colorBin(
#     palette = "Greens", 
#     domain=domain,
#     bins = risk.label,
#     na.color = NA,
#     pretty = FALSE,
#     ...)
#   
#   
#   
#   return(list(pal=risk.pal,label=risk.label))
# }
# 

csbMapServer <- function(input, output, session){
  ns <- session$ns
  
  statefund.pal <- makePalette(as.numeric(DATASETS$csb.ogr$State.Funds))
  localfund.pal <- makePalette(as.numeric(DATASETS$csb.ogr$Local.Funds))
  federalfund.pal <- makePalette(as.numeric(DATASETS$csb.ogr$Federal.Funds))
  netassets.pal <- makePalette(as.numeric(DATASETS$csb.ogr$Total.Net.Assets))
  
  
  
  
  
  csbSelection <- eventReactive(input$csbMap_shape_click,{
    req(input$csbMap_shape_click)
    click <- input$csbMap_shape_click
    
    if (is.null(click))
      return()
    
    click.id <- click$id
    #take out the 3 letters added to the start of GEOID to create the layerID
    tmp <- gsub("[a-z][a-z][a-z]","",click.id)
    
    tmp
  })
  
  
  output$csbMap <- renderLeaflet({
    print("inside cbsMap")
    
    csb.pop <- paste0("<strong>CSB: </strong>",
                       DATASETS$csb.ogr$CSBName,
                      "<br><strong>PPR: </strong>",
                      DATASETS$csb.ogr$PPR,
                      "<br><strong>HPR: </strong>",
                      DATASETS$csb.ogr$HPR)
    
    tmp <- DATASETS$csb.ogr %>%
      leaflet() %>%
      # addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(
        color = "#b2aeae",
        weight = 1,
        layerId=~paste0("clf",OBJECTID),
        fillColor = ~statefund.pal$pal(`State.Funds`),
        fillOpacity = 0.8,
        popup = csb.pop,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        label = ~CSBName,
      highlightOptions = highlightOptions(
        color = "white",
        weight = 3,
        # bringToFront = TRUE,
        sendToBack = TRUE) ) %>%
      addLegend(pal = statefund.pal$pal,
                values = ~State.Funds,
                position = "bottomright",
                title = "State Funds",
                layerId = "csbLegend",
                labels = statefund.pal$label
      ) %>%
      addPolygons(
        color = "#b2aeae",
        weight = 1,
        layerId=~paste0("clf",OBJECTID),
        fillOpacity = 0.8,
        fillColor = ~localfund.pal$pal(`Local.Funds`),
        popup = ~csb.pop,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        label = ~CSBName,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 3,
          # bringToFront = TRUE,
          sendToBack = TRUE) ) %>%

      addPolygons(
        color = "#b2aeae",
        weight = 1,
        layerId=~paste0("cna",OBJECTID),
        fillColor = ~netassets.pal$pal(`Total.Net.Assets`),
        popup = ~csb.pop,
        fillOpacity = 0.8,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        label = ~CSBName,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 3,
          # bringToFront = TRUE,
          sendToBack = TRUE) ) %>%
      addLayersControl(
        baseGroups = c("State Funds", "Local Funds","Total Net Assets"),
        options = layersControlOptions(collapsed = FALSE)
      )
    # %>%
      # addLegend(pal = colorFactor(~CSBName,DATASETS$csb.ogr@data$CSBName),value = ~CSBName)
    
    tmp
    
  })
  
  
  
  
  
  # Changes in the group selection trigger a change in the legend
  observeEvent(input$csbMap_groups,{
    
    if (is.null(input$csbMap_groups))
      return()
    
    
    
    csbMap <- leafletProxy("csbMap", session=session) %>% 
      removeControl("csbLegend")
    
    if (input$csbMap_groups == 'State Funds'){
      csbMap <- csbMap %>%
        addLegend(pal = statefund.pal$pal,
                  values = DATASETS$csb.ogr$State.Funds,
                  position = "bottomright",
                  title = "State Funds",
                  layerId = "csbLegend",
                  labels = statefund.pal$label
        ) 
    }
    else if (input$csbMap_groups == 'Local Funds'){
      csbMap <- csbMap %>%
        addLegend(pal = localfund.pal$pal,
                  values = DATASETS$csb.ogr$Local.Funds,
                  position = "bottomright",
                  title = "Local Funds",
                  layerId = "csbLegend",
                  labels = localfund.pal$label
        ) 
    } 
    else if (input$csbMap_groups == 'Total Net Assets'){
      csbMap <- csbMap %>%
        addLegend(pal = netassets.pal$pal,
                  values = DATASETS$csb.ogr$Total.Net.Assets,
                  position = "bottomright",
                  title = "Total Net Assets",
                  layerId = "csbLegend",
                  labels = netassets.pal$label
        ) 
    }
    
  })
  
  
  output$csbTable <- renderDataTable({
    DATASETS$csb.lookup[as.character(csbSelection())]
    
  })
  
  
  list(
    
    
    "csbSelection" = csbSelection
  )
  
  
  
}



# 
# csbMapUI <- function(id){
#   ns <- NS(id)
#   
#   
# }