print("loading csb_map")

makePalette <- function(domain,...){
  #Function to construct a risk palette with specific shared style choices
  risk.label <- round(unname(quantile(domain,na.rm = T,probs=c(0,.25,.5,.75,1))),digits=1)
  
  risk.pal <- colorBin(
    palette = "Greens", 
    domain=domain,
    bins = risk.label,
    na.color = NA,
    pretty = FALSE,
    ...)
  
  
  
  return(list(pal=risk.pal,label=risk.label))
}


csbMapServer <- function(input, output, session){
  ns <- session$ns
  output$csbMap <- renderLeaflet({
    print("inside cbsMap")
    
    tmp <- DATASETS$csb.ogr %>%
      leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(weight = 1)
    # %>%
      # addLegend(pal = colorFactor(~CSBName,DATASETS$csb.ogr@data$CSBName),value = ~CSBName)
    
    tmp
    
  })
  
  
  output$fipsMap <- renderLeaflet({
    print("inside cbsMap")
    fatal.pal <- makePalette(DATASETS$maps.df$Rate)
    fillColor = ~fatal.pal$pal()
    
    tmp <- DATASETS$maps.df %>%
      leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(
        weight = 1,
        fillOpacity = 0.6,
        fillColor = ~fatal.pal$pal(Rate)) %>%
      addLegend(pal = fatal.pal$pal,
                values = DATASETS$counties.ogr$Rate,
                position = "bottomright",
                title = "All Drugs",
                layerId = "deathLegend",
                labels = fatal.pal$label
      ) 
    # %>%
    # addLegend(pal = colorFactor(~CSBName,DATASETS$csb.ogr@data$CSBName),value = ~CSBName)
    
    tmp
    
  })
  
  
  
}



# 
# csbMapUI <- function(id){
#   ns <- NS(id)
#   
#   
# }