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
  

  
  
}



# 
# csbMapUI <- function(id){
#   ns <- NS(id)
#   
#   
# }