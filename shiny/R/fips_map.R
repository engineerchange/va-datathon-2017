#FIPS Maps



fipsMapServer <- function(input, output, session){

output$fipsMap <- renderLeaflet({
  print("inside cbsMap")
  fatal.pal <- makePalette(DATASETS$maps.df$Rate)
  fillColor = ~fatal.pal$pal()
  
  tmp <- DATASETS$maps.df %>%
    leaflet() %>%
    # addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
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
