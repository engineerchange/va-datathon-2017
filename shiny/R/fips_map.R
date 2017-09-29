#FIPS Maps



fipsMapServer <- function(input, output, session){

output$fipsMap <- renderLeaflet({
  print("inside cbsMap")
  fatal.pal <- makePalette(maps.df[["2016"]]$`All Drugs`)
  fillColor = ~fatal.pal$pal()
  print(names(DATASETS$maps.df))
  tmp <- DATASETS$maps.df[[as.character(input$yearFIPS)]] %>%
    leaflet() %>%
    # addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(
      weight = 1,
      fillOpacity = 0.6,
      fillColor = ~fatal.pal$pal(`All Drugs`)) %>%
    addLegend(pal = fatal.pal$pal,
              values = DATASETS$counties.ogr$`All Drugs`,
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




fipsMapUI <- function(id){
  ns <- NS(id)
  
  sliderInput(
    inputId = ns("yearFIPS"),
    label = "Select Year:",
    min=2012,
    max=2016,
    value = 2015,
    step=1,
    round=TRUE,
    sep = "",animate = TRUE,
    ticks = FALSE)
  
  
  
}
