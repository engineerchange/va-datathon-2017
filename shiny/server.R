# server.R


shinyServer(function(input,output,session){
  
  output$test <- renderPrint({
    tmp <- fromJSON('http://localhost:5000?hello=jessica')
    tmp
  })
  
  
  callModule(
    csbMapServer,
    CSB.MAP
  )
  
  
  fips.info <- callModule(
    fipsMapServer,
    FIPS.MAP
  )
  
  fipsInfo <- reactive({
    
    fips.info$fipsSelection()
  })
  
  
  callModule(
    fipsDisplayServer,
    FIPS.DISPLAY,
    fipsSelection=reactive(fipsInfo())
  )
  
  
  
  
  
})