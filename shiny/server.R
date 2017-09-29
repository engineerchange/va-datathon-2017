# server.R


shinyServer(function(input,output,session){
  
  output$test <- renderPrint({
    tmp <- fromJSON('http://localhost:5000?hello=jessica')
    tmp
  })
  
  
  csb.info <- callModule(
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
  
  strategy.info <- callModule(
    strategyMapServer,
    STRATEGY.MAP
  )
  
  # strategyInfo <- reactive({
  #   
  #   strategy.info$strategySelection()
  # })
  
  
  
  
  
})