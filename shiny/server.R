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
  
  
  callModule(
    fipsMapServer,
    FIPS.MAP
  )
  
  
  
  
  
  
  
  
  
  
})