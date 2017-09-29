



fipsDisplayTitlePresentation <- function(id){
  ns <- NS(id)
  ui.objects <- mapRiskUI(id)
  ui.objects$countySummary
  
}


# displayCountyInfoTablePresentation <- function(id){
#   ns <- NS(id)
#   ui.objects <- mapRiskUI(id)
#   ui.objects$countyMetrics
#   
# }

#' Server function for displayCountyInfoServer.
#' 
#' Used to define the server within the \code{display_county_info} shiny module
#' 
#' @family display_county_info modue functions
#' 
#' @param input  standard \code{shiny} input
#' @param output standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param fipsSelection  pass the FIPS of the currently selected county 
#'        output by \code{mapPopulationServer}, \code{mapBivariateServer}, or \code{mapRiskServer}
#' @param risk.type  value indicating what kind of risk should be displayed in the table 
#'
#' 
#' #@set output@page  the output@page is set and will be the page that renders
#' 
#' @examples 
#' callModule(
#' displayCountyInfoServer,
#' "countydisplay",
#' reactive(bivariateReturn())
#' risk.type
#' )
#' @export
#' 


fipsDisplayServer <- function(input, output, session, fipsSelection, which.tab=NULL){
  ns <- session$ns
  # req(fipsSelection)
  # observe({
  #   print(which.tab())
  #   print(countySelection())
  #   
  # })
  
  # riskType <- reactive({
  #   req(which.tab())
  #   risk.type <- switch(
  #     which.tab(),
  #     "popMapTab" = NA,
  #     "riskMapTab" = NA,
  #     "mentalBivarMapTab" = "mental",
  #     "overallBivarMapTab" = "overall",
  #     "physicalBivarMapTab" = "physical"
  #   )
  #   print(risk.type)
  #   risk.type
  # })
  
  # scoreCol <- reactive({
  #   score.col <- switch(
  #     which.tab(),
  #     "popMapTab" = NA,
  #     "riskMapTab" = NA,
  #     "mentalBivarMapTab" = "mhscore",
  #     "overallBivarMapTab" = "ohscore",
  #     "physicalBivarMapTab" = "phscore"
  #   )
    
  #   score.col
  # })
  # score.col <- "mhscore"
  # risk.type <- "mental"
  
  
  # Reactive to capture the lookup data for the currently selected county
  fipsInfo <- reactive({

    DATASETS$fips.lookup[as.character(fipsSelection())] 
  })
  

  
  # Reactive to capture the title data for the currently selected county
  output $ countyTitle <- renderText({
    paste0(titleInfo()$county,", ",titleInfo()$ST)
  })
  
  output$fipsTable <- renderDataTable({
  
    fipsInfo()


  })
  
  
  
  # # Reactive to retrieve the state averages for the selected county
  # stateAvg <- reactive({
  #   DATASETS$state.avgs %>%
  #     filter(as.numeric(statecode)==countyInfo()$statecode) %>%
  #     gather(key="measure",value="state.avg")
  #   
  # })
  # 
  # # Create a table of national and state averages for the selected county
  # avgTable <- reactive({
  #   merge(DATASETS$national.avgs,stateAvg(),by="measure")
  # })
  
  
  baseCountyMetrics <- reactive({
    req(countySelection())
    
    # req(input$riskMap_shape_click)
    tmp <- DATASETS$county.risk %>%
      filter(fipscode == countySelection()) %>%
      select(contains('measure'),contains('f0954513'),contains('opioid'))
    
    #Two measures in use do not conform to the same naming pattern (since they are from different data sources)
    # For now we will deal with it separately by name, later we should deal with it programmatically
    weird.two <- data.frame(measure=c("f0954513","opioid"),
                            cihigh=c(NA,NA),
                            cilow=c(NA,NA),
                            value = c(tmp$f0954513[1],tmp$opioid[1]),
                            mhscore = c(tmp$f0954513_mhscore[1],tmp$opioid_mhscore[1]),
                            ohscore = c(tmp$f0954513_ohscore[1],tmp$opioid_ohscore[1]),
                            phscore = c(tmp$f0954513_phscore[1],tmp$opioid_phscore[1]))
    
    tmp <- tmp %>% select(-contains('f0954513'),-contains('opioid')) %>%
      gather(key="key",value="value") %>%
      extract(key,c("measure","valtype"),"(measure_[[:alnum:]][[:alnum:]]?[[:alnum:]]?_)([[:alnum:]]+)") %>%
      spread(key=valtype,value=value) %>%
      mutate(measure=paste0(measure,"value")) %>%
      select(measure,value,cilow,cihigh,mhscore,ohscore,phscore) %>%
      filter(! measure %in% c("measure_2_value","measure_36_value","measure_42_value")) %>%
      bind_rows(.,weird.two) %>%
      
      merge(.,avgTable(),by="measure") %>%
      mutate(
        state.avg = as.numeric(as.character(state.avg)),
        LowIsBad = DATASETS$measure.lookup[measure]$LowIsBad,
        #For Sig Columns: 2 = Not Sig, -1 = sig bad, 1 = sig good, 3 = NA
        #HighIsBad and LowIsGood are used to convert above/below to good/bad
        Sig.State = ifelse(is.na(cihigh),3,
                           ifelse(state.avg < cilow,1 * LowIsBad,
                                  ifelse(state.avg > cihigh, -1 * LowIsBad,2))),
        Sig.National = ifelse(is.na(cihigh),3,
                              ifelse(national.avg < cilow,1* LowIsBad,
                                     ifelse(national.avg > cihigh, -1 * LowIsBad,2))),
        measure.title = DATASETS$measure.lookup[measure]$title,
        units = DATASETS$measure.lookup[measure]$Units
      )
    
    tmp
  })
  
  compRiskCountyMetrics <- reactive({
    score.col <- scoreCol()
    
    tmp <- baseCountyMetrics() %>%
      mutate(        
        score = .[[score.col]],
        Direction=
          ifelse(is.na(score),"",
                 ifelse(
                   score<0,
                   as.character(icon("arrow-up",'text-info')),
                   as.character(icon("arrow-down",'text-info'))))
      ) %>%
      select(-cihigh,-cilow,-LowIsBad) %>%
      select(measure.title,units,value,state.avg,Sig.State,
             national.avg,Sig.National,Direction,score) %>% 
      arrange(Sig.National,score) %>%
      mutate(score = abs(score))
    
    tmp
  })
  
  nonRiskCountyMetrics <- reactive({
    tmp <- baseCountyMetrics() %>%
      select(-cihigh,-cilow,-LowIsBad) %>%
      select(measure.title,units,value,state.avg,Sig.State,
             national.avg,Sig.National) %>%
      arrange(Sig.National)
    
    tmp
  })
  
  
  output$countyTable <- renderDataTable({
    # tmp <- countyMetrics()
    #move this below later
    # tmp$state.avg[grepl(pattern = "%",tmp$uni)] <- tmp$state.avg[grepl(pattern = "%",tmp$uni)] * 100
    # tmp$national.avg[grepl(pattern = "%",tmp$uni)] <- round(tmp$national.avg[grepl(pattern = "%",tmp$uni)] * 100,digits = 1)
    risk.type <- riskType()
    
    
    if (risk.type %in% c('mental','overall','physical')){
      metrics.df <- compRiskCountyMetrics()
      title.string <- paste("Intervention Impact on<br>Composite",tools::toTitleCase(risk.type),"Health Risk")
      
      colnames(metrics.df)[colnames(metrics.df) == 'score'] <- title.string
      
      
      
      
      # currency.cols <- c("County Value", "State Average", "National Average", title.string)
    } else {
      
      metrics.df <- nonRiskCountyMetrics()
      title.string <- "placeholder"
      
      
    }
    
    
    hidden.cols <- which(colnames(metrics.df) %in% c("Sig.State", "Sig.National"))
    left.cols <- which(colnames(metrics.df) %in% c('measure.title','value','units',title.string,'state.avg','national.avg'))
    right.cols <- which(colnames(metrics.df) %in% c('Direction'))
    currency.cols <- which(colnames(metrics.df) %in% c("value", "state.avg", "national.avg", title.string))
    
    
    col.def.list <- list(
      list(visible=FALSE,
           targets=c(hidden.cols)),
      list(className = "dt-left", targets = left.cols),
      list(className = "dt-right", targets = right.cols))
    
    
    
    metrics.df <- metrics.df %>%
      rename(`Measure Variable` = measure.title,
             Units = units,
             `County Value` = value,
             `State Average` = state.avg,
             `National Average` = national.avg
      )
    
    
    
    metrics.df$`County Value`[grepl(pattern = "%",metrics.df$Units)] <- round(metrics.df$`County Value`[grepl(pattern = "%",metrics.df$Units)] * 100,digits = 1)
    metrics.df$`State Average`[grepl(pattern = "%",metrics.df$Units)] <- round(metrics.df$`State Average`[grepl(pattern = "%",metrics.df$Units)] * 100,digits = 1)
    metrics.df$`National Average`[grepl(pattern = "%",metrics.df$Units)] <- round(metrics.df$`National Average`[grepl(pattern = "%",metrics.df$Units)] * 100,digits = 1)
    
    
    metrics.dt <- metrics.df %>%
      DT::datatable(
        escape = FALSE,
        options=list(
          pageLength = 25,
          dom = 'ft',
          columnDefs = col.def.list))
    
    
    metrics.dt %>%
      formatStyle(
        columns=which(colnames(metrics.df) == "State Average"),
        valueColumns = which(colnames(metrics.df) == "Sig.State"),
        border = styleEqual(c(-1,1),c('2px solid #FFFFFF', '2px solid #FFFFFF')),
        backgroundColor = styleEqual(c(2,-1,1,3),values=c("transparent","#FFB6C1","#90EE90","transparent"))) %>%
      formatStyle(
        columns=which(colnames(metrics.df) == "National Average"),
        valueColumns = which(colnames(metrics.df) == "Sig.National"),
        border = styleEqual(c(-1,1),c('2px solid #FFFFFF','2px solid #FFFFFF')),
        backgroundColor = styleEqual(c(2,-1,1,3),values=c("transparent","#FFB6C1","#90EE90","transparent"))) %>%
      #formatRound(columns=c("Value", "State Average", "National Average"), digits=5)
      formatCurrency(currency.cols, currency="")
    
    
  })
  
  
  output$tableDescription <- renderUI({
    req(which.tab)
    
    text.bin <- switch(
      which.tab(),
      "popMapTab" = c(FALSE,""),
      "riskMapTab" = c(FALSE, ""),
      "mentalBivarMapTab" = c(TRUE,"mental"),
      "overallBivarMapTab" = c(TRUE,"overall"),
      "physicalBivarMapTab" = c(TRUE,"physical")
    )
    
    p.list <- list()
    
    if (text.bin[1]){
      if(text.bin[2] == "overall"){
        out <-
          tags$p("The percentage of adults reporting poor or fair health
                 (scale: [poor, fair, good, very good, excellent]) in a county
                 is predicted from 19 factors impacting a county's health. ",
                 tags$b("Intervention impact"),
                 " refers to the predicted change this percentage of adults which would result from substituting the national average for
                 an individual county's value for that metric. ",
                 tags$b("Direction"),
                 " indicates whether this substitution would increase or decrease the number adults reporting poor/fair health."
          )
      }else{
        out <-
          
          p(paste0("The number of poor ", text.bin[2],
                   " health days per year in a county is predicted from 19 factors
                   impacting a county's health. "),
            tags$b("Intervention impact"),
            " refers to the predicted change in the number of poor ",
            text.bin[2], " health days per year which would result from substituting the national average for
            an individual county's value for that metric. ",
            tags$b("Direction"),
            " indicates whether this substitution would increase or decrease the number of days."
          )
        
      }
      # append(p.list, " in a county is predicted from 19 factors impacting a county's health. ")
    }
    else{
      
      
      out <- tags$p("")
    }
    
    out
  })
  
  
  
}