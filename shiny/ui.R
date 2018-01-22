# UI 




# Build the wire frame of the front end
# sidebar <- dashboardSidebar(disable = T)

countyMetricsBox <- box(
  title = "Metrics",
  width = 12,
  p("Weighted composite risk scores are derived from 18 health factors
    impacting a county's health risk. ",tags$b("Intervention impact"), " refers to the
    change in risk score which would result from substituting the national average for
    an individual county's value for that metric.")
  # status = "",
  # dataTableOutput(NS(COUNTY.METRICS)("countyTable"))
)





modelInputsBox <- box(status = "warning",
  width = 12,
  title = "Model Adjustments",
  # h2("Placeholder"),
  # fipsMapUI(FIPS.MAP)$yearFIPS,
  fipsMapUI(FIPS.MAP)$selectFeatures
  # style = 'padding: 0px;',

  # h2(textOutput(NS(COUNTY.METRICS)("countyTitle")))

)

# All data components were derived from public data sets.
# 
mapsTabBox <-
  tabBox(
    width = 12,
    id = "mapsTabSet",
    # style = 'padding: 0px;',

    tabPanel(
      title = "FIPS",
      value = "fipsMapTab",
      # style = 'padding: 0px;',
      fluidRow(
        column(width = 2,
               h3("Opioid Fatality Incidence Rates"),
               box(width=12,fipsMapUI(FIPS.MAP)$yearFIPS)),
        column(width =10,
      leafletOutput(NS(FIPS.MAP)("fipsMap"))
        )
    )),
    tabPanel(
      title = "CSB",
      value = "popMapTab",
      # style = 'padding: 0px;',
      fluidRow(
        
        leafletOutput(NS(CSB.MAP)("csbMap"))
        
        
      )),
    tabPanel(
      title = "Strategy",
      value = "strategyMapTab",
      # style = 'padding: 0px;',
      fluidRow(
        
        column(width=3,
      h3("Identify counties whose needs are closest.")),
        column(width = 9,box(width=12,strategyMapPresentation(STRATEGY.MAP)))),
        # column(width =10,
      leafletOutput(NS(STRATEGY.MAP)("strategyMap")
    
    ))
    # ,
    # tabPanel(
    #   title = "Mental Health Risk v. Population",
    #   value= "bivarMapTab",
    #   p("Display of overlapping mental health and population metrics."),
    #   # style = 'padding: 0px;',
    #   leafletOutput(NS(MAP.BIVARIATE)("bivMap")),
    #   absolutePanel(
    #     id = "legendpanel",
    #     class = "panel panel-default", #fixed = TRUE,
    #     draggable = TRUE,
    #     top = "auto", left = "auto", right = 25, bottom = 23,
    #     width = "200", height = "200",
    #     style = 'padding: 0px;',
    #     plotOutput(NS(MAP.BIVARIATE)("bivariateLegend"))
    #   )
    # )
    # ,
    # tabPanel(
    #   title="Test Space",
    #   value = "testSpaceTab",
    #   verbatimTextOutput("clickText")
    )
  

# 
# 
# 
# 
# Build the guts of the front end
body <- dashboardBody(
  fluidRow(

    column(width = 12,
           mapsTabBox
    )
  ),
  fluidRow(
    # textOutput(NS(FIPS.MAP)("clickText")),
    # h2("placeholder"),
    # leafletOutput(NS(CSB.MAP)("csbMap"))
    dataTableOutput(NS(FIPS.DISPLAY)("fipsTable"))
  )
)


# print("here")

shinyUI(
  # Uncomment this line to reenable login
  # uiOutput(NS(LOGIN)("page")),
  
  ui = dashboardPage(
    title = "APPLICATION",
    skin="yellow",
    header = dashboardHeader(title = "VA Governor's Datathon",
                             titleWidth = 450),
    sidebar = dashboardSidebar(disable=TRUE),
    body = body
  )
)