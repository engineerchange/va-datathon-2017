# UI 




# Build the wire frame of the front end
# sidebar <- dashboardSidebar(disable = T)

countyMetricsBox <- box(
  title = "Stuff",
  width = 12,
  p("Weighted composite risk scores are derived from 18 health factors
    impacting a county's health risk. ",tags$b("Intervention impact"), " refers to the
    change in risk score which would result from substituting the national average for
    an individual county's value for that metric.")
  # status = "",
  # dataTableOutput(NS(COUNTY.METRICS)("countyTable"))
)

countySummaryBox <- box(status = "warning",
  width = 12,
  title = "County Metrics",
  h2("Placeholder")
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
      title = "Population Maps",
      value = "popMapTab",
      # style = 'padding: 0px;',
      p("County population estimates obtained from 2014 American Community Survey from U.S. Census."),
      leafletOutput(NS(CSB.MAP)("csbMap"))
    ),
    tabPanel(
      title = "Risk Maps",
      value = "riskMapTab",
      # style = 'padding: 0px;',
      p("This interactive map displays the predicted risk of three health-related proxy readiness outcomes.
        Select a specific county on the map to view its risk"),
      leafletOutput(NS(CSB.MAP)("fipsMap"))
    )
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
    # )
  )

# 
# 
# 
# 
# Build the guts of the front end
body <- dashboardBody(
  fluidRow(
    column(width = 3,
      countySummaryBox
      ),
    column(width = 9,
           textOutput("test")
    )
  ),
  fluidRow(
    h2("placeholder"),
    # leafletOutput(NS(CSB.MAP)("csbMap"))
    mapsTabBox
  )
)


print("here")

shinyUI(
  # Uncomment this line to reenable login
  # uiOutput(NS(LOGIN)("page")),
  
  ui = dashboardPage(
    title = "APPLICATION",
    skin="green",
    header = dashboardHeader(title = "WHAT",
                             titleWidth = 450),
    sidebar = dashboardSidebar(disable=TRUE),
    body = body
  )
)