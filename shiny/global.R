require(shiny)
require(shinydashboard)
require(dplyr)
require(tidyr)
require(DT)
require(ggplot2)
require(viridis)
require(leaflet)
require(shinycssloaders)
require(jsonlite)
require(RColorBrewer)



# Source modules and scripts

source("./R/data_prep.R")
source("./R/csb_map.R")
source("./R/fips_map.R")
source("./R/strategy_map.R")
source("./R/fips_display.R")


## Namespaces
CSB.MAP <- "csb.map"
FIPS.MAP <- "fips.map"
STRATEGY.MAP <- "strategy.map"
FIPS.DISPLAY <- "fips.display"


## Functions


makePalette <- function(domain,...){
  #Function to construct a risk palette with specific shared style choices
  risk.label <- round(unname(quantile(domain,na.rm = T,probs=c(0,.5,.75,1))),digits=1)
  
  risk.pal <- colorBin(
    palette = "Greens", 
    domain=domain,
    bins = risk.label,
    na.color = NA,
    pretty = FALSE,
    ...)
  
  return(list(pal=risk.pal,label=risk.label))
}
  
  