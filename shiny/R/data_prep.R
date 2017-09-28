#Data Prep

datasets <- function() {
  print("dataprep")
  csb.ogr <- readRDS("../data/CSB/CSBmap.rds")
  va.ogr <- readRDS("../data/counties/va_ogr_500k.rds")
  
  opioid.fatalities <- read.csv("../data/healthcare-deathdata/va_opioid_fatalities.csv",header=T) %>%
    filter(!is.na(Rate)) %>%
    filter( !GEOID %in% c(51620,51760,51770)) %>%
    filter(Year == 2016,Drug =="All Drugs")
  maps.df <- merge(va.ogr,opioid.fatalities,by="GEOID")
  # county.lookup <- va.counties@data %>% 
  #   select(GEOID,STATEFP, NAME,ST,county,Reserve.Pop,Reserve.Above.Avg,Active.Pop,Active.Above.Avg) %>%
  #   data.table::data.table(.,key="GEOID")

  
  
  opioidRx2016 <- read.csv("../data/healthcare-prescribers/2016prescriptions.csv")
  
  
  
  # Return outputs in a list
  output <- list(
    "csb.ogr" = csb.ogr,
    "va.ogr" = va.ogr,
    "opioidRx2016" = opioidRx2016,
    "maps.df" = maps.df
  )
  
  
  return(output)
  
}

# assign datasets to global value for access throughout the application. 
DATASETS <- datasets()
gc()

