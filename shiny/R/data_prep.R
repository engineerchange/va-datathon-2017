#Data Prep

datasets <- function() {
  print("dataprep")
  csb.ogr <- readRDS("../data/CSB/CSBmap.rds")
  va.ogr <- readRDS("../data/counties/va_ogr_500k.rds")
  
  opioid.fatalities <- read.csv("../data/healthcare-deathdata/va_opioid_fatalities.csv",header=T,stringsAsFactors = FALSE) %>%
    select(-X,-DeathByYear) %>%
    filter(!is.na(Rate)) %>%
    filter( !GEOID %in% c(51019,51620,51760,51770)) %>%
    # filter(Year > 2012) %>%
    spread(Drug,Rate) %>%
    mutate(Year = as.factor(Year))
  
  ## split into tables by year
  fatalities.list <- split(opioid.fatalities,list(opioid.fatalities$Year))
  
  maps.df <- lapply(fatalities.list,function(x){
    print("inside")
    thing <- x$GEOID[duplicated(x$GEOID)]

    return(merge(va.ogr,x,by="GEOID"))
  })
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

