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
  
  
  # opioid.fatalities <- read.csv("../data/healthcare-deathdata/va_opioid_fatalities.csv",header=T,stringsAsFactors = FALSE) %>%
  #   select(-X,-DeathByYear) %>%
  #   filter(!is.na(Rate)) %>%
  #   filter( !GEOID %in% c(51019,51620,51760,51770)) %>%
  #   # filter(Year > 2012) %>%
  #   spread(Year,Rate) %>%
  #   mutate(Drug = as.factor(Drug))
  # 
  # ## split into tables by year
  # fatalities.list <- split(opioid.fatalities,list(opioid.fatalities$Drug))
  
  maps.df <- lapply(fatalities.list,function(x){
    print("inside")
    thing <- x$GEOID[duplicated(x$GEOID)]

    return(merge(va.ogr,x,by="GEOID"))
  })
  


  
  bonnie2015 <- read.csv("../data/2015master.csv",stringsAsFactors = FALSE)
  opioidRx2016 <- read.csv("../data/healthcare-prescribers/2016prescriptions.csv")
  
  
  fips.lookup <- bonnie2015 %>% 
    mutate(GEOCODE = as.character(FIPS.Code)) %>%
    select(GEOCODE,
           County,
           Type,
           Rate_x,
           Rate_y) %>%
    group_by(GEOCODE,
             County,
             Type,
             Rate_x,
             Rate_y) %>%
    summarize(count=n()) %>% ungroup() %>%
    rename(
      `Incidence Rate` = Rate_x,
      `Prescription Opioid Rate` = Rate_y) %>% 
    spread(Type,`Incidence Rate`) %>%
    data.table::data.table(.,key="GEOCODE")
  
  
  # Return outputs in a list
  output <- list(
    "csb.ogr" = csb.ogr,
    "va.ogr" = va.ogr,
    "opioidRx2016" = opioidRx2016,
    "maps.df" = maps.df,
    "fips.lookup" = fips.lookup
  )
  
  
  return(output)
  
}

# assign datasets to global value for access throughout the application. 
DATASETS <- datasets()
gc()

