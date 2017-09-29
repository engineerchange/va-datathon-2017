#Data Prep

datasets <- function() {
  print("dataprep")
  csb.ogr <- readRDS("../data/CSB/CSBmap.rds")
  va.ogr <- readRDS("../data/counties/va_ogr_500k.rds")
  csb_crosswalk <- read.csv("../data/CSB/CSB_FIPS_GIS.csv",stringsAsFactors = FALSE)
  csb.finances <- read.csv("../data/CSB/csb_financial_data.csv",stringsAsFactors = FALSE) %>%
    filter(FY == 2014) %>%
    select(-CSB) %>%
  mutate_at(vars(State.Funds,Local.Funds,Federal.Funds,Total.Net.Assets),funs(as.numeric(gsub(",","",.))))
  csb_crosswalk <- merge(csb_crosswalk,csb.finances,by="OBJECTID",all.x=T)
  
  csb.ogr <- merge(csb.ogr,csb.finances,by="OBJECTID",all.x=T)
  
  opioid.fatalities <- read.csv("../data/healthcare-deathdata/va_opioid_fatalities.csv",header=T,stringsAsFactors = FALSE) %>%
    select(-X,-DeathByYear) %>%
    filter(!is.na(Rate)) %>%
    filter( !GEOID %in% c(51019,51620,51760,51770)) %>%
    filter(Year > 2011) %>%
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
    print(thing)
    tmp <- merge(va.ogr,x,by="GEOID")
    tmp <- merge(tmp,csb_crosswalk,by.x = "GEOID",by.y="ST.FIPS")

    return(tmp)
  })
  

  
  bonnie2015 <- read.csv("../data/2015master.csv",stringsAsFactors = FALSE) %>%
    mutate(GEOCODE = as.character(FIPS.Code)) %>% 
    merge(.,csb_crosswalk,by.x="GEOCODE",by.y="ST.FIPS",all.x=TRUE)
  opioidRx2016 <- read.csv("../data/healthcare-prescribers/2016prescriptions.csv")
  
  
  csb.lookup <- bonnie2015 %>% 
    # mutate(GEOCODE = as.character(FIPS.Code)) %>%
    select(GEOCODE,
           County,
           City.County,
           OBJECTID,
           CSBName,
           Type,
           Rate_x,
           Rate_y) %>%
    rename(
      `Incidence Rate` = Rate_x,
      `Prescription Opioid Rate` = Rate_y) %>%
    spread(Type,`Incidence Rate`) %>%
    data.table::data.table(.,key="OBJECTID")
  
  fips.lookup <- bonnie2015 %>% 
    # mutate(GEOCODE = as.character(FIPS.Code)) %>%
    select(GEOCODE,
           County,
           City.County,
           CSBName,
           Type,
           Rate_x,
           Rate_y) %>%
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
    "fips.lookup" = fips.lookup,
    "csb.lookup" = csb.lookup
  )
  
  
  return(output)
  
}

# assign datasets to global value for access throughout the application. 
DATASETS <- datasets()
gc()

