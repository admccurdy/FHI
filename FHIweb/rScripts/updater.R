library(rnoaa)

updateSnoTel <- function(currentSnoTel){
  NCEI_token <- "zYkGbjKEoZXmvSVEmCgDAaOZpsIjViSy"
  aprilStations <- currentSnoTel$april %>% group_by(id) %>% summarise(maxYear = max(year))
  maxStations <- currentSnoTel$max %>% dplyr::group_by(id) %>% summarise(maxYear = max(year))
  currentDate <- Sys.Date()
  aprilYear <- maxYear_check <- year(currentDate)
  if(currentDate < as.Date(paste0(aprilYear, "-04-03"))){
    aprilYear <- aprilYear - 1
  }
  if(currentDate < as.Date(paste0(maxYear_check, "-06-03"))){
    maxYear_check <- maxYear_check - 1
  }
  
  maxStations[20:22, maxYear := 2015]
  aprilStations[1:3, maxYear := 2016]
  
  aprilStations <- aprilStations %>% mutate(aprilUpdate = maxYear < aprilYear & !maxYear < aprilYear - 3) %>% filter(aprilUpdate)
  maxStations <- maxStations %>% mutate(maxUpdate = maxYear < maxYear_check & !maxYear < maxYear_check - 3) %>% filter(maxUpdate)
  updateStations <- aprilStations %>% full_join(maxStations)
  updateStations <- updateStations %>% as_tibble() %>% mutate_all(funs(replace(., is.na(.), F))) %>% group_by(id) %>%
    summarise(maxYear = min(maxYear), aprilUpdate = any(aprilUpdate), maxUpdate = any(maxUpdate)) %>% 
    mutate(dateStart = as.Date(paste0(maxYear, "-10-01")))
  
  results <- lapply(1:nrow(updateStations), FUN = function(x){
    ncdc(datasetid = "GHCND", stationid = updateStations$id[x], 
         startdate = updateStations$dateStart[x], enddate = currentDate,
         token = NCEI_token)
  })
}
