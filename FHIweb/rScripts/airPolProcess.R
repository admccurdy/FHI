library(data.table)
library(dplyr)
library(dtplyr)
library(jsonlite)
library(httr)

https://aqs.epa.gov/api/profile?user=*&pw=**&format=CSV&param=44201&bdate=2010&edate=2015&state=37&county=063
apiBase <- "https://aqs.epa.gov/api/rawData?user="
*&pw=**
  
apiMiddle <- "&format=DMCSV&param=44201&bdate=20110501&edate=20110501&state=37&"



epaAPI <- function(requestType, param, startDate, endDate, state, otherParams = NULL){
  apiBase <- "https://aqs.epa.gov/api/"
  user <- "amccurdy@aspennature.org"
  pword <- "sandwren91"
  if(!is.null(otherParams))otherParams <- paste0("&", otherParams)
  url <- paste0(apiBase, requestType, "?user=", user, "&pw=", pword, 
                "&format=CSV&param=", param, "&bdate=", startDate, "&edate=", 
                 endDate, "&state=", state)
  return <- GET(url)
}

temp <- GET("https://aqs.epa.gov/api/profile?user=amccurdy@aspennature.org&pw=sandwren91&format=CSV&param=44201&bdate=2010&edate=2015&state=37&county=063")
