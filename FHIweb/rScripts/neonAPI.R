library(httr)
library(jsonlite)
base <- "https://api.neoncrm.com/neonws/services/api/"
loginEnd <- "common/login?login.apiKey="
loginMid <- "&login.orgid="
orgID <- "aspenpopulated"
apiKey <- "75cc362f090f474611c6f1ef1ab96bd7"
loginCall <- paste0(base, loginEnd, apiKey, loginMid, orgID)
loginCall
loginText <- GET(loginCall)
loginJSON <- GET(loginCall)
loginText <- content(loginJSON, "text")
loginText
sessionID <- "ada750b84f306a191007b087ece65c3a"
productTest <- paste0(
  
customObjs <- GET(paste0("https://api.neoncrm.com/neonws/services/api/customObject/listCustomObjects?userSessionId=",
                         sessionID))
  
objFileds <- GET(paste0("https://api.neoncrm.com/neonws/services/api/customObject/listCustomObjectFields?userSessionId=",
  sessionID, "&objectApiName=2nd_Object_c"))

do.call(rbind.data.frame, lapply(objFileds, rjson::fromJSON))
fromJSON(content(objFileds, "text"))
z <- jsonlite::fromJSON(content(objFileds, "text"), simplifyVector = FALSE)
