library(httr)
library(jsonlite)
base <- "https://api.neoncrm.com/neonws/services/api/"
loginEnd <- "common/login?login.apiKey="
loginMid <- "&login.orgid="
orgID <- "aspennature"
apiKey <- "7992fe4861980528a1ef8c02e3820820"
loginCall <- paste0(base, loginEnd, apiKey, loginMid, orgID)
loginJSON <- GET(loginCall)
loginText <- content(loginJSON, "text")
loginText
sessionID <- "816e015b7ce0e2f920596196c11766d2"

accountInfo <- paste0("https://api.neoncrm.com/neonws/services/api/account/retrieveIndividualAccount?userSessionId=", 
  sessionID, "&accountId=21476")

accountInfoRet <- GET(accountInfo)
accountText <- content(accountInfoRet, "text")

customObjs <- GET(paste0("https://api.neoncrm.com/neonws/services/api/customObject/listCustomObjects?userSessionId=",
                         sessionID))
 
objFileds <- GET(paste0("https://api.neoncrm.com/neonws/services/api/customObject/listCustomObjectFields?userSessionId=",
  sessionID, "&objectApiName=2nd_Object_c"))

do.call(rbind.data.frame, lapply(objFileds, rjson::fromJSON))
fromJSON(content(objFileds, "text"))
z <- jsonlite::fromJSON(content(objFileds, "text"), simplifyVector = FALSE)
