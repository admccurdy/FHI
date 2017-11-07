"
Scratchpad for pulling COOP site precip totals
for a specified state ('wrc.state', set in 'vars.R'). 

Once developed futher, this code should be wrapped
into a function in 'support_functions.R'.
"

library(XML)
library(stringr)

source("R/support_functions.R")

if(!exists("wrc.state")){
  wrc.state="co"
}

##list of station links
sta=getHTMLLinks(paste0("http://www.wrcc.dri.edu/summary/",wrc.state,"lst.html"))
sta=sta[grepl(paste0("?",wrc.state),sta)]
sta=str_replace_all(sta,"cliMAIN","cliMeta")

####Pull all COOP sites in state####
coops=list()
for (st in sta){
  
  stcode=substr(st,nchar(st)-3,nchar(st))
  stanm=as.character(unlist(htmlTreeParse(paste0("http://www.wrcc.dri.edu",st))$children$html[2]$body)[6]) #station name
  cat(stanm,"\n")

  tryCatch({
    
    stmeta=readHTMLTable(paste0("http://www.wrcc.dri.edu",st),skip.rows = c(1:2),stringsAsFactors=F)[[1]]
    stmeta$lat=as.numeric(substr(stmeta$V4,1,2))+as.numeric(substr(stmeta$V4,3,4))/60
    stmeta$long=-(as.numeric(substr(stmeta$V5,1,3))+as.numeric(substr(stmeta$V5,4,5))/60)
    sta_loc=colMeans(stmeta[,c("lat","long")]) #station "location" - centroid of all the point pairs listed
    
    # fetch state id (from first entry only)
    # state codes in links don't necessarily correspond to FIPS (i.e. AZ)
    if(length(coops)==0){
      state.code=substr(stmeta$V2[1],1,2)
    }

        ##monthly precip totals
    staprecip=readHTMLTable(paste0("http://www.wrcc.dri.edu/WRCCWrappers.py?sodxtrmts+",state.code,stcode,"+por+por+pcpn+none+msum+5+01+F"),stringsAsFactors=F)[[1]] #monthly precip totals
    if(staprecip[,1][which(staprecip[,1]=="Period of Record Statistics")-1]!="2016"){ #station period of record ends prior to 2016
      
      staprecip=staprecip[-(which(staprecip[,1]=="Period of Record Statistics"):nrow(staprecip)),] #can't use 'skip.rows' because stations have different periods of record/no. of rows
      
    }else{ #station is current to 2016
      
      staprecip=staprecip[-(which(staprecip[,1]=="2016"):nrow(staprecip)),] #can't use 'skip.rows' because stations have different periods of record/no. of rows
      
    }
    
    staprecip=staprecip[,!names(staprecip) %in% c("V26",paste0("V",seq(3,27,by=2)))] #remove annotations and totals column
    staprecip[,-1]=sapply(staprecip[,-1],as.numeric) #convert to numeric 
    
    stadd=list()
    
    stadd$code=stcode
    stadd$meta=stmeta
    stadd$precip=staprecip
    stadd$loc=sta_loc
    stadd$grid=staGrid
    
    coops[[stanm]]=stadd
    
  },error=function(e){
    cat("could not pull",stanm,"\n\n")
  })
                         
}

save(coops,file="data/coops.RData")