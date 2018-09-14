library(odbc)
library(DBI)

dbiConn <- dbConnect(odbc(), 
                     driver = "SQL Server",
                     server = "tcp:foresthealthindex.database.windows.net,1433",
                     database = "FHI", 
                     uid = "amccurdy@foresthealthindex",
                     pwd = "Aces2k12$")
myConn <- odbcDriverConnect(connectionString)
fhiExist <- read.csv("FHIweb/data/baseData.csv")
sqlSave(channel = myConn, dat = fhiExist, tablename = "userObs",
        safer = F)

for(i in 1982:2016){
  dbWriteTable(conn = dbiConn, name = "dayMetPrecip", value = dayMetCO[[1]][year == i,], 
               overwrite = FALSE, append = TRUE, row.names = FALSE)
}

vals = paste(watershedKey[["14010004"]], collapse = ", ")
ptm <- proc.time()
rs <- dbSendQuery(dbiConn, qry)
proc.time() - ptm
ptm <- proc.time()
test <- dbFetch(rs)
proc.time() - ptm
qry <- sprintf("SELECT * FROM dayMetPrecip WHERE grid in (%s)", vals)


"F:/Documents/ACES/FHI/FHIweb/data/dayMetAnn/csv/tmin.txt"


myData <- dayMetCO[[3]][grid %in% watershedKey[["14010004"]]]

myData <- myData %>% group_by(year) %>% summarise(avg = mean(value))
