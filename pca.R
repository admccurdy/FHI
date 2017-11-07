library("Hmisc")
library("data.table")
fhiData <- fread("data/fhiAll.csv")
fhiData <- fhiData[!is.na(Year),]
fhiLT <- fhiData[, !names(fhiData) %in%
                   c("Year", "CRN_vpd", "NPP", "insDis", "wild_OVN", "precip_PH", "haze"), with = F]
corMat <- cor(fhiLT, use = "complete.obs")
corList <- rcorr(as.matrix(fhiLT))
corBool <- apply(corList[[3]], c(1,2), function(x){ifelse(x <= .05, T, F)})
pca <- prcomp(fhiLT[complete.cases(fhiLT),], scale. = T, center = T)
ev <- pca$sdev^2
ev <- ev/sum(ev)


evProp <- sapply(1:length(ev), function(x){
  return(sum(ev[1:x]))
})
qplot(x = 1:length(ev), y = evProp, geom = "point")

print(pca)
