scoreClean <- function(scores, myNames){
  if(length(myNames) != length(scores)){
    myNames <- 1:length(scores)
  }
  returnFrame <- data.frame("Station" = myNames,
                            "Score" = unlist(scores))
  return(stargazer::stargazer(returnFrame, summary = F, type = "html", rownames = F,
                   digits = 0))
}

scorer <- function(myData, myYears, method, metric){
  if(length(myData) == 0){
    myReturn <- "No data for selected time period"
  }else{
    value <- myData %>% lapply(function(x)x[year %in% myYears, value] %>% mean())
    if(method == "quant"){
      myReturn <- quantFHI(myData, "tempmax", value)  
    }else if(method == "FHI"){
      
    }else if(method == "trend"){
      myReturn <- myData %>% lapply(function(x)trendScore(year = x$year, value = x$value) %>% round(digits = 2))  
    }else{
      myReturn <- "Invalid method selected"
    }
    
  }
  return(myReturn)
}
# myData <- copy(tempSave)
# validYears <- lapply(myData, "[",, year) %>% 
#   lapply(function(x)all(c(2015:2015) %in% x)) %>% unlist()

quantFHI <- function(myData, metric, value){
    if(metric == "npp"){
      param <- myData %>% lapply(FUN = function(x)list(fhat = kde(x$value)))
      myDist <- "kde"
    }else{
      if(grepl("temp", metric)){
        myData <- lapply(myData, function(x) x[, value:= value / 10 + 273.15])
        value <- lapply(value, function(x) x / 10 + 273.15)
      }
      param <- 
        tryCatch({
          myData %>% lapply(FUN = function(x)fitdistr(x$value, "gamma")[[1]])
        }, error = function(e){
          # print(paste("dist error", e))
          tryCatch({
            myData %>% lapply(FUN = function(x)list(fhat = kde(x$value)))
          }, error = function(f){
            print(f)
            return("Unable to determine quantile")
          })
        })
      myDist <- ifelse(class(param[[1]]) == "list", "kde", 
                       ifelse(class(param[[1]]) == "numeric", "gamma", "none"))
    }
    myReturn <- 
      lapply(1:length(value), FUN = function(x)quantileScore(value = value[[x]], params = param[[x]], dist = myDist) %>%
               round(digits = 2))
    print(myReturn)
    return(myReturn)
}
# 
# den <- density(myData[[1]]$value)
# dat <- data.frame(x = den$x, y = den$y)
# 
# fit.params <- fitdistr(myData[[1]]$value, "gamma")
# 
# ggplot(data = dat, aes(x = x,y = y)) + 
#   geom_point(size = 1) + geom_histogram(data = myData[[1]], aes(x = value, y = ..density..))     
#   geom_line(aes(x=dat$x, y=dgamma(dat$x,fit.params$estimate["shape"], fit.params$estimate["rate"])), color="red", size = 1) + 
#   theme_classic()
#   
# ggplot() + geom_histogram(myData[[1]], aes("value"))
# 
# temp2 <- fitdistr(myData[[1]]$value, "gamma")
# temp3 <- lapply(value, function(y) quantileScore(value = y, params = param[[1]], dist = myDist) %>%
         # round(digits = 2) )
# lapply(1:length(value), FUN = function(x)quantileScore(value = value[[x]], params = param[[x]], dist = myDist) %>%
#          round(digits = 2))  