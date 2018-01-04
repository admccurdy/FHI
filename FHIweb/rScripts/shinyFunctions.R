scoreClean <- function(scores, myNames){
  if(length(myNames) != length(scores)){
    myNames <- 1:length(scores)
  }
  returnFrame <- data.frame("Station" = myNames,
                            "Score" = unlist(scores))
  return(stargazer(returnFrame, summary = F, type = "html", rownames = F,
                   digits = 0))
}