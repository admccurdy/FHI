library(data.table)
library(dplyr)
library(ggplot2)
library(dtplyr)
library(lubridate)

snoMetrics <- readRDS("FHIweb/data/snotel/snoMetrics.RDS")
snoMax <- snoMetrics$max
snoMax <- snoMetrics$april
snoKey <- readRDS("FHIweb/data/snotel/snoTelKey.RDS")
stationDates <- snoMax %>% group_by(id) %>% summarise(startYear = min(year), endYear = max(year))
snoKey %<>% left_join(stationDates)
snoKey %<>% filter(startYear >= 1984 & endYear == 2018)


lowElev <- quantile(snoKey$elevation, .25)
highElev <- quantile(snoKey$elevation, .75)

snoKey %<>% filter(elevation <= lowElev | elevation >= highElev)
snoKey %<>% mutate(highStation = ifelse(elevation <= lowElev, F, T))

snoMax <- snoMax[id %in% snoKey$id]

snoMax %<>% left_join(snoKey) 
snoMax %<>% filter(waterYear <= 2000) %>% group_by(id) %>% summarise(stationMedian = median(value)) %>% left_join(snoMax)
snoMax %<>% mutate(PoM = value / stationMedian)
snoMax %>% filter(waterYear > 2000) %>% group_by(waterYear, highStation) %>% summarise(PoM = mean(PoM)) %>% 
  ggplot(aes(x = waterYear, y = PoM, color = highStation, fill = highStation)) + 
  geom_bar(stat = "identity", position = "dodge") + stat_smooth(se = F) + geom_hline(aes(yintercept = 1))

snoMax %<>% mutate(myDate = ymd(paste0(year, "/", month, "/", day)))
snoMax %<>% mutate(DoY = yday(myDate))
snoMax %<>% filter(waterYear <= 2000) %>% group_by(id) %>% summarise(medianPeakDay = median(DoY)) %>% left_join(snoMax)
snoMax %<>% mutate(PoM = DoY / medianPeakDay)
snoMax %>% filter(waterYear > 2000) %>% group_by(waterYear, highStation) %>% summarise(PoM = mean(PoM)) %>% 
  ggplot(aes(x = waterYear, y = PoM, color = highStation, fill = highStation)) + 
  geom_bar(stat = "identity", position = "dodge") + stat_smooth(se = F) + geom_hline(aes(yintercept = 1))

