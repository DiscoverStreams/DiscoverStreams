########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
############################## read in gage locations and get list of unique USGS gages
library(sf)

setwd("C:\\Users\\jhammond\\Desktop\\which_depeletion_metrics_sensitive")

gage_location <- st_read("Gages_Depletion.gpkg")
gages <- unique(gage_location$STAID)

########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
############################## download USGS gage time series for 04/01/80 to 03/31/21 to only include approved data from recent hydroclimate
### Access data and only download data for complete climate years

library(dplyr)
library(lubridate)
library(dataRetrieval)

setwd("C:\\Users\\jhammond\\Desktop\\which_depeletion_metrics_sensitive\\complete_years_data")

for(i in seq_along(gages)){
  # i = 1
  try(current <- readNWISdv(siteNumber = gages[i], parameterCd = "00060", startDate = "1980-04-01", endDate = "2021-03-31"))
  try(current$Date <- as.Date(current$Date))
  
  # assess data completeness, only keep complete years
  
  try(current$month <- month(current$Date))
  try(current$year <- year(current$Date))
  try(current$climyear <- ifelse(current$month<4, current$year-1,current$year))
  try(currentcounts <- current %>% group_by(climyear) %>% summarise(count = sum(!is.na(X_00060_00003))))
  try(currentcounts <- subset(currentcounts, currentcounts$count>350))
  try(completeyears <- currentcounts$climyear)
  try(current <- subset(current, current$climyear %in% completeyears))
  
  # calculate flow in cubic meters per second
  try(current$cms <- 0.028316847*current$X_00060_00003)
  try(current$cfs <- current$X_00060_00003)
  
  # pull NWIS drainage area for site and calculate streamflow in mm/d
  
  try(siteinfoNWIS <- readNWISsite(siteNumbers = gages[i]))
  try(currentareasqkm <- siteinfoNWIS$drain_area_va*2.58999)
  try(current$mmd <- 1000*(current$X_00060_00003*0.028316847*86400)/(currentareasqkm*1000000))
  
  try(current <- current[,c("site_no","Date","climyear","cfs","cms","mmd")])
  
  try(write.csv(current,file=paste0(gages[i],".csv"))) 
  
}

########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
############################## calculate annual metrics for the climate year. Climate year generally isolates dry periods better than water year because some dry periods continue from Sep into Oct or Nov
############################## For example climate year 2020 = 4/01/20 to 3/31/21

library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(zoo)
library(EcoHydRology)
library(foreign)
library(tidyverse)

setwd("C:\\Users\\jhammond\\Desktop\\which_depeletion_metrics_sensitive\\complete_years_data")

usgs.files <- list.files(pattern = ".csv")

for(i in seq_along(usgs.files)){
    # i = 1
  setwd("C:\\Users\\jhammond\\Desktop\\which_depeletion_metrics_sensitive\\complete_years_data")
  current <- read.csv(usgs.files[i], stringsAsFactors = FALSE)
  current$day7mean <- rollmean(current$mmd,7, align = "center", fill = NA)
  # determine season of the climate year
  current$month <- month(current$Date)
  current$season <- ifelse(current$month > 3 & current$month < 7, "spring",
                           ifelse(current$month > 6 & current$month < 10, "summer",
                                  ifelse(current$month > 9 & current$month, "fall","winter")))
  record_length <- as.numeric(length(unique(current$climyear)))
  
  # calculate annual flow metrics if 10 or more years of complete data
  if(record_length > 9){
  # a few metrics that have to do with different elements of the natural flow regime, focusing on low flows
  
  # magnitude = minimum 7 day flow, volume deficit, total annual flow
  # duration = # of days below 10% threshold
  # frequency = # of periods below 10% threshold 
  # timing = date of minimum 7 day flow, 
  # rate of change = recession constant of X days prior, slope of X days prior
  
  # total annual flow
  
  totalannualflow <- current %>% group_by(climyear) %>% summarise(total_annual_flow_mm = sum(mmd, na.rm = TRUE))
  totalannualflow <- totalannualflow[,"total_annual_flow_mm"]
  wetthresh <- quantile(totalannualflow, 0.75, na.rm = TRUE)
  drythresh <- quantile(totalannualflow, 0.25, na.rm = TRUE)
  
  # annual minimum 7-day flow
  
  lowest7dayperclimyear <- current %>% group_by(climyear) %>% summarise(min = min(day7mean, na.rm = TRUE))
  
  # date of the lowest 7-day flow
  
  lowest7dayperclimyeardate <- current %>% group_by(climyear) %>% slice(which.min(day7mean))
  lowest7dayperclimyeardate$yday <- yday(lowest7dayperclimyeardate$Date)
  lowest7dayperclimyeardate$climday_min_7 <- ifelse(lowest7dayperclimyeardate$yday>90, lowest7dayperclimyeardate$yday-90, lowest7dayperclimyeardate$yday+275)
  lowest7dayperclimyeardate <- lowest7dayperclimyeardate[,c("climday_min_7")] 
  
  # the annual number of days below the 5-percent threshold
  
  thresh0.1 <- quantile(current$mmd, 0.1, na.rm = TRUE)

  current$lessthan0.1 <- ifelse(current$mmd < thresh0.1,1,0)
  daysbelow0.1climyear <- current %>% group_by(climyear) %>% summarise(days_below_0.1_quant = sum(lessthan0.1, na.rm = TRUE))
  daysbelow0.1climyear <- daysbelow0.1climyear[,"days_below_0.1_quant"]
  # total deviation below 2% thresh---- calc column that is flow - 5% flow, then sum if negative
  
  current$below0.1quantile <- ifelse(current$mmd<thresh0.1,thresh0.1-current$mmd,0)
  totalbelow0.1climyear <- current %>% group_by(climyear) %>% summarise(deficit_mm = sum(below0.1quantile, na.rm = TRUE))
  totalbelow0.1climyear <- totalbelow0.1climyear[,"deficit_mm"]
  
  # frequency below 2%
  
  current$binarybelow0.1quantile <- ifelse(current$mmd < thresh0.1,1,0)
  years <- unique(current$climyear)
  fillwithannualvalues <- as.data.frame(matrix(data= NA, nrow = nrow(lowest7dayperclimyear), ncol = 9))
  colnames(fillwithannualvalues) <- c("totallowflowperiods","meanlowflowperiods","maxlowflowperiods","slope7","slope14","slope30","recess7","recess14","recess30")
  
  for(y in seq_along(years)){
    #y = 1
    currentyear <- subset(current,current$climyear == years[y])
    currentyear$cyearday <- 1:nrow(currentyear)
    
    try(flow.rle  <- rle(currentyear$binarybelow0.1quantile))
    try(flow.values <- as.data.frame(flow.rle$values))
    try(flow.values$lengths <- flow.rle$lengths)
    try(colnames(flow.values) <- c("value","length"))
    try(lowflowperiods <- subset(flow.values, flow.values$value == 1))
    
    try(totallowflowperiods <- as.numeric(length(lowflowperiods$length)))
    try(meanlengthlowflow <- as.numeric(mean(lowflowperiods$length)))
    try(maxlengthlowflow <- as.numeric(max(lowflowperiods$length)))
    
    # which is date of minimum 7 day flow? now count back X days and calculate recession   
    
    lowest7daythisyear <- currentyear %>% slice(which.min(day7mean))
    daytocountbackfrom <- as.numeric(lowest7daythisyear$cyearday)
    
    # need if else here so if negative....
    firstday30 <- daytocountbackfrom-30
    
    try(if(firstday30>0){
      
      recessionperiod30 <- currentyear[firstday30:daytocountbackfrom,]
      firstday14 <- daytocountbackfrom-14
      recessionperiod14 <- currentyear[firstday14:daytocountbackfrom,]
      firstday7 <- daytocountbackfrom-7
      recessionperiod7 <- currentyear[firstday7:daytocountbackfrom,]
      
      recessionperiod30$mmd <- ifelse(recessionperiod30$mmd<0.00001,0.00001,recessionperiod30$mmd)
      recessionperiod14$mmd <- ifelse(recessionperiod14$mmd<0.00001,0.00001,recessionperiod14$mmd)
      recessionperiod7$mmd <- ifelse(recessionperiod7$mmd<0.00001,0.00001,recessionperiod7$mmd)
      
      # can use lines commented out below to limit recession calculation only to days with flow below X percentile to limit influence of peak variability on recession
    #  recessionperiod30 <- subset(recessionperiod30, recessionperiod30$mmd < perXthresh)
    #  recessionperiod14 <- subset(recessionperiod14, recessionperiod14$mmd < perXthresh)
    #  recessionperiod7 <- subset(recessionperiod7, recessionperiod7$mmd < perXthresh)
      
      recessionperiod30$lnq <- log(recessionperiod30$mmd)
      recessionperiod14$lnq <- log(recessionperiod14$mmd)
      recessionperiod7$lnq <- log(recessionperiod7$mmd)
      
      recess30 <- lm(lnq ~ cyearday, data = recessionperiod30)
      recess30 <- recess30$coefficients[2]
      recess14 <- lm(lnq ~ cyearday, data = recessionperiod14)
      recess14 <- recess14$coefficients[2]
      recess7 <- lm(lnq ~ cyearday, data = recessionperiod7)
      recess7 <- recess7$coefficients[2]
      # rate of change
      
      slope30 <- lm(mmd ~ cyearday, data = recessionperiod30)
      slope30 <- slope30$coefficients[2]
      slope14 <- lm(mmd ~ cyearday, data = recessionperiod14)
      slope14 <- slope14$coefficients[2]
      slope7 <- lm(mmd ~ cyearday, data = recessionperiod7)
      slope7 <- slope7$coefficients[2]
      
      ######### ADD MORE COLUMNS TO OUTPUT
      
    } else{
      slope30 <- NA
      slope14 <- NA
      slope7 <- NA
      recess30 <- NA
      recess14 <- NA
      recess7 <- NA
    })
    
    try(fillrows <- c(totallowflowperiods,meanlengthlowflow,maxlengthlowflow,slope7,slope14,slope30,recess7,recess14,recess30))
    try(fillwithannualvalues[y,] <- fillrows)
    
  }
  }
  
  ################################################################
  # now many of the same metrics at seasonal resolution
  ################################################################
  # still only calculate metrics if 10 or more years of complete data available
  ################################################################
  
  if(record_length > 9){
    # a few metrics that have to do with different elements of the natural flow regime, focusing on low flows
    
    # magnitude = minimum 7 day flow, volume deficit, total annual flow
    # duration = # of days below 10% threshold
    # frequency = # of periods below 10% threshold 
    # timing = date of minimum 7 day flow, 
    # rate of change = recession constant of X days prior, slope of X days prior
    
    # total annual flow
    
    totalseasonalflow <- current %>% group_by(climyear,season) %>% summarise(total_seasonal_flow_mm = sum(mmd, na.rm = TRUE))
    
    # annual minimum 7-day flow
    
    lowest7dayperclimyearandseason <- current %>% group_by(climyear,season) %>% summarise(min = min(day7mean, na.rm = TRUE))
    
    # date of the lowest 7-day flow
    
    lowest7dayperclimyearandseasondate <- current %>% group_by(climyear,season) %>% slice(which.min(day7mean))
    lowest7dayperclimyearandseasondate$yday <- yday(lowest7dayperclimyearandseasondate$Date)
    lowest7dayperclimyearandseasondate$climday_min_7 <- ifelse(lowest7dayperclimyearandseasondate$yday>90, lowest7dayperclimyearandseasondate$yday-90, lowest7dayperclimyearandseasondate$yday+275)

    # the annual number of days below the 5-percent threshold
    # thresh0.1 <- quantile(current$mmd, 0.1, na.rm = TRUE) # already calculated in annual loop
    
    # current$lessthan0.1 <- ifelse(current$mmd < thresh0.1,1,0) # already calculated in annual loop
    daysbelow0.1climyearandseason <- current %>% group_by(climyear,season) %>% summarise(days_below_0.1_quant = sum(lessthan0.1, na.rm = TRUE))
    # total deviation below 2% thresh---- calc column that is flow - 5% flow, then sum if negative
    
    # current$below0.1quantile <- ifelse(current$mmd<thresh0.1,thresh0.1-current$mmd,0) # already calculated in annual loop
    totalbelow0.1climyearandseason <- current %>% group_by(climyear,season) %>% summarise(deficit_mm = sum(below0.1quantile, na.rm = TRUE))

    # frequency below 2%
    
    # current$binarybelow0.1quantile <- ifelse(current$mmd < thresh0.1,1,0) # already calculated in annual loop
    years <- unique(current$climyear)
    fillwithannualvaluesseason <- as.data.frame(matrix(data= NA, nrow = 0, ncol = 5))
    colnames(fillwithannualvaluesseason) <- c("totallowflowperiods","meanlowflowperiods","maxlowflowperiods","year","season")
    
    seasons <- c("spring","summer","fall","winter")
    
    for(y in 1:length(years)){
      #y = 1
      currentyear <- subset(current,current$climyear == years[y])
      currentyear$cyearday <- 1:nrow(currentyear)
      
      for(s in 1:4){
      #s = 2
        currentseason <- subset(currentyear,currentyear$season == seasons[s])
        
      try(flow.rle.season  <- rle(currentseason$binarybelow0.1quantile))
      try(flow.values.season <- as.data.frame(flow.rle.season$values))
      try(flow.values.season$lengths <- flow.rle.season$lengths)
      try(colnames(flow.values.season) <- c("value","length"))
      try(lowflowperiods.season <- subset(flow.values.season, flow.values.season$value == 1))
      
      try(totallowflowperiods.season <- as.numeric(length(lowflowperiods.season$length)))
      try(meanlengthlowflow.season <- as.numeric(mean(lowflowperiods.season$length)))
      try(maxlengthlowflow.season <- as.numeric(max(lowflowperiods.season$length)))
      
      try(fillrowsseason <- c(totallowflowperiods.season,meanlengthlowflow.season,maxlengthlowflow.season, years[y],seasons[s]))
      try(fillwithannualvaluesseason <- rbind(fillwithannualvaluesseason,fillrowsseason))
      
    }
  
  
    }
    
    colnames(fillwithannualvaluesseason) <- c("totallowflowperiods","meanlowflowperiods","maxlowflowperiods","year","season")
    
  
  }
  
    ################################################################
    # now annual temperature metrics
    ################################################################
    # still only calculate metrics if 10 or more years of complete data available
    ################################################################

  # line to create a fake temperature time series in the meantime  
   current$temp <- runif(length(current$Date), min = -5, max = 20) 

    if(record_length > 9){
      
      longtermmeantemp <- mean(current$temp, na.rm = TRUE)
      current$temp7day <- rollmean(current$temp,7, align = "center", fill = NA)

      sdevtemp <- sd(current$temp, na.rm = TRUE)
      
      annualmeantemp <- current %>% group_by(climyear) %>% summarise(annualmeantemp = mean(temp, na.rm = TRUE))
      annualmintemp <-  current %>% group_by(climyear) %>% summarise(annualmintemp = min(temp, na.rm = TRUE))
      annualmaxtemp <-  current %>% group_by(climyear) %>% summarise(annualmaxtemp = max(temp, na.rm = TRUE))
      annualsdtemp <- current %>% group_by(climyear) %>% summarise(annualsdtemp = sd(temp, na.rm = TRUE))
      annualrange <- annualmaxtemp$annualmaxtemp - annualmintemp$annualmintemp
      annualcv <- annualsdtemp$annualsdtemp / annualmeantemp$annualmeantemp
      # degree days calculation (based on a temperature threshold of 10 deg C from Charnov, E.L., and Gillooly, J.F. 2003. Thermal time: body size, food quality and the 10 degree C rule. Evol. Ecol. Res. 5(1):
      # 43-51)
        
      current$isabove10c <- ifelse(current$temp > 10, 1, 0)
      current$degreedayc <- current$temp-10
      degreedayctotal <-  current %>% filter(isabove10c > 0) %>% group_by(climyear) %>% summarise(degreedayctotal = sum(degreedayc, na.rm = TRUE))
      
      # metrics based on 7-day average temps
      # annual minimum and max 7-day temp
      
      min7daytempperclimyear <- current %>% group_by(climyear) %>% summarise(min7daytemp = min(temp7day, na.rm = TRUE))
      max7daytempperclimyear <- current %>% group_by(climyear) %>% summarise(max7daytemp = max(temp7day, na.rm = TRUE))
      
      # date of the lowest and highest 7-day temp
      
      min7daytempperclimyeardate <- current %>% group_by(climyear) %>% slice(which.min(day7mean))
      min7daytempperclimyeardate$yday <- yday(min7daytempperclimyeardate$Date)
      min7daytempperclimyeardate$climday_min_7 <- ifelse(min7daytempperclimyeardate$yday>90, min7daytempperclimyeardate$yday-90, min7daytempperclimyeardate$yday+275)
      
      max7daytempperclimyeardate <- current %>% group_by(climyear) %>% slice(which.max(day7mean))
      max7daytempperclimyeardate$yday <- yday(max7daytempperclimyeardate$Date)
      max7daytempperclimyeardate$climday_max_7 <- ifelse(max7daytempperclimyeardate$yday>90, max7daytempperclimyeardate$yday-90, max7daytempperclimyeardate$yday+275)
      
      # calculate standardized temperature
      
      current$standardtemp <- (current$temp-longtermmeantemp)/sdevtemp
      current$great1st <- ifelse(current$standardtemp > 1, 1,0)
      current$lessneg1st <- ifelse(current$standardtemp < -1, 1,0)
      
      annualcolddays <- current %>% group_by(climyear) %>% summarise(annualcolddays = sum(lessneg1st, na.rm = TRUE))
      annualwarmdays <- current %>% group_by(climyear) %>% summarise(annualwarmdays = sum(great1st, na.rm = TRUE))
      
      years <- unique(current$climyear)
      fillwithannualtempvalues <- as.data.frame(matrix(data= NA, nrow = 0, ncol = 7))
      colnames(fillwithannualtempvalues) <- c("totalwarmtempperiods","meanlengthwarmtemp","maxlengthwarmtemp","totalcoldtempperiods","meanlengthcoldtemp","maxlengthcoldtemp","year")
      
      for(y in 1:length(years)){
        #y = 1
        currentyear <- subset(current,current$climyear == years[y])
        currentyear$cyearday <- 1:nrow(currentyear)
      
          try(warmtemp.rle  <- rle(currentyear$great1st))
          try(warmtemp.values <- as.data.frame(warmtemp.rle$values))
          try(warmtemp.values$lengths <- warmtemp.rle$lengths)
          try(colnames(warmtemp.values) <- c("value","length"))
          try(warmtempperiods <- subset(warmtemp.values, warmtemp.values$value == 1))
          try(totalwarmtempperiods <- as.numeric(length(warmtempperiods$length)))
          try(meanlengthwarmtemp <- as.numeric(mean(warmtempperiods$length)))
          try(maxlengthwarmtemp <- as.numeric(max(warmtempperiods$length)))
        
        try(coldtemp.rle  <- rle(currentyear$great1st))
        try(coldtemp.values <- as.data.frame(coldtemp.rle$values))
        try(coldtemp.values$lengths <- coldtemp.rle$lengths)
        try(colnames(coldtemp.values) <- c("value","length"))
        try(coldtempperiods <- subset(coldtemp.values, coldtemp.values$value == 1))
        try(totalcoldtempperiods <- as.numeric(length(coldtempperiods$length)))
        try(meanlengthcoldtemp <- as.numeric(mean(coldtempperiods$length)))
        try(maxlengthcoldtemp <- as.numeric(max(coldtempperiods$length)))
          
          try(fillrowstempannual <- c(totalwarmtempperiods,meanlengthwarmtemp,maxlengthwarmtemp,totalcoldtempperiods,meanlengthcoldtemp,maxlengthcoldtemp, years[y]))
          try(fillwithannualtempvalues <- rbind(fillwithannualtempvalues,fillrowstempannual))
          
        }
      
      colnames(fillwithannualtempvalues) <- c("totalwarmtempperiods","meanlengthwarmtemp","maxlengthwarmtemp","totalcoldtempperiods","meanlengthcoldtemp","maxlengthcoldtemp","year")
      
      
      
    }
  
    ################################################################
    # now seasonal temperature metrics
    ################################################################
    # still only calculate metrics if 10 or more years of complete data available
    ################################################################
    
  if(record_length > 9){
    
    seasonalmeantemp <- current %>% group_by(climyear, season) %>% summarise(seasonalmeantemp = mean(temp, na.rm = TRUE))
    seasonalmintemp <-  current %>% group_by(climyear, season) %>% summarise(seasonalmintemp = min(temp, na.rm = TRUE))
    seasonalmaxtemp <-  current %>% group_by(climyear, season) %>% summarise(seasonalmaxtemp = max(temp, na.rm = TRUE))
    seasonalsdtemp <- current %>% group_by(climyear, season) %>% summarise(seasonalsdtemp = sd(temp, na.rm = TRUE))
    seasonalrange <- seasonalmaxtemp$seasonalmaxtemp - seasonalmintemp$seasonalmintemp
    seasonalcv <- seasonalsdtemp$seasonalsdtemp / seasonalmeantemp$seasonalmeantemp
    # degree days calculation (based on a temperature threshold of 10 deg C from Charnov, E.L., and Gillooly, J.F. 2003. Thermal time: body size, food quality and the 10 degree C rule. Evol. Ecol. Res. 5(1):
    # 43-51)

    degreedayc.season <-  current %>% filter(isabove10c > 0) %>% group_by(climyear,season) %>% summarise(degreedayctotal = sum(degreedayc, na.rm = TRUE))
    
    # metrics based on 7-day average temps
    # seasonal minimum and max 7-day temp
    
    min7daytempperclimyear.season <- current %>% group_by(climyear,season) %>% summarise(min7daytemp = min(temp7day, na.rm = TRUE))
    max7daytempperclimyear.season <- current %>% group_by(climyear,season) %>% summarise(max7daytemp = max(temp7day, na.rm = TRUE))
    
    # date of the lowest and highest 7-day temp
    
    min7daytempperclimyeardate.season <- current %>% group_by(climyear,season) %>% slice(which.min(day7mean))
    min7daytempperclimyeardate.season$yday <- yday(min7daytempperclimyeardate.season$Date)
    min7daytempperclimyeardate.season$climday_min_7 <- ifelse(min7daytempperclimyeardate.season$yday>90, min7daytempperclimyeardate.season$yday-90, min7daytempperclimyeardate.season$yday+275)
    
    max7daytempperclimyeardate.season <- current %>% group_by(climyear,season) %>% slice(which.max(day7mean))
    max7daytempperclimyeardate.season$yday <- yday(max7daytempperclimyeardate.season$Date)
    max7daytempperclimyeardate.season$climday_max_7 <- ifelse(max7daytempperclimyeardate.season$yday>90, max7daytempperclimyeardate.season$yday-90, max7daytempperclimyeardate.season$yday+275)
    
    seasonalcolddays <- current %>% group_by(climyear,season) %>% summarise(seasonalcolddays = sum(lessneg1st, na.rm = TRUE))
    seasonalwarmdays <- current %>% group_by(climyear,season) %>% summarise(seasonalwarmdays = sum(great1st, na.rm = TRUE))
    
    years <- unique(current$climyear)
    fillwithseasonaltempvalues <- as.data.frame(matrix(data= NA, nrow = 0, ncol = 8))
    colnames(fillwithseasonaltempvalues) <- c("totalwarmtempperiods","meanlengthwarmtemp","maxlengthwarmtemp","totalcoldtempperiods","meanlengthcoldtemp","maxlengthcoldtemp","year","season")
    
    for(y in 1:length(years)){
      #y = 1
      currentyear <- subset(current,current$climyear == years[y])
      currentyear$cyearday <- 1:nrow(currentyear)
      
      for(s in 1:4){
        #s = 2
        currentseason <- subset(currentyear,currentyear$season == seasons[s])
        
      try(warmtemp.rle.season  <- rle(currentseason$great1st))
      try(warmtemp.values.season <- as.data.frame(warmtemp.rle.season$values))
      try(warmtemp.values.season$lengths <- warmtemp.rle.season$lengths)
      try(colnames(warmtemp.values.season) <- c("value","length"))
      try(warmtempperiods.season <- subset(warmtemp.values.season, warmtemp.values.season$value == 1))
      try(totalwarmtempperiods.season <- as.numeric(length(warmtempperiods.season$length)))
      try(meanlengthwarmtemp.season <- as.numeric(mean(warmtempperiods.season$length)))
      try(maxlengthwarmtemp.season <- as.numeric(max(warmtempperiods.season$length)))
      
      try(coldtemp.rle.season  <- rle(currentseason$great1st))
      try(coldtemp.values.season <- as.data.frame(coldtemp.rle.season$values))
      try(coldtemp.values.season$lengths <- coldtemp.rle.season$lengths)
      try(colnames(coldtemp.values.season) <- c("value","length"))
      try(coldtempperiods.season <- subset(coldtemp.values.season, coldtemp.values.season$value == 1))
      try(totalcoldtempperiods.season <- as.numeric(length(coldtempperiods.season$length)))
      try(meanlengthcoldtemp.season <- as.numeric(mean(coldtempperiods.season$length)))
      try(maxlengthcoldtemp.season <- as.numeric(max(coldtempperiods.season$length)))
      
      try(fillrowstempseasonal <- c(totalwarmtempperiods.season,meanlengthwarmtemp.season,maxlengthwarmtemp.season,totalcoldtempperiods.season,meanlengthcoldtemp.season,maxlengthcoldtemp.season, years[y], seasons[s]))
      try(fillwithseasonaltempvalues <- rbind(fillwithseasonaltempvalues,fillrowstempseasonal))
      
    }
    }
    colnames(fillwithseasonaltempvalues) <- c("totalwarmtempperiods","meanlengthwarmtemp","maxlengthwarmtemp","totalcoldtempperiods","meanlengthcoldtemp","maxlengthcoldtemp","year","season")
  
  }
  
  ########################################################################################
  ########################################################################################
  ########################################################################################
  ####################                   Write files
  ########################################################################################
  ########################################################################################
  ########################################################################################
  
  # annual flow metrics
  
  try(alloutput <- cbind(lowest7dayperclimyear,lowest7dayperclimyeardate,daysbelow0.1climyear,totalbelow0.1climyear,fillwithannualvalues, totalannualflow))
  try(alloutput$wet_dry_normal <- ifelse(alloutput$total_annual_flow_mm >= wetthresh, "wet",ifelse(alloutput$total_annual_flow_mm < drythresh,"dry","normal")))
  try(alloutput$gage <- unique(current$site_no))
  
  # seasonal flow metrics
  try(allseasonaloutput <- cbind(totalseasonalflow,
                                 lowest7dayperclimyearandseason,
                                 lowest7dayperclimyearandseasondate,
                                 daysbelow0.1climyearandseason,
                                 totalbelow0.1climyearandseason,
                                 fillwithannualvaluesseason))
  try(allseasonaloutput <- allseasonaloutput[c("climyear...1"   ,        "season...2" ,            "total_seasonal_flow_mm", "min" ,           
                                               "climday_min_7"   ,            "days_below_0.1_quant"  ,  "deficit_mm" ,"totallowflowperiods","meanlowflowperiods", "maxlowflowperiods")])
  try(colnames(allseasonaloutput) <- c("climyear", "season" , "total_seasonal_flow_mm", "min7dayflow" ,  "climday_min7dayflow"   ,  "days_below_0.1_quant"  ,  "deficit_mm" ,"totallowflowperiods","meanlowflowperiods", "maxlowflowperiods"))
  try(allseasonaloutput$gage <- unique(current$site_no))
  
  # annual temp metrics
  try(alltempoutput <- cbind(annualmeantemp,
                             annualmintemp,
                             annualmaxtemp,
                             annualrange,
                             annualcv,
                             degreedayctotal,
                             min7daytempperclimyear,
                             max7daytempperclimyear,
                             min7daytempperclimyeardate,
                             max7daytempperclimyeardate,
                             annualcolddays,
                             annualwarmdays,
                             fillwithannualtempvalues))
  try(alltempoutput <- alltempoutput[,c("climyear","annualmeantemp","annualmintemp","annualmaxtemp","annualrange","annualcv","degreedayctotal","min7daytemp","max7daytemp"  ,
                                        "climday_min_7","climday_max_7","annualcolddays","annualwarmdays",     
                                        "totalwarmtempperiods","meanlengthwarmtemp","maxlengthwarmtemp","totalcoldtempperiods",  
                                        "meanlengthcoldtemp","maxlengthcoldtemp")])
  try(alltempoutput$gage <- unique(current$site_no))
  
  # seasonal temp metrics
  
  try(allseasonaltempoutput <- cbind(seasonalmeantemp,
                                     seasonalmintemp,
                                     seasonalmaxtemp,
                                     seasonalrange,
                                     seasonalcv,
                                     degreedayc.season,
                                     min7daytempperclimyear.season,
                                     max7daytempperclimyear.season,
                                     min7daytempperclimyeardate.season,
                                     max7daytempperclimyeardate.season,
                                     seasonalcolddays,
                                     seasonalwarmdays,
                                     fillwithseasonaltempvalues))
  
try(allseasonaltempoutput <- allseasonaltempoutput[,c("climyear...1","season...2","seasonalmeantemp","seasonalmintemp","seasonalmaxtemp","degreedayctotal","min7daytemp","max7daytemp",
"climday_min_7","climday_max_7","seasonalcolddays","seasonalwarmdays","totalwarmtempperiods","meanlengthwarmtemp",
"maxlengthwarmtemp","totalcoldtempperiods","meanlengthcoldtemp","maxlengthcoldtemp")])
  try(colnames(allseasonaltempoutput) <- c("climyear","season","seasonalmeantemp","seasonalmintemp","seasonalmaxtemp","degreedayctotal","min7daytemp","max7daytemp",
                                                        "climday_min_7","climday_max_7","seasonalcolddays","seasonalwarmdays","totalwarmtempperiods","meanlengthwarmtemp",
                                                        "maxlengthwarmtemp","totalcoldtempperiods","meanlengthcoldtemp","maxlengthcoldtemp"))
  try(allseasonaltempoutput$gage <- unique(current$site_no))

  
  
  
  setwd("C:\\Users\\jhammond\\Desktop\\which_depeletion_metrics_sensitive\\metrics_for_sites_with_10_or_more_complete_years_data")
  
  try(write.csv(alloutput, paste0(usgs.files[i], "_annual_flow_metrics.csv") ))
  try(write.csv(allseasonaloutput, paste0(usgs.files[i], "_seasonal_flow_metrics.csv") ))
  try(write.csv(alltempoutput, paste0(usgs.files[i], "_annual_temp_metrics.csv") ))
  try(write.csv(allseasonaltempoutput, paste0(usgs.files[i], "_seasonal_temp_metrics.csv") ))
  
}

########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
############################## read and combine into a master file

setwd("C:\\Users\\jhammond\\Desktop\\which_depeletion_metrics_sensitive\\metrics_for_sites_with_10_or_more_complete_years_data")

all.annual.flow.files <- list.files(pattern = "_annual_flow_metrics.csv")
all.annual.flow.data <- do.call(rbind, lapply(all.annual.flow.files, read.csv))

all.seasonal.flow.files <- list.files(pattern = "_seasonal_flow_metrics.csv")
all.seasonal.flow.data <- do.call(plyr::rbind.fill, lapply(all.seasonal.flow.files, read.csv))

all.annual.temp.files <- list.files(pattern = "_annual_temp_metrics.csv")
all.annual.temp.data <- do.call(rbind, lapply(all.annual.temp.files, read.csv))

all.seasonal.temp.files <- list.files(pattern = "_seasonal_temp_metrics.csv")
all.seasonal.temp.data <- do.call(plyr::rbind.fill, lapply(all.seasonal.temp.files, read.csv))

setwd("C:\\Users\\jhammond\\Desktop\\which_depeletion_metrics_sensitive\\")

write.csv(all.annual.flow.data, "all.annual.flow.metrics.102021.csv")
write.csv(all.seasonal.flow.data, "all.seasonal.flow.metrics.102021.csv")

write.csv(all.annual.temp.data, "all.annual.temp.metrics.102021.csv")
write.csv(all.seasonal.temp.data, "all.seasonal.temp.metrics.102021.csv")
# for later use, you could subset to a random year for each site based on classification as "wet","normal","dry"


############# what fraction of years wet or dry at each site on average
library(dplyr)
library(ggplot2)

setwd("C:\\Users\\jhammond\\Desktop\\which_depeletion_metrics_sensitive\\")

all.annual.flow.data <- read.csv("all.annual.flow.metrics.102021.csv")

all.annual.flow.data.summary <- all.annual.flow.data %>% group_by(gage) %>% summarise(years = n())
all.annual.flow.data.wet.dry.normal.summary <- all.annual.flow.data %>% group_by(gage) %>% count(wet_dry_normal)

all.annual.summary <- merge(all.annual.flow.data.wet.dry.normal.summary, all.annual.flow.data.summary, by = "gage")
all.annual.summary$fraction <- all.annual.summary$n/all.annual.summary$years

ggplot(all.annual.summary, aes(x = fraction))+geom_histogram()+facet_grid(rows = wet_dry_normal)
