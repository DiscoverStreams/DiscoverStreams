###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

### Get data

library(dplyr)
library(lubridate)
library(dataRetrieval)

setwd("C:\\Users\\jhammond\\Desktop\\low_flow_metrics\\metadata")

lowflowsites <- read.csv("gage_characteristics.csv", stringsAsFactors = FALSE )
lowflowsiteids <- substr(lowflowsites$site_id, 3,nchar(lowflowsites$site_id))

setwd("C:\\Users\\jhammond\\Desktop\\low_flow_metrics\\input")

for(i in seq_along(lowflowsiteids)){
  # i = 1
  try(current <- readNWISdv(siteNumber = lowflowsiteids[i], parameterCd = "00060"))
  try(current$asdate <- as.Date(current$Date))
  try(write.csv(current,file=paste0(lowflowsiteids[i],".csv"))) 
}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

### Calculate annual metrics only for complete data

library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(zoo)
library(EcoHydRology)
library(foreign)
library(tidyverse)

setwd("C:\\Users\\jhammond\\Desktop\\low_flow_metrics\\input")

usgs.files <- list.files(pattern = ".csv")

# get drainage area from here to calculate flow in mm/d

setwd("C:\\Users\\jhammond\\Desktop\\low_flow_metrics\\metadata")

lowflowsites <- read.csv("gage_characteristics.csv", stringsAsFactors = FALSE )
lowflowsites$site_id <- substr(lowflowsites$site_id, 3,nchar(lowflowsites$site_id))


for(i in seq_along(usgs.files)){
  
  # i = 1
  setwd("C:\\Users\\jhammond\\Desktop\\low_flow_metrics\\input")
  
  current <- read.csv(usgs.files[i], stringsAsFactors = FALSE)
  currentsite <- gsub(".csv","",usgs.files[i])
  
  currentarea <- subset(lowflowsites, lowflowsites$site_id == currentsite)
  currentarea <- currentarea$drainSqKm
  # get drainage area and then use this line
  current$mmd <- 1000*(current$X_00060_00003*0.028316847*86400)/(currentarea*1000000)
  
  
  current$month <- month(current$Date)
  current$year <- year(current$Date)
  current$climyear <- ifelse(current$month<4, current$year-1,current$year)
  currentcounts <- current %>% group_by(climyear) %>% summarise(count = sum(!is.na(X_00060_00003)))
  currentcounts <- subset(currentcounts, currentcounts$count>350)
  completeyears <- currentcounts$climyear
  current <- subset(current, current$climyear %in% completeyears)
  current$day7mean <- rollmean(current$mmd,7, align = "center", fill = NA)
  
  # Metrics that have to do with different elements of the natural flow regime, focusing on low flows
  
  # Poff et al., 1997 - https://watermark.silverchair.com/47-11-769.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAAngwggJ0BgkqhkiG9w0BBwagggJlMIICYQIBADCCAloGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMbaTouRL_llBh2He7AgEQgIICK5_Z0oD5CUZbGwR_9ut8n4fje9rOY3yBU0lPY8S2EO7pfAi10y-glDNc78IrxIaDwYNBeE2UhAGb43PLJvCF9wsW7pHohsxCCd5Ch6DgLVXfWnOq72tLqyIju0Jv8wcqk7FhRbkZEnNOAH61ag3qeQMrS3j3Ry6lIyZ8nVb5RfLKHPq-DvdFzUDOl5ZFc77SKiRY8oJZb91U-okv_2FgmfnE0cSMhdwuPh7cARKrI0CFGXcKNxp2LEF9aTmT3GI3wVxZ_lgY0Q4K04bH_ihnD7EgadBBCfdR7XniAeA3uoBvxRjLkE_XHMs1nSrQOv6WbpzaPUaHB-ySMKaWit9XSGDGuMsMCG047XAxqy17xy26nONlDWyuYG8X7-lJzAvcApL_hX4VwSWc05KcUwNQfpPSbAVGDvVevl0r9XFdYQRUEBmc-wfv1NwQYfChIJJWoTyZoZJ-JUHBIWESaAShbQn8JYt2jQEKTib42BaBPbCWZMjwNOtigCvWlPqNi089slvoJAPMaj0riXGoYyD7nAEEwA0T7Oyung5SpPFl0Cy6mLjEh0f00goV3hH3QMAcYCT-qSnpLMasQGWpSUjPQ7SvEJPaj2duR7LEvBwXDAOoKOfAftPv4wFOyPcDW6QvOLBgwrhav9-MXG4EJB-1ojIdz1ZaKznnY2P-YNONb4p4frmfnpRAxXsMYhYNSw4BeYSlEEkSLXB0rrRZkmWFBGrMWAmr_z3ruOPtCA
  
  # magnitude = minimum 7 day flow, volume deficit
  # duration = # of days below 5% threshold
  # frequency = # of periods below 5% threshold 
  # timing = date of minimum 7 day flow, 
  # rate of change = recession constant of 90 days prior?
  
  current <- subset(current, current$climyear>1979 & current$climyear<2019)
  
  # annual minimum 7-day flow
  
  lowest7dayperclimyear <- current %>% group_by(climyear) %>% summarise(min = min(day7mean, na.rm = TRUE))
  
  # date of the lowest 7-day flow
  
  lowest7dayperclimyeardate <- current %>% group_by(climyear) %>% slice(which.min(day7mean))
  lowest7dayperclimyeardate$yday <- yday(lowest7dayperclimyeardate$Date)
  lowest7dayperclimyeardate$climday_min_7 <- ifelse(lowest7dayperclimyeardate$yday>90, lowest7dayperclimyeardate$yday-90, lowest7dayperclimyeardate$yday+275)
  lowest7dayperclimyeardate <- lowest7dayperclimyeardate[,c("climday_min_7")] 
  
  # the annual number of days below the 2-percent threshold
  
  per2thresh <- quantile(current$mmd, 0.02, na.rm = TRUE)
  per70thresh <- quantile(current$mmd, 0.70, na.rm = TRUE)
  
  current$lessthan0.02 <- ifelse(current$mmd < per2thresh,1,0)
  daysbelow0.02climyear <- current %>% group_by(climyear) %>% summarise(days_below_0.02_quant = sum(lessthan0.02, na.rm = TRUE))
  daysbelow0.02climyear <- daysbelow0.02climyear[,"days_below_0.02_quant"]
  # total deviation below 2% thresh---- calc column that is flow - 2% flow, then sum if negative
  
  current$below0.02quantile <- ifelse(current$mmd<per2thresh,per2thresh-current$mmd,0)
  totalbelow0.02climyear <- current %>% group_by(climyear) %>% summarise(deficit_mm = sum(below0.02quantile, na.rm = TRUE))
  totalbelow0.02climyear <- totalbelow0.02climyear[,"deficit_mm"]
  
  # frequency below 2%
  
  current$binarybelow0.02quantile <- ifelse(current$mmd<per2thresh,1,0)
  
  years <- unique(current$climyear)
  
  fillwithannualvalues <- as.data.frame(matrix(data= NA, nrow = nrow(lowest7dayperclimyear), ncol = 9))
  colnames(fillwithannualvalues) <- c("totallowflowperiods","meanlowflowperiods","maxlowflowperiods","slope7","slope14","slope30","recess7","recess14","recess30")
  
  for(y in seq_along(years)){
    #y = 1
    currentyear <- subset(current,current$climyear == years[y])
    currentyear$cyearday <- 1:nrow(currentyear)
    
    try(flow.rle  <- rle(currentyear$binarybelow0.02quantile))
    try(flow.values <- as.data.frame(flow.rle$values))
    try(flow.values$lengths <- flow.rle$lengths)
    try(colnames(flow.values) <- c("value","length"))
    try(lowflowperiods <- subset(flow.values, flow.values$value == 1))
    
    try(totallowflowperiods <- as.numeric(length(lowflowperiods$length)))
    try(meanlengthlowflow <- as.numeric(mean(lowflowperiods$length)))
    try(maxlengthlowflow <- as.numeric(max(lowflowperiods$length)))
    
    # which is date of minimum 7 day flow? now count back 30 days and calculate recession   
    
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
      
      recessionperiod30 <- subset(recessionperiod30, recessionperiod30$mmd < per70thresh)
      recessionperiod14 <- subset(recessionperiod14, recessionperiod14$mmd < per70thresh)
      recessionperiod7 <- subset(recessionperiod7, recessionperiod7$mmd < per70thresh)
      
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
  
 try( alloutput <- cbind(lowest7dayperclimyear,lowest7dayperclimyeardate,daysbelow0.02climyear,totalbelow0.02climyear,fillwithannualvalues))
  
 try( alloutput$gage <- unique(current$site_no))
  
  setwd("C:\\Users\\jhammond\\Desktop\\low_flow_metrics\\annual_metrics")
  
  try(write.csv(alloutput, usgs.files[i]))
  
}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

### Calculate sites by site means and coefficients of variation for each metric

setwd("C:\\Users\\jhammond\\Desktop\\low_flow_metrics\\annual_metrics")

all <- list.files(pattern = ".csv")
alldata <- do.call(rbind, lapply(all, read.csv))

alldata_mean_annual <- alldata %>% group_by(gage) %>%   summarise_if(is.numeric, mean, na.rm = TRUE)
alldata_sd_annual <- alldata %>% group_by(gage) %>%   summarise_if(is.numeric, sd, na.rm = TRUE)
alldata_cv_annual <- alldata_sd_annual/alldata_mean_annual

setwd("C:\\Users\\jhammond\\Desktop\\low_flow_metrics\\summary")

write.csv(alldata_mean_annual, "CONUS_dudley_mean_annual_103120.csv")
write.csv(alldata_cv_annual, "CONUS_dudley_coefficient_variation_using_mean_103120.csv")
