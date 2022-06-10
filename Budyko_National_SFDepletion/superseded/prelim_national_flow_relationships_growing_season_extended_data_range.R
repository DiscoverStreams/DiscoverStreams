###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

path <- "C:\\School\\R\\Projects\\RManning_Masters_Project\\"
setwd(path)

######### Get data

library(dplyr)
library(lubridate)
library(dataRetrieval)
library(data.table)


ArkansasSites <- read.csv(paste(path,"Arkansas_Basin_IDs.csv", sep = ""), stringsAsFactors = FALSE )
ArkansasSiteIds <- substr(ArkansasSites$site_id, 3,nchar(ArkansasSites$site_id))

setwd(paste(path,"Data", sep=""))

### Loop through ArkansasSites and write a csv file for each station with parameter 00060 
### Parameter "00060" = Discharge, cubic feet per second
for(i in seq_along(ArkansasSiteIds)){
  # i = 1
  try(current <- readNWISdv(siteNumber = ArkansasSiteIds[i], parameterCd = "00060"))
  try(current$asdate <- as.Date(current$Date))
  try(write.csv(current,file=paste0(ArkansasSiteIds[i],".csv"))) 
}

setwd(paste(path,"Data\\Daily_Data", sep=""))

### usgs.files is a list of all the files saved in daily_data, with one file for each site
usgs.files <- list.files(pattern = ".csv")

## Read in meteorological data

#gridmet_P_mm <- read.csv(paste(path,"Data\\gridMET\\gridmet_P_mm_all_CONUS_gages2.csv", sep = ""), stringsAsFactors = FALSE, check.names = FALSE) 
#gridmet_PDSI <- read.csv(paste(path,"Data\\gridMET\\gridmet_PDSI_all_CONUS_gages2.csv", sep = ""), stringsAsFactors = FALSE, check.names = FALSE) 
#gridmet_PET_mm <- read.csv(paste(path,"Data\\gridMET\\gridmet_PET_mm_all_CONUS_gages2.csv", sep = ""), stringsAsFactors = FALSE, check.names = FALSE) 
#gridmet_spei14 <- read.csv(paste(path,"Data\\gridMET\\gridmet_spei14d.csv", sep = ""), stringsAsFactors = FALSE, check.names = FALSE) 

#climgrid_PET_mm <- read.csv(paste(path,"Data\\gridMET\\MWBM_PET_mm_all_CONUS_gages2_climgrid.csv", sep = ""), stringsAsFactors = FALSE) 
#climgrid_P_mm <- read.csv(paste(path,"Data\\gridMET\\MWBM_PRCP_mm_all_CONUS_gages2_climgrid.csv", sep = ""), stringsAsFactors = FALSE) 
#climgrid_SWE <- read.csv(paste(path,"Data\\gridMET\\MWBM_SWE_mm_all_CONUS_gages2_climgrid.csv", sep = ""), stringsAsFactors = FALSE) 
#climgrid_Tmean_C <- read.csv(paste(path,"Data\\gridMET\\MWBM_Tmean_C_all_CONUS_gages2_climgrid.csv", sep = ""), stringsAsFactors = FALSE) 
#climgrid_SWE_mm <- read.csv(paste(path,"Data\\gridMET\\broxton_SWE_mm_all_CONUS_gages2.csv", sep = ""), stringsAsFactors = FALSE) 

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

setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\data")


# get drainage area from here to calculate flow in mm/d
setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\data\\daily_data")

#lowflowsites <- read.csv("gage_characteristics.csv", stringsAsFactors = FALSE )
#lowflowsites$site_id <- substr(lowflowsites$site_id, 3,nchar(lowflowsites$site_id))

library(EGRET)
library(ggplot2)

### Loop through stations, compile flow and climate data, calculate flow stats, plot flow vs climate data
###################################################################################

for(i in seq_along(usgs.files)){

  #i = 1
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\data")
  
  #Prep streamflow data
  
  current <- read.csv(usgs.files[i], stringsAsFactors = FALSE)
  current$Date <- as.Date(current$Date, format = "%Y-%m-%d")
  currentsite <- gsub(".csv","",usgs.files[i])
  currentsite_num <- as.numeric(currentsite)
  currentsite_string <- as.character(currentsite_num)
  currentsite_MET <- paste0("X",currentsite)
  
  #currentarea <- subset(lowflowsites, lowflowsites$site_id == currentsite)
  #currentarea <- currentarea$drainSqKm
  # get drainage area and then use this line
  #current$mmd <- 1000*(current$X_00060_00003*0.028316847*86400)/(currentarea*1000000)
  #UPDATE REQUIRED
  current$mmd <- current$X_00060_00003
  
  current$month <- month(current$Date)
  current$year <- year(current$Date)
  ### Assign climate year (APril of year until March of next year)
  ### Count how many non NA values there are in x_00060_00003 for that given climate year
  ### If there are more than 350 complete values for that climate year, add to complete years
  current$climyear <- ifelse(current$month<4, current$year-1,current$year)
  current$season <- ifelse((current$month>3 &current$month<10),paste0(current$climyear,"g"),paste0(current$climyear,"n"))
  currentcounts <- current %>% group_by(climyear) %>% summarise(count = sum(!is.na(X_00060_00003)))
  currentcounts <- subset(currentcounts, currentcounts$count>350)
  completeyears <- currentcounts$climyear
  current <- subset(current, current$climyear %in% completeyears)
  current$day7mean <- rollmean(current$mmd,7, align = "center", fill = NA)
  
  monthly <- current
  monthly$year_month <- paste0(monthly$year, "_", monthly$month)
  monthly <- monthly %>% group_by(year_month) %>% summarise(mean = mean(X_00060_00003, na.rm = TRUE))
  monthly$month <-substr(monthly$year_month,6,7)
  monthly$year <-substr(monthly$year_month,1,4)
  monthly$date <-paste0(monthly$year,"-",monthly$month,"-",15)
  monthly$date <- as.Date(monthly$date, format = "%Y-%m-%d")
  
  ##### Plot Daily Flow
  plottitle = paste0(currentsite, " Daily Flow")
  plotfilesave = paste0(currentsite, "_Daily.jpg")
  
  plotdaily <- ggplot(data=current, aes(x=Date, y=X_00060_00003)) + 
    geom_point(colour='Black', size=1) + 
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          text = element_text(color="black", size=12),
          plot.title = element_text(hjust = 0.5)) + 
    labs(title=plottitle,
         x="Date", y="Discharge (ft3/sec)") +
    scale_x_date(date_breaks = "10 years", 
                 labels = date_format("%Y"), 
                 limits = as.Date(c('1916-01-01','2020-01-01'))) +
    ylim(0,30000)
  
  plotdaily
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Plots\\DailyFlow\\")
  ggsave(plot=plotdaily, file=plotfilesave, dpi=600, width=6.6, height=3.75)
  
  
  ##### Plot 7 Day Mean Flow
  plottitle = paste0(currentsite, " Flow - 7 Day Mean")
  plotfilesave = paste0(currentsite, "_7DayMean.jpg")
  
  plot7daymean <- ggplot(data=current, aes(x=Date, y=day7mean)) + 
    geom_point(colour='Blue', size=1) + 
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          text = element_text(color="black", size=12),
          plot.title = element_text(hjust = 0.5)) + 
    labs(title=plottitle,
         x="Date", y="Discharge (ft3/sec)") +
    scale_x_date(date_breaks = "10 years", 
                 labels = date_format("%Y"), 
                 limits = as.Date(c('1916-01-01','2020-01-01'))) +
    ylim(0,20000)
  
  plot7daymean
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Plots\\7DayMean\\")
  ggsave(plot=plot7daymean, file=plotfilesave, dpi=600, width=6.6, height=3.75)
  
  ##### Plot Monthly Flow
  plottitle = paste0(currentsite, " Mean Monthly Flow")
  plotfilesave = paste0(currentsite, "_Monthly.jpg")
  
  plotdaily <- ggplot(data=monthly, aes(x=date, y=mean)) + 
    geom_point(colour='Black', size=2) + 
    geom_line() +
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          text = element_text(color="black", size=12),
          plot.title = element_text(hjust = 0.5)) + 
    labs(title=plottitle,
         x="Date", y="Discharge (ft3/sec)") +
    scale_x_date(date_breaks = "10 years", 
                 labels = date_format("%Y"), 
                 limits = as.Date(c('1916-01-01','2020-01-01'))) +
    ylim(0,8000)
  
  plotdaily
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Plots\\DailyFlow\\")
  ggsave(plot=plotdaily, file=plotfilesave, dpi=600, width=6.6, height=3.75)
  
  #Prep MET data
  current_P_mm <- gridmet_P_mm[,c("Date",currentsite_string)]
  current_P_mm$Date <- as.Date(current_P_mm$Date, origin = "1899-12-30")
  current_P_mm$month <- month(current_P_mm$Date)
  current_P_mm$year <- year(current_P_mm$Date)
  current_P_mm$climyear <- ifelse(current_P_mm$month<4, current_P_mm$year-1,current_P_mm$year)
  current_P_mm$season <- ifelse((current_P_mm$month>3 &current_P_mm$month<10),paste0(current_P_mm$climyear,"g"),paste0(current_P_mm$climyear,"n"))
  current_P_mm <- subset(current_P_mm, current_P_mm$climyear>1979 & current_P_mm$climyear<2019)
  current_P_mm <- rename(current_P_mm,"meanP" = currentsite_string)
  meanannual_P_mm <- current_P_mm %>% group_by(season) %>% summarise(meanP = mean(meanP, na.rm = TRUE))
  
  current_PDSI <- gridmet_PDSI[,c("Date",currentsite_string)]
  current_PDSI$Date <- as.Date(current_PDSI$Date, origin = "1899-12-30")
  current_PDSI$month <- month(current_PDSI$Date)
  current_PDSI$year <- year(current_PDSI$Date)
  current_PDSI$climyear <- ifelse(current_PDSI$month<4, current_PDSI$year-1,current_PDSI$year)
  current_PDSI$season <- ifelse((current_PDSI$month>3 &current_PDSI$month<10),paste0(current_PDSI$climyear,"g"),paste0(current_PDSI$climyear,"n"))
  current_PDSI <- subset(current_PDSI, current_PDSI$climyear>1979 & current_PDSI$climyear<2019)
  current_PDSI <- rename(current_PDSI,"meanPDSI" = currentsite_string)
  meanannual_PDSI <- current_PDSI %>% group_by(season) %>% summarise(meanPDSI = mean(meanPDSI, na.rm = TRUE))
  
  current_PET_mm <- gridmet_PET_mm[,c("Date",currentsite_string)]
  current_PET_mm$Date <- as.Date(current_PET_mm$Date, origin = "1899-12-30")
  current_PET_mm$month <- month(current_PET_mm$Date)
  current_PET_mm$year <- year(current_PET_mm$Date)
  current_PET_mm$climyear <- ifelse(current_PET_mm$month<4, current_PET_mm$year-1,current_PET_mm$year)
  current_PET_mm$season <- ifelse((current_PET_mm$month>3 &current_PET_mm$month<10),paste0(current_PET_mm$climyear,"g"),paste0(current_PET_mm$climyear,"n"))
  current_PET_mm <- subset(current_PET_mm, current_PET_mm$climyear>1979 & current_PET_mm$climyear<2019)
  current_PET_mm <- rename(current_PET_mm,"meanPET" = currentsite_string)
  meanannual_PET <- current_PET_mm %>% group_by(season) %>% summarise(meanPET = mean(meanPET, na.rm = TRUE))
  
  
  current_spei14 <- gridmet_spei14[,c("Date",currentsite_string)]
  current_spei14$Date <- as.Date(current_spei14$Date, origin = "1899-12-30")
  current_spei14$month <- month(current_spei14$Date)
  current_spei14$year <- year(current_spei14$Date)
  current_spei14$climyear <- ifelse(current_spei14$month<4, current_spei14$year-1,current_spei14$year)
  current_spei14$season <- ifelse((current_spei14$month>3 &current_spei14$month<10),paste0(current_spei14$climyear,"g"),paste0(current_spei14$climyear,"n"))
  current_spei14 <- subset(current_spei14, current_spei14$climyear>1979 & current_spei14$climyear<2019)
  current_spei14 <- rename(current_spei14,"meanSPEI" = currentsite_string)
  meanannual_spei14 <- current_spei14 %>% group_by(season) %>% summarise(meanSPEI = mean(meanSPEI, na.rm = TRUE))
  
  climData <- merge(meanannual_P_mm, meanannual_PDSI, by = "season")
  climData <- merge(climData, meanannual_PET, by = "season")
  climData <- merge(climData, meanannual_spei14, by= "season")

  # Metrics that have to do with different elements of the natural flow regime, focusing on low flows
  
  # Poff et al., 1997 - https://watermark.silverchair.com/47-11-769.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAAngwggJ0BgkqhkiG9w0BBwagggJlMIICYQIBADCCAloGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMbaTouRL_llBh2He7AgEQgIICK5_Z0oD5CUZbGwR_9ut8n4fje9rOY3yBU0lPY8S2EO7pfAi10y-glDNc78IrxIaDwYNBeE2UhAGb43PLJvCF9wsW7pHohsxCCd5Ch6DgLVXfWnOq72tLqyIju0Jv8wcqk7FhRbkZEnNOAH61ag3qeQMrS3j3Ry6lIyZ8nVb5RfLKHPq-DvdFzUDOl5ZFc77SKiRY8oJZb91U-okv_2FgmfnE0cSMhdwuPh7cARKrI0CFGXcKNxp2LEF9aTmT3GI3wVxZ_lgY0Q4K04bH_ihnD7EgadBBCfdR7XniAeA3uoBvxRjLkE_XHMs1nSrQOv6WbpzaPUaHB-ySMKaWit9XSGDGuMsMCG047XAxqy17xy26nONlDWyuYG8X7-lJzAvcApL_hX4VwSWc05KcUwNQfpPSbAVGDvVevl0r9XFdYQRUEBmc-wfv1NwQYfChIJJWoTyZoZJ-JUHBIWESaAShbQn8JYt2jQEKTib42BaBPbCWZMjwNOtigCvWlPqNi089slvoJAPMaj0riXGoYyD7nAEEwA0T7Oyung5SpPFl0Cy6mLjEh0f00goV3hH3QMAcYCT-qSnpLMasQGWpSUjPQ7SvEJPaj2duR7LEvBwXDAOoKOfAftPv4wFOyPcDW6QvOLBgwrhav9-MXG4EJB-1ojIdz1ZaKznnY2P-YNONb4p4frmfnpRAxXsMYhYNSw4BeYSlEEkSLXB0rrRZkmWFBGrMWAmr_z3ruOPtCA
  
  # magnitude = minimum 7 day flow, volume deficit
  # duration = # of days below 5% threshold
  # frequency = # of periods below 5% threshold 
  # timing = date of minimum 7 day flow, 
  # rate of change = recession constant of 90 days prior?
  
  current <- subset(current, current$climyear>1979 & current$climyear<2019)
  
  # mean average daily flow
  
  meanannualdailyflow <- current %>% group_by(season) %>% summarise(mean = mean(X_00060_00003, na.rm = TRUE))
  meanannualdailyflow <- meanannualdailyflow[,c("mean")]
  
  
  # annual minimum 7-day flow
  
  lowest7dayperclimyear <- current %>% group_by(season) %>% summarise(min = min(day7mean, na.rm = TRUE))
  
  # date of the lowest 7-day flow
  
  lowest7dayperclimyeardate <- current %>% group_by(season) %>% slice(which.min(day7mean))
  lowest7dayperclimyeardate$yday <- yday(lowest7dayperclimyeardate$Date)
  lowest7dayperclimyeardate$climday_min_7 <- ifelse(lowest7dayperclimyeardate$yday>90, lowest7dayperclimyeardate$yday-90, lowest7dayperclimyeardate$yday+275)
  lowest7dayperclimyeardate <- lowest7dayperclimyeardate[,c("climday_min_7")] 
  
  # the annual number of days below the 5-percent threshold
  
  per2thresh <- quantile(current$mmd, 0.02, na.rm = TRUE)
  per70thresh <- quantile(current$mmd, 0.70, na.rm = TRUE)
  
  current$lessthan0.02 <- ifelse(current$mmd < per2thresh,1,0)
  daysbelow0.02climyear <- current %>% group_by(season) %>% summarise(days_below_0.02_quant = sum(lessthan0.02, na.rm = TRUE))
  daysbelow0.02climyear <- daysbelow0.02climyear[,"days_below_0.02_quant"]
  # total deviation below 2% thresh---- calc column that is flow - 5% flow, then sum if negative
  
  current$below0.02quantile <- ifelse(current$mmd<per2thresh,per2thresh-current$mmd,0)
  totalbelow0.02climyear <- current %>% group_by(season) %>% summarise(deficit_mm = sum(below0.02quantile, na.rm = TRUE))
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
  
 try( alloutput <- cbind(meanannualdailyflow,lowest7dayperclimyear,lowest7dayperclimyeardate,daysbelow0.02climyear,totalbelow0.02climyear,fillwithannualvalues))
  
 try( alloutput$gage <- unique(current$site_no))
  
alloutput <- merge(alloutput, climData, by = "season")
  
  
 try( alloutput$climyear <- as.numeric(str_sub(alloutput$season,1,4)))



 try( alloutput$season <- str_sub(alloutput$season,5,6))
 alloutput$season <- ifelse(alloutput$season=="g","Growing Season","Non-Growing Season")


 #merge (join) annual climate stats to all output  
  
 
 
   try(write.csv(alloutput, usgs.files[i]))
  
  
  ######################################## PLOTS FOR SITE i #####################################
  
  ##### Plot Annual Mean Flow
  plottitle = paste0(currentsite, " Average Daily Flow by Climate Year")
  plotfilesave = paste0(currentsite, "_AvgDailyByYear.jpg")
  
  plotavgannual <- ggplot(data=alloutput, aes(x=climyear, y=mean, colour = season)) + 
                  geom_point(size=3) + 
                  scale_color_manual(values = c("Growing Season" = "green3", 
                                                "Non-Growing Season" = "purple")) +
                  geom_line() +
                  theme(panel.background = element_blank(), 
                        panel.grid = element_blank(),
                        panel.border = element_rect(fill  = NA),
                        text = element_text(color="black", size=12),
                        plot.title = element_text(hjust = 0.5),
                        legend.position = c(0.8,0.85)) + 
                  labs(title=plottitle,
                       x="Year", y="Avg Faily Flow by Climate Year (ft3/sec)") + 
                  xlim(1980,2020) +
                  ylim(0,1500)
  
  plotavgannual
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Plots\\AnnualAverages\\")
  ggsave(plot=plotavgannual, file=plotfilesave, dpi=600, width=6.6, height=3.75)
  
  
  ### Scatter Plot of Mean Precipitation vs Mean Flow
  plottitle = paste0(currentsite, " Precipitation vs Mean Flow")
  plotfilesave = paste0(currentsite, "_PrecipvsFlow.jpg")
  
  correlation = as.character(round(cor(alloutput$mean, alloutput$meanP),digits = 2))
  allGrowing <- subset(alloutput, season == "Growing Season")
  CorGrowing = as.character(round(cor(allGrowing$mean, allGrowing$meanP),digits = 2))
  allNonGrowing <- subset(alloutput, season == "Non-Growing Season")
  CorNonGrowing = as.character(round(cor(allNonGrowing$mean, allNonGrowing$meanP),digits = 2))

  
  plotprecipvsflow <- ggplot(data=alloutput, aes(x=mean, y=meanP, colour = season)) + 
                      geom_point(size=2) + 
                      geom_smooth(method="lm", se=FALSE)+
                      scale_color_manual(values = c("Growing Season" = "orange3", 
                                                    "Non-Growing Season" = "dodgerblue")) +
                      theme(panel.background = element_blank(), 
                            panel.grid = element_blank(),
                            panel.border = element_rect(fill = NA),
                            text = element_text(color="black", size=12),
                            plot.title = element_text(hjust = 0.5),
                            legend.position = c(0.8,0.2)) + 
                      labs(title=plottitle,
                           x="Mean Flow (ft3/sec)", y="Mean Precipitation (mm)") + 
                      xlim(0,1500) +
                      ylim(0,6) + 
                      annotate("text", x=350, y=6, label = paste0("Growing Season (Apr - Sep) r value = ",CorGrowing), colour = "orange3") +
                      annotate("text", x=350, y=5.75, label = paste0("        Non-Growing Season (Oct-Mar) r value = ",CorNonGrowing), colour = "dodgerblue")
  
  plotprecipvsflow
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Plots\\ScatterPlots\\")
  ggsave(plot=plotprecipvsflow, file=plotfilesave, dpi=600, width=6.6, height=3.75)
  

  ### Scatter Plot of Mean PET vs Mean Flow
  plottitle = paste0(currentsite, " PET vs Mean Flow")
  plotfilesave = paste0(currentsite, "_PETvsFlow.jpg")
  
  correlation = as.character(round(cor(alloutput$mean, alloutput$meanPET),digits = 2))
  allGrowing <- subset(alloutput, season == "Growing Season")
  CorGrowing = as.character(round(cor(allGrowing$mean, allGrowing$meanPET),digits = 2))
  allNonGrowing <- subset(alloutput, season == "Non-Growing Season")
  CorNonGrowing = as.character(round(cor(allNonGrowing$mean, allNonGrowing$meanPET),digits = 2))
  correlation
  
  #old colour sybology Growing season = "green3", Non-Growing SEason" = purple
  
  plotPETvsflow <- ggplot(data=alloutput, aes(x=mean, y=meanPET, colour = season)) + 
                    geom_point(size=2) + 
                    geom_smooth(method="lm", se=FALSE)+
                    scale_color_manual(values = c("Growing Season" = "orange3", 
                                                  "Non-Growing Season" = "dodgerblue")) +
                    theme(panel.background = element_blank(), 
                          panel.grid = element_blank(),
                          panel.border = element_rect(fill = NA),
                          text = element_text(color="black", size=12),
                          plot.title = element_text(hjust = 0.5),
                          legend.position = c(0.8,0.2)) + 
                    labs(title=plottitle,
                         x="Mean Flow (ft3/sec)", y="Mean PET (mm)") + 
                    xlim(0,1500) +
                    ylim(0,10) +
                    annotate("text", x=350, y=10, label = paste0("Growing Season (Apr - Sep) r value = ",CorGrowing), colour = "orange3") +
                    annotate("text", x=350, y=9.5, label = paste0("Non-Growing Season (Oct-Mar) r value = ",CorNonGrowing), colour = "dodgerblue")
  
  
  plotPETvsflow
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Plots\\ScatterPlots\\")
  ggsave(plot=plotPETvsflow, file=plotfilesave, dpi=600, width=6.6, height=3.75)
  
  ### Scatter Plot of Mean SPEI14 vs Mean Flow
  plottitle = paste0(currentsite, " SPEI14 vs Mean Flow")
  plotfilesave = paste0(currentsite, "_SPEI14vsFlow.jpg")
  
  correlation = as.character(round(cor(alloutput$mean, alloutput$meanSPEI),digits = 2))
  allGrowing <- subset(alloutput, season == "Growing Season")
  CorGrowing = as.character(round(cor(allGrowing$mean, allGrowing$meanSPEI),digits = 2))
  allNonGrowing <- subset(alloutput, season == "Non-Growing Season")
  CorNonGrowing = as.character(round(cor(allNonGrowing$mean, allNonGrowing$meanSPEI),digits = 2))
  correlation
  
  plotSPEI14vsflow <- ggplot(data=alloutput, aes(x=mean, y=meanSPEI, colour = season)) + 
                      geom_point(size=2) + 
                      geom_smooth(method="lm", se=FALSE)+
                      scale_color_manual(values = c("Growing Season" = "orange3", 
                                                    "Non-Growing Season" = "dodgerblue")) +
                      theme(panel.background = element_blank(), 
                            panel.grid = element_blank(),
                            panel.border = element_rect(fill = NA),
                            text = element_text(color="black", size=12),
                            plot.title = element_text(hjust = 0.5),
                            legend.position = c(0.8,0.2)) + 
                      labs(title=plottitle,
                           x="Mean Flow (ft3/sec)", y="Mean SPEI14") + 
                      xlim(0,1500) +
                      ylim(-1,1) +
                      annotate("text", x=350, y=1, label = paste0("Growing Season (Apr - Sep) r value = ",CorGrowing), colour = "orange3") +
                      annotate("text", x=350, y=0.95, label = paste0("Non-Growing Season (Oct-Mar) r value = ",CorNonGrowing), colour = "dodgerblue")
  
  plotSPEI14vsflow
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Plots\\ScatterPlots\\")
  ggsave(plot=plotSPEI14vsflow, file=plotfilesave, dpi=600, width=6.6, height=3.75)
  
  
  ### Scatter Plot of Mean PDSI vs Mean Flow
  plottitle = paste0(currentsite, " PDSI vs Mean Flow")
  plotfilesave = paste0(currentsite, "_PDSIvsFlow.jpg")
  
  correlation = as.character(round(cor(alloutput$mean, alloutput$meanPDSI),digits = 2))
  allGrowing <- subset(alloutput, season == "Growing Season")
  CorGrowing = as.character(round(cor(allGrowing$mean, allGrowing$meanPDSI),digits = 2))
  allNonGrowing <- subset(alloutput, season == "Non-Growing Season")
  CorNonGrowing = as.character(round(cor(allNonGrowing$mean, allNonGrowing$meanPDSI),digits = 2))
  correlation
  
  plotPDSIvsflow <- ggplot(data=alloutput, aes(x=mean, y=meanPDSI, colour = season)) + 
                    geom_point(size=2) + 
                    geom_smooth(method="lm", se=FALSE)+
                    scale_color_manual(values = c("Growing Season" = "orange3", 
                                                  "Non-Growing Season" = "dodgerblue")) +
                    theme(panel.background = element_blank(), 
                          panel.grid = element_blank(),
                          panel.border = element_rect(fill = NA),
                          text = element_text(color="black", size=12),
                          plot.title = element_text(hjust = 0.5),
                          legend.position = c(0.8,0.2)) + 
                    labs(title=plottitle,
                         x="Mean Flow (ft3/sec)", y="Mean PDSI")+ 
                    xlim(0,1500) +
                    ylim(-6,6) +
                    annotate("text", x=350, y=6, label = paste0("Growing Season (Apr - Sep) r value = ",CorGrowing), colour = "orange3") +
                    annotate("text", x=350, y=5.5, label = paste0("Non-Growing Season (Oct-Mar) r value = ",CorNonGrowing), colour = "dodgerblue")
  
                  
  plotPDSIvsflow
  
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Plots\\ScatterPlots\\")
  ggsave(plot=plotPDSIvsflow, file=plotfilesave, dpi=600, width=6.6, height=3.75)

  
  
  
  
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\data\\daily_data")
  
  
}



###########################################################################################
for(i in seq_along(usgs.files)){
  
  i = 1
  setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\data")
  
  #Prep streamflow data
  
  current <- read.csv(usgs.files[i], stringsAsFactors = FALSE)
  current$Date <- as.Date(current$Date, format = "%Y-%m-%d")
  currentsite <- gsub(".csv","",usgs.files[i])
  current$month <- month(current$Date)
  current$year <- year(current$Date)
  ### Assign climate year (APril of year until March of next year)
  ### Count how many non NA values there are in x_00060_00003 for that given climate year
  ### If there are more than 350 complete values for that climate year, add to complete years
  current$climyear <- ifelse(current$month<4, current$year-1,current$year)
  current$season <- ifelse((current$month>3 &current$month<10),paste0(current$climyear,"g"),paste0(current$climyear,"n"))
  currentcounts <- current %>% group_by(climyear) %>% summarise(count = sum(!is.na(X_00060_00003)))
  currentcounts <- subset(currentcounts, currentcounts$count>350)
  completeyears <- currentcounts$climyear
  current <- subset(current, current$climyear %in% completeyears)
  current <- subset(current, current$climyear>1979 & current$climyear<2019)
  
  #GET LIST OF YEARS
  years <- unique(current$climyear)
  
  for(y in seq_along(flow.years)){
    
    #y=1
    currentyear <- subset(current,current$climyear == years[y])
    year<- years[y]
    year
    
    #source for FDC code: https://rpubs.com/cassiorampinelli/528388
    #Loading discharges removing NA values
    flow<-na.omit(currentyear$X_00060_00003)

    #Sorting discharges in decreasing order
    flow<-sort(flow,decreasing=T)
    
    #Creating a data frame in which x column is hte percent of ie less than a specific time and
    #y is the correspondent discharge.
    df<-data.frame(x=100/length(flow)*1:length(flow),y=flow)
    
    #Plot
    plot(x = df$x, y = df$y, type = "l", log = "y",ylab="Discharge (cfs)",xlab="Percentage of Time Flow is Equaled or Less Than (%)",main="Flow Duration Curve")
    grid()
    
    #Plot Flow Duration Curve
    plottitle = paste0(currentsite, " Flow Duration Curve (Climate Year ", year,")")
    plotfilesave = paste0(currentsite, "_FDC_",year,".jpg")
    ylim = c(1,10001)
    
    plot <- ggplot(data=df, aes(x=x, y=y)) + 
      geom_point(colour='Black', size=1) + 
      theme(panel.background = element_blank(), 
            panel.grid = element_blank(),
            panel.border = element_rect(fill = NA),
            text = element_text(color="black", size=12),
            plot.title = element_text(hjust = 0.5)) + 
      labs(title=plottitle,
           x="Percentage of Time Flow is Equaled or Less Than (%)", 
           y="Discharge (cfs)") +
      scale_y_continuous(trans ='log10', 
                         limits = ylim)
    
    plot
    setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Plots\\FDC\\ByYear\\")
    ggsave(plot=plot, filename=plotfilesave, dpi=600, width=6.6, height=3.75)
  
  
  
  } #close for(i in seq_along(flow.years))
  

} #close for(i in seq_along(usgs.files))

setwd("C:\\School\\R\\Projects\\RManning_Masters_Project\\Data\\daily_data")









