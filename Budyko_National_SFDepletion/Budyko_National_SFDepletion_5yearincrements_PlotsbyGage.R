###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

projectPath <- "C:\\Users\\rebkm\\OneDrive\\Documents\\GitHub\\DiscoveRStreams\\Budyko_National_SFDepletion\\"
setwd(projectPath)

######### Get data ################################################################
### Loops through a list of sites and writes a csv file for each station with daily streamflow values


library(lubridate)
library(dataRetrieval)
library(data.table)
library(plyr)
library(dplyr)


setwd(paste0(projectPath,"Data"))

Sites <- read.csv("Basin_IDs_longterm.csv", stringsAsFactors = FALSE )
SiteIds <- substr(Sites$site_id, 3,nchar(Sites$site_id))

#setwd(paste0(projectPath,"Data\\Daily_Data"))

### Loop through Sites and write a csv file for each station with parameter 00060 
### Parameter "00060" = Discharge, cubic feet per second
### The csv files will be written 

#for(i in seq_along(SiteIds)){
# i = 1
#  try(current <- readNWISdv(siteNumber = SiteIds[i], parameterCd = "00060"))
#  try(current$asdate <- as.Date(current$Date))
#  try(write.csv(current,file=paste0(SiteIds[i],".csv"))) 
#}


### usgs.files is a list of all the files saved in daily_data, with one file for each site
#usgs.files <- list.files(pattern = ".csv")

### Read in meteorological data
setwd(paste0(projectPath,"Data\\Climate"))

### CODE UPDATE: Consider writing these to csv files (for each individual station??)  
###               so such a large dataframe isn't being carried around
gridmet_P_mm <- read.csv("ClimGrid_MWBM_prcp.csv", stringsAsFactors = FALSE, check.names = FALSE) 
gridmet_PET_mm <- read.csv("ClimGrid_MWBM_PET.csv", stringsAsFactors = FALSE, check.names = FALSE) 

###################################################################################

library(ggplot2)
library(scales)
library(lubridate)
library(zoo)
library(EcoHydRology)
library(foreign)
library(tidyverse)
library(EGRET)
library(budyko)

### Loop through stations, compile flow and climate data in units of mm/d 
### Build matrix with average values for years as specified in user input
###################################################################################


### USER INPUT ###
StartYear = 1900
EndYear = 2020
IntervalLen = 5

years = seq(from=StartYear, to = EndYear - IntervalLen, by=IntervalLen)

#initialize matrix for average values
BudykoMatrix <- matrix(data = NA, nrow = 0, ncol = 8)
colnames(BudykoMatrix) <- c("siteID", "interval", "group", "meanSF", "meanP", "meanPET", "PET.P", "AET.P")

try(write.csv(BudykoMatrix, file="BudykoParameters.csv"))
BudykoDF <- read.csv("BudykoParameters.csv")

#BudykoMatrix <- matrix(data = NA, nrow = 0, ncol = 4)
#colnames(BudykoMatrix) <- c("SiteID", "PET.P", "AET.P", "group")

# get drainage area and group designations from here
setwd(paste0(projectPath,"Data"))
gagedetails <- read.csv("gage_characteristics.csv", stringsAsFactors = FALSE, colClasses=c("site_id"="character") )



for(i in seq_along(SiteIds)){
  
  i = 1
  setwd(paste0(projectPath,"Data//Daily_Data"))
  
  #Prep streamflow data
  
  #read in streamflow data and subsect for the specified years
  current <- read.csv(paste0(SiteIds[i],".csv"), stringsAsFactors = FALSE)
  currentsite <- (SiteIds[i])
  currentsite_string <- substr(currentsite,2,8)
  
  current$Date <- as.Date(current$Date, format = "%Y-%m-%d")
  current$year <- year(current$Date)
  current <- subset(current, (current$year >= StartYear) & (current$year <= EndYear))
  
  # get drainage area and then convert flow to mmd
  currentarea <- subset(gagedetails, gagedetails$site_id == currentsite)
  currentarea <- currentarea$drainSqKm
  current$mmd <- 1000*(current$X_00060_00003*0.028316847*86400)/(currentarea*1000000)
  
  #get group designation from gagedetails
  currentgroup <- subset(gagedetails, gagedetails$site_id == currentsite)
  currentgroup = currentgroup$Group
  
  ### Count how many non NA values there are in x_00060_00003 for that given year
  ### If there are more than 350 complete values for that climate year, add to complete years
  currentcounts <- current %>% group_by(year) %>% summarise(count = sum(!is.na(X_00060_00003)))
  currentcounts <- subset(currentcounts, currentcounts$count>350)
  completeyears <- currentcounts$year 
  
  #Add a column to current with the interval group and then calculate the average SF for each interval group
  current$interval <- round_any(current$year, 5, f=floor)
  meanSF <- current %>% group_by(interval) %>% summarise(meanSF = mean(mmd))
  
  
  #Prep MET data
  current_P <- gridmet_P_mm[,c("Date",currentsite_string)]
  current_P$Date <- as.Date(current_P$Date, origin = "1899-12-30")
  current_P$year <- year(current_P$Date)
  current_P$interval <- round_any(current_P$year, 5, f=floor)
  current_P <- subset(current_P, (current_P$year>=StartYear) & (current_P$year<=EndYear))
  current_P <- rename(current_P, "Precip" = currentsite_string)
  meanP <- current_P %>% group_by(interval) %>% summarise(meanP = mean(Precip, na.rm = TRUE))
  meanP$meanP <- meanP$meanP / 30.437
  
  current_PET <- gridmet_PET_mm[,c("Date",currentsite_string)]
  current_PET$Date <- as.Date(current_PET$Date, origin = "1899-12-30")
  current_PET$year <- year(current_PET$Date)
  current_PET$interval <- round_any(current_PET$year, 5, f=floor)
  current_PET <- subset(current_PET, (current_PET$year>=StartYear) & (current_PET$year<=EndYear))
  current_PET <- rename(current_PET,"PET" = currentsite_string)
  meanPET <- current_PET %>% group_by(interval) %>% summarise(meanPET = mean(PET, na.rm = TRUE))
  meanPET$meanPET <- meanPET$meanPET / 30.437
  
  #join arrays based on interval group
  fluxes <- merge(meanP, meanPET, by = "interval")
  fluxes <- merge(fluxes, meanSF, by = "interval")
  fluxes$siteID <- currentsite
  fluxes$group <- currentgroup
  
  fluxes$PET.P = fluxes$meanPET / fluxes$meanP
  fluxes$AET.P = (1 - fluxes$meanSF / fluxes$meanP)
  
  #Budyko curve plot for the current gage with data point for each increment of years
  
  #custom fit the data
  fit1=budyko_fit(data=fluxes,method="Fu",dif="mae",silent = TRUE)
  
  #fit1
  
  sim1=budyko_sim(fit=fit1, method="Fu")
  
  blankBC+
    geom_line(data=sim1)+
    geom_point(data=fluxes, aes(colour = group))+
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          panel.border = element_rect(fill  = NA),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.8,0.2)) + 
    coord_cartesian(xlim=c(0,3), ylim=c(0.4,1))
  
  
  
  #head(fluxes)
  #head(BudykoDF)
  
  #BudykoDF <- rbind(BudykoDF, fluxes )
  
}

setwd(paste0(projectPath,"Data"))

try(write.csv(BudykoDF, file="BudykoParameters.csv"))

BudykoDF <- subset(BudykoDF, (BudykoDF$group == "Reference" |BudykoDF$group ==  "Impacted"))
BudykoReference <- subset(BudykoDF, (BudykoDF$group == "Reference"))
BudykoImpacted <- subset(BudykoDF, (BudykoDF$group == "Impacted"))

#install.packages("devtools")
#devtools::install_github("tylerbhampton/budykoR")

library(ggplot2)
library(budyko)

#default budyko-type curve using default settings of budyko_sim()
ogbudyko=budyko_sim()

#custom fit the data
fit1=budyko_fit(data=BudykoDF,method="Fu",dif="mae",silent = TRUE)
fit2=budyko_fit(data=BudykoReference, method="Fu", dif="mae", silent=TRUE)
fit3=budyko_fit(data=BudykoImpacted, method="Fu", dif="mae", silent=TRUE)

#fit1
#fit2
#fit3

sim1=budyko_sim(fit=fit1, method="Fu")
sim2=budyko_sim(fit=fit2, method="Fu")
sim3=budyko_sim(fit=fit3, method="Fu")

blankBC+
  geom_line(data=sim1, col=1)+
  geom_line(data=sim2, col=5)+
  geom_line(data=sim3, col=2)+
  geom_point(data=BudykoDF, aes(colour = group))+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_rect(fill  = NA),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8,0.2)) + 
  coord_cartesian(xlim=c(0,3), ylim=c(0.4,1))





