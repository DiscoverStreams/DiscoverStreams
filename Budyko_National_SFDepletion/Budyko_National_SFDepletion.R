###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

projectPath <- "C:\\School\\R\\Projects\\Budyko_National_SFDepletion\\Budyko_National_SFDepletion_AllStations\\"
setwd(projectPath)

######### Get data ################################################################
### Loops through a list of sites and writes a csv file for each station with daily streamflow values


library(lubridate)
library(dataRetrieval)
library(data.table)
library(plyr)
library(dplyr)


setwd(paste0(projectPath,"Data"))


Sites <- read.csv("Basin_IDs.csv", stringsAsFactors = FALSE )
SiteIds <- substr(Sites$site_id, 3,nchar(Sites$site_id))

setwd(paste0(projectPath,"Data\\Daily_Data"))

### Loop through Sites and write a csv file for each station with parameter 00060 
### Parameter "00060" = Discharge, cubic feet per second
### The csv files will be written 

#for(i in seq_along(SiteIds)){
 #i = 1
#  try(current <- readNWISdv(siteNumber = SiteIds[i], parameterCd = "00060"))
#  try(current$asdate <- as.Date(current$Date))
#  try(write.csv(current,file=paste0(SiteIds[i],".csv"))) 
#}


### usgs.files is a list of all the files saved in daily_data, with one file for each site
usgs.files <- list.files(pattern = ".csv")

### Read in meteorological data
setwd(paste0(projectPath,"Data\\Climate"))

### CODE UPDATE: Consider writing these to csv files (for each individual station??)  
###               so such a large dataframe isn't being carried around
gridmet_P_mm <- read.csv("ClimGrid_MWBM_P_CA_MI_KS_072722.csv", stringsAsFactors = FALSE, check.names = FALSE, colClasses = "numeric") 
gridmet_PET_mm <- read.csv("ClimGrid_MWBM_PET_CA_MI_KS_072722.csv", stringsAsFactors = FALSE, check.names = FALSE, colClasses = "numeric") 

###################################################################################


library(ggplot2)
library(scales)
library(lubridate)
library(zoo)
library(EcoHydRology)
library(foreign)
library(tidyverse)
library(EGRET)


### Loop through stations, compile flow and climate data in units of mm/d 
### Build matrix with average values for years as specified in user input
###################################################################################


### USER INPUT ###
StartYear = 1960
EndYear = 2020

#initialize matrix for average values
BudykoMatrix <- matrix(data = NA, nrow = 0, ncol = 9)
colnames(BudykoMatrix) <- c("siteID", "interval", "group","state",  "meanSF", "meanP", "meanPET", "PET.P", "AET.P")

try(write.csv(BudykoMatrix, file="BudykoParameters.csv"))
BudykoDF <- read.csv("BudykoParameters.csv")

# get drainage area and group designations from here
setwd(paste0(projectPath,"Data"))
gagedetails <- read.csv("gage_characteristics.csv", stringsAsFactors = FALSE, colClasses=c("site_id"="character") )



for(i in seq_along(SiteIds)){
  
  #i = 55
  setwd(paste0(projectPath,"Data//Daily_Data"))
  
  #Prep streamflow data
  
  #read in streamflow data and subsect for the specified years
  current <- read.csv(paste0(SiteIds[i],".csv"), stringsAsFactors = FALSE)
  currentsite <- (SiteIds[i])
  currentsite_string <- as.character(as.numeric(currentsite))  
  
  current$Date <- as.Date(current$Date, format = "%Y-%m-%d")
  current$year <- year(current$Date)
  current <- subset(current, (current$year >= StartYear) & (current$year <= EndYear))
  
  # get drainage area and then convert flow to mmd
  currentarea <- subset(gagedetails, gagedetails$site_id == currentsite)
  currentarea <- currentarea$drainSqKm
  current$mmd <- 1000*(current$X_00060_00003*0.028316847*86400)/(currentarea*1000000)
  currentsite
  
  #get group designation from gagedetails
  currentgroup <- subset(gagedetails, gagedetails$site_id == currentsite)
  currentgroup <- currentgroup$Group
  
  currentState <- subset(gagedetails, gagedetails$site_id == currentsite)
  currentstate <- currentState$Area
  
  
  ### Count how many non NA values there are in x_00060_00003 for that given year
  ### If there are more than 350 complete values for that climate year, add to complete years
  currentcounts <- current %>% group_by(year) %>% summarise(count = sum(!is.na(X_00060_00003)))
  currentcounts <- subset(currentcounts, currentcounts$count>350)
  completeyears <- currentcounts$year 
  
  #calculate the average SF for each station
  meanSF <- mean(current$mmd)
  
  currentsite_string
  #Prep MET data
  current_P <- gridmet_P_mm[,c("Date",currentsite_string)]
  current_P$Date <- as.Date(current_P$Date, origin = "1899-12-30")
  current_P$year <- year(current_P$Date)
  #current_P$interval <- round_any(current_P$year, IntervalLen, f=floor)
  current_P <- subset(current_P, (current_P$year>=StartYear) & (current_P$year<=EndYear))
  current_P <- rename(current_P, "Precip" = currentsite_string)
  meanP <- mean(current_P$Precip)
  meanP <- meanP / 30.437
  
  current_PET <- gridmet_PET_mm[,c("Date",currentsite_string)]
  current_PET$Date <- as.Date(current_PET$Date, origin = "1899-12-30")
  current_PET$year <- year(current_PET$Date)
  #current_PET$interval <- round_any(current_PET$year, IntervalLen, f=floor)
  current_PET <- subset(current_PET, (current_PET$year>=StartYear) & (current_PET$year<=EndYear))
  current_PET <- rename(current_PET,"PET" = currentsite_string)
  meanPET <- mean(current_PET$PET)
  meanPET <- meanPET / 30.437
 
  PET.P = meanPET / meanP
  AET.P = (1 - meanSF / meanP)
  
  
  colnames(BudykoMatrix) <- c("siteID", "interval", "group", "state", "meanSF", "meanP", "meanPET", "PET.P", "AET.P")
  
  
  #join arrays based on interval group
  currentBudyko <- data.frame(matrix(ncol=9,nrow=0))
  colnames(currentBudyko) <-c("siteID", "interval", "group", "state", "meanSF", "meanP", "meanPET", "PET.P", "AET.P")
  currentBudyko[1,] <- c(currentsite, "NA" , currentgroup, currentstate, meanSF, meanP ,meanPET,  PET.P, AET.P)

  BudykoDF <- rbind(BudykoDF, currentBudyko )
  
}

setwd(paste0(projectPath,"Data"))



try(write.csv(BudykoDF, file="BudykoParameters_avg_1960_2020.csv"))
BudykoDF$PET.P <- as.numeric(BudykoDF$PET.P)
BudykoDF$AET.P <- as.numeric(BudykoDF$AET.P)

BudykoDF$Legend <- paste0(BudykoDF$state, " ",BudykoDF$group)



#str(BudykoDF)


BudykoDF <- subset(BudykoDF, (BudykoDF$group == "Reference" |BudykoDF$group ==  "Non Reference"))
BudykoReference <- subset(BudykoDF, (BudykoDF$group == "Reference"))
BudykoImpacted <- subset(BudykoDF, (BudykoDF$group == "Non Reference"))
BudykoCA <- subset(BudykoDF, (BudykoDF$state == "CA"))
BudykoKS <- subset(BudykoDF, (BudykoDF$state == "KS"))
BudykoMI <- subset(BudykoDF, (BudykoDF$state == "MI"))

BudykoCARef <- subset(BudykoCA, (BudykoCA$group == "Reference" | BudykoCA$AET.P >0))


BudykoCANR <- subset(BudykoCA, (BudykoCA$group == "Non Reference"))
BudykoKSRef <- subset(BudykoKS, (BudykoKS$group == "Reference"))
BudykoKSNR <- subset(BudykoKS, (BudykoKS$group == "Non Reference"))
BudykoMIRef <- subset(BudykoMI, (BudykoMI$group == "Reference"))
BudykoMINR <- subset(BudykoMI, (BudykoMI$group == "Non Reference"))






#install.packages("devtools")
#devtools::install_github("tylerbhampton/budykoR")

library(ggplot2)
library(budyko)

#default budyko-type curve using default settings of budyko_sim()
ogbudyko=budyko_sim()

#custom fit the data
fit1=budyko_fit(data=BudykoDF,method="Fu",dif="nls",silent = TRUE)
fit2=budyko_fit(data=BudykoReference, method="Fu", dif="nls", silent=TRUE)
fit3=budyko_fit(data=BudykoImpacted, method="Fu", dif="nls", silent=TRUE)
fitCA=budyko_fit(data=BudykoCA, method="Fu", dif="nls", silent=TRUE)
fitKS=budyko_fit(data=BudykoKS, method="Fu", dif="nls", silent=TRUE)
fitMI=budyko_fit(data=BudykoMI, method="Fu", dif="nls", silent=TRUE)

fitMIRef=budyko_fit(data=BudykoMIRef, method="Fu", dif="nls", silent=TRUE)
fitMINR=budyko_fit(data=BudykoMINR, method="Fu", dif="nls", silent=TRUE)
fitKSRef=budyko_fit(data=BudykoKSRef, method="Fu", dif="nls", silent=TRUE)
fitKSNR=budyko_fit(data=BudykoKSNR, method="Fu", dif="nls", silent=TRUE)
fitCARef=budyko_fit(data=BudykoCARef, method="Fu", dif="nls", silent=TRUE)
fitCANR=budyko_fit(data=BudykoCANR, method="Fu", dif="nls", silent=TRUE)




fit1
fit2
fit3
fitCA
fitCARef
fitCANR
fitKS
fitMI
fitKSRef


sim1=budyko_sim(fit=fit1, method="Fu")
sim2=budyko_sim(fit=fit2, method="Fu")
sim3=budyko_sim(fit=fit3, method="Fu")

simCA=budyko_sim(fit=fitCA, method="Fu")
simKS=budyko_sim(fit=fitKS, method="Fu")
simMI=budyko_sim(fit=fitMI, method="Fu")

simCARef=budyko_sim(fit=fitCARef, method="Fu")
simCANR=budyko_sim(fit=fitCANR, method="Fu")

simKSRef=budyko_sim(fit=fitKSRef, method="Fu")
simKSNR=budyko_sim(fit=fitKSNR, method="Fu")
simMIRef=budyko_sim(fit=fitMIRef, method="Fu")
simMINR=budyko_sim(fit=fitMINR, method="Fu")



setwd(paste0(projectPath,"Plots"))

#Plot by group (Reference vs Non Reference)
blankBC+
  geom_line(data=sim1, col=1)+
  geom_line(data=sim2, col=2)+
  geom_line(data=sim3, col=5)+
  geom_point(data=BudykoDF, aes(colour = group))+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_rect(fill  = NA),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8,0.2)) + 
  coord_cartesian(xlim=c(0,3), ylim=c(0,1))

#Plot by study area
#Plot used as figure in report
plot <- blankBC+
  geom_line(data=simCA, col="orangered")+
  geom_line(data=simKS, col="darkgreen")+
  geom_line(data=simMI, col="purple4")+
  geom_point(data=BudykoDF, aes(colour = Legend, shape = Legend), size=1.5)+
  scale_colour_manual(values=c("orangered", "orangered", "darkgreen", "darkgreen", "purple4", "purple4"))+
  scale_shape_manual(values=c(15, 0, 16, 1, 17, 2))+
   theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_rect(fill  = NA),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7,0.3)) + 
  coord_cartesian(xlim=c(0,2), ylim=c(0,1))+
 labs(title="1960 - 2020 Average Values by State")

plot
ggsave(plot=plot, file="ByState_AllGages_1960_2020_Avg.jpg", dpi=600, width=4, height=4)


#Plot CA
plot <- blankBC+
  geom_line(data=simCANR, col=2)+
  geom_line(data=simCARef, col=5)+
  geom_point(data=BudykoCA, aes(colour = group))+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_rect(fill  = NA),
        plot.title = element_text(hjust = 0.5, size=12),
        legend.position = c(0.8,0.2)) + 
  coord_cartesian(xlim=c(0,2), ylim=c(0,1))+
  labs(title="Klamath River Watershed, California")

plot
ggsave(plot=plot, file="CA_AllGages_1960_2020_Avg.jpg", dpi=600, width=4, height=4)


#Plot KS
plot <- blankBC+
  geom_line(data=simKSRef, col=5)+
  geom_line(data=simKSNR, col=2)+
  geom_point(data=BudykoKS, aes(colour = group))+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_rect(fill  = NA),
        plot.title = element_text(hjust = 0.5, size=12),
        legend.position = c(0.8,0.2)) + 
  coord_cartesian(xlim=c(0,2), ylim=c(0,1))+
  labs(title="Middle Arkansas River Watershed, Kansas")

plot
ggsave(plot=plot, file="KS_AllGages_1960_2020_Avg.jpg", dpi=600, width=4, height=4)



#Plot MI
plot <- blankBC+
  geom_line(data=simMIRef,colour=5)+
  geom_line(data=simMINR, colour=2)+
  geom_point(data=BudykoMI,aes(colour = group), size=1)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_rect(fill  = NA),
        plot.title = element_text(hjust = 0.5, size=12),
        legend.position = c(0.8,0.2)) + 
  coord_cartesian(xlim=c(0,2), ylim=c(0,1))+
  labs(title="Southeast Lake Michigan Watershed, Michigan")

  plot
ggsave(plot=plot, file="MI_AllGages_1960_2020_Avg.jpg", dpi=600, width=4, height=4)








plot <- blankBC+
  geom_line(data=sim1)+
  geom_point(data=fluxes, aes(colour = interval), size=1)+
  scale_color_gradient(low="dark blue", high="orange")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_rect(fill  = NA),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8,0.3)) + 
  coord_cartesian(xlim=c(0,3), ylim=c(0,1))+
  labs(title=paste0(currentsite," (",currentgroup,", ",projectarea,")"))+ 
  annotate("text", x=2.5, y=0.7, label = paste0("mae = ",mae)) +
  annotate("text", x=2.5, y=0.65, label = paste0("rsq = ",rsq)) 



