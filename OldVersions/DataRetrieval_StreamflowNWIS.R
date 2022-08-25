library(readr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(DataExplorer)
library(dplyr)
library(jsonlite)
library(jsonify)
library(dataRetrieval)


setwd("~/GradSchool/DiscoverStreams")
wname <- getwd() # set back to working directory
dname <- paste(wname,"data",sep="/") # will open data file in workikng directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in workikng directory
pname<-paste(wname,"plots",sep="/") # will open plots file in workikng directory



### STREAMFLOW (NWIS) - MICHIGAN###
site_number <- c("04097540", "04096900", "04101500")
site_info <- readNWISsite(site_number)

### Prairie River near Nottawa ###
site_number <- c("04097540")
# view(Cheney_info)
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- ""
end_date <- ""

raw_daily_Prairie1 <- readNWISdv(site_number, parameter_code, start_date, end_date)

raw_daily_Prairie1 <- drop_columns(raw_daily_Prairie1, c("agency_cd", "X_00060_00003_cd"))

colnames(raw_daily_Prairie1) <- c("SiteNum", "Date", "Discharge_cfs")

### Nottawa Creek - feeds Prairie River ###
site_number <- c("04096900")

raw_daily_Prairie2 <- readNWISdv(site_number, parameter_code, start_date, end_date)

raw_daily_Prairie2 <- drop_columns(raw_daily_Prairie2, c("agency_cd", "X_00060_00003_cd"))

colnames(raw_daily_Prairie2) <- c("SiteNum", "Date", "Discharge_cfs")

### St. Joseph River ###
site_number <- c("04101500")
raw_daily_StJoseph <- readNWISdv(site_number, parameter_code, start_date, end_date)

raw_daily_StJoseph <- drop_columns(raw_daily_StJoseph, c("agency_cd", "X_00060_00003_cd"))

colnames(raw_daily_StJoseph) <- c("SiteNum", "Date", "Discharge_cfs")

### DataExplorer - Plot Streamflow ###
# DataExplorer::create_report(raw_daily_USGS)

# x <- raw_daily_USGS$Date
# y <- raw_daily_USGS$Discharge_cfs
# z <- raw_daily_USGS$SiteNum


plot(raw_daily_StJoseph$Date, raw_daily_StJoseph$Discharge_cfs,
     type = "l", 
     main = "Michigan", 
     xlab = "Date", 
     ylab = "Discharge (Cfs)",
     col = "black")
lines(raw_daily_Prairie2$Date, raw_daily_Prairie2$Discharge_cfs,
      type = "l",
      col = "darkorchid")
lines(raw_daily_Prairie1$Date, raw_daily_Prairie1$Discharge_cfs, 
      type = "l",
      col = "deepskyblue")
legend("topleft", 
       legend = site_info$station_nm, 
       lty = 1:1, 
       col = c("darkorchid", "deepskyblue", "black"), 
       title = "Stations",
       cex = 0.6)

# smoothScatter(raw_daily_USGS$Date, raw_daily_USGS$Discharge_cfs, main = site_info$station_nm, xlab = "Date", ylab = "Discharge (Cfs)")



### STREAMFLOW (NWIS) - CALIFORNIA ###

site_number <- c("04097540", "04096900")   
site_info <- readNWISsite(site_number)
# view(Cheney_info)
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- ""
end_date <- ""

raw_daily_USGS <- readNWISdv(site_number, parameter_code, start_date, end_date)

raw_daily_USGS <- drop_columns(raw_daily_USGS, c("agency_cd", "X_00060_00003_cd"))

colnames(raw_daily_USGS) <- c("SiteNum", "Date", "Discharge_cfs")

view(raw_daily_USGS,)

### DataExplorer - Plot Streamflow ###
# DataExplorer::create_report(raw_daily_USGS)

x <- raw_daily_USGS$Date
y <- raw_daily_USGS$Discharge_cfs
z <- raw_daily_USGS$SiteNum

plot(x, y, 
     type = "l", 
     main = "Michigan", 
     xlab = "Date", 
     ylab = "Discharge (Cfs)",
     col = ifelse(raw_daily_USGS$SiteNum == "04096900","darkorchid", 
                  ifelse(raw_daily_USGS$SiteNum == "04097540","deepskyblue", 
                         "grey")))
legend("topleft", legend = site_info$station_nm, lty = 1:1, col = c("darkorchid", "deepskyblue"), title = "Stations", cex = 0.6)

smoothScatter(raw_daily_USGS$Date, raw_daily_USGS$Discharge_cfs, main = site_info$station_nm, xlab = "Date", ylab = "Discharge (Cfs)")





########
# qColNames = c("Year", "Month", "Discharge")
# 
# q_select <- streamflow_monthly_syracuse[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_syracuse <- data_join
# write.csv(q_syracuse, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/Syracuse_Qmo.csv", row.names = FALSE, na = "null")
# 
# q_select <- streamflow_monthly_gardencity[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_gardencity <- data_join
# write.csv(q_gardencity, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/GardenCity_Qmo.csv", row.names = FALSE, na = "null")
# 
# q_select <- streamflow_monthly_dodgecity[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_dodgecity <- data_join
# write.csv(q_dodgecity, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/DodgeCity_Qmo.csv", row.names = FALSE, na = "null")
# 
# q_select <- streamflow_monthly_greatbend[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_greatbend <- data_join
# write.csv(q_greatbend, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/GreatBend_Qmo.csv", row.names = FALSE, na = "null")
# 
# q_select <- streamflow_monthly_wichita[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_wichita <- data_join
# write.csv(q_wichita, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/Wichita_Qmo.csv", row.names = FALSE, na = "null")
