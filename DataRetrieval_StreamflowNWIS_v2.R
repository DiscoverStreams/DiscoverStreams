library(readr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(DataExplorer)
library(dplyr)
library(jsonlite)

library(dataRetrieval)


setwd("~/GradSchool/DiscoverStreams")
wname <- getwd() # set back to working directory
dname <- paste(wname,"data",sep="/") # will open data file in workikng directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in workikng directory
pname<-paste(wname,"plots",sep="/") # will open plots file in workikng directory



### STREAMFLOW (NWIS) - MICHIGAN ###
site_number <- c("04097540", "04096900", "04101500")
site_info <- readNWISsite(site_number)
color_mi <- c("darkorchid", "black", "deepskyblue")

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


plot(raw_daily_USGS$Date, raw_daily_USGS$Discharge_cfs,
     type = "l", 
     # main = "Michigan", 
     xlab = "Date", 
     ylab = "Discharge (Cfs)",
     # log = "x,y",
     col = color_sv[4])
# lines(raw_daily_Prairie2$Date, raw_daily_Prairie2$Discharge_cfs,
#       type = "l",
#       col = "darkorchid")
# lines(raw_daily_Prairie1$Date, raw_daily_Prairie1$Discharge_cfs, 
#       type = "l",
#       col = "deepskyblue")
legend("topleft", 
       legend = site_info$station_nm, 
       lty = 1:1, 
       col = color_sv[4], 
       # col = c("darkorchid", "deepskyblue", "black"), 
       title = "Station",
       cex = 0.6)





### STREAMFLOW (NWIS) - SCOTT VALLEY, CALIFORNIA ###
site_number <- c("11519500", "11519000", "11518200", "11518050")
site_info <- readNWISsite(site_number)
color_sv <- c("aquamarine3", "darkblue", "darkred", "burlywood3")

### WATER USE (NWIS) ###
site_number <- "11519500"
state <- site_info$state_cd
county <- site_info$county_cd


waterUse_SV <- readNWISuse(
        stateCd = state,
        countyCd = county,
        years = "ALL",
        categories = "ALL",
        convertType = TRUE,
        transform = FALSE
)

raw_daily_StJoseph <- drop_columns(raw_daily_StJoseph, c("agency_cd", "X_00060_00003_cd"))

colnames(raw_daily_StJoseph) <- c("SiteNum", "Date", "Discharge_cfs")

### STREAMFLOW (NWIS) - KAWEAH, CALIFORNIA ###
site_number <- c("11209500", "11208600", "11208000", "11208730", "11208615", "11206820")
site_info <- readNWISsite(site_number)
color_kv <- c("darkorchid", "black", "deepskyblue", "chartreuse4", "coral2")

### Kaweah R at Kaweah ###
site_number <- c("11518050")   
site_name <- site_info$station_nm

raw_daily_USGS <- readNWISdv(site_number, parameter_code, start_date, end_date)

raw_daily_USGS <- drop_columns(raw_daily_USGS, c("agency_cd", "X_00060_00003_cd"))

colnames(raw_daily_USGS) <- c("SiteNum", "Date", "Discharge_cfs")

daily_streamflow_CA <- raw_daily_USGS
# view(daily_streamflow_CA)


### Kaweah R Below Conduit 2 Near Hammond ###
site_number <- c("11208600")
site_name <- site_info$station_nm

raw_daily_USGS <- readNWISdv(site_number, parameter_code, start_date, end_date)

raw_daily_USGS <- drop_columns(raw_daily_USGS, c("agency_cd", "site_no", "X_00060_00003_cd"))

colnames(raw_daily_USGS) <- c("Date", site_name[1])

# daily_streamflow_CA <- left_join(daily_streamflow_CA, raw_daily_USGS, by "Date")
# view(daily_streamflow_CA)


### Kaweah R Marble Fork ###
site_number <- c("11208000")
site_name <- site_info$station_nm

raw_daily_USGS <- readNWISdv(site_number, parameter_code, start_date, end_date)

raw_daily_USGS <- drop_columns(raw_daily_USGS, c("agency_cd", "site_no", "X_00060_00003_cd"))

colnames(raw_daily_USGS) <- c("Date", site_name[1])

daily_streamflow_CA <- left_join(daily_streamflow_CA, raw_daily_USGS)
# view(daily_streamflow_CA)


### EF Kaweah R Near Three Rivers ###
site_number <- c("11208730")   
site_name <- site_info$station_nm

raw_daily_USGS <- readNWISdv(site_number, parameter_code, start_date, end_date)
print(raw_daily_USGS [raw_daily_USGS == 0.00])
raw_daily_USGS <- drop_columns(raw_daily_USGS, c("agency_cd", "site_no", "X_00060_00003_cd"))

raw_daily_USGS$X_00060_00003 [raw_daily_USGS$X_00060_00003 == 0.00] <- 0.001

colnames(raw_daily_USGS) <- c("Date", site_name[1])


daily_streamflow_CA <- left_join(daily_streamflow_CA, raw_daily_USGS)
# view(daily_streamflow_CA)


### EF Kaweah R Below Monarch Near Hamond ---> NO DATA!!! ###
site_number <- c("11208615")   
site_name <- site_info$station_nm

raw_daily_USGS <- readNWISdv(site_number, parameter_code, start_date, end_date)

raw_daily_USGS <- drop_columns(raw_daily_USGS, c("agency_cd", "site_no", "X_00060_00003_cd"))

colnames(raw_daily_USGS) <- c("Date", site_name[1])

daily_streamflow_CA <- left_join(daily_streamflow_CA, raw_daily_USGS)
# view(daily_streamflow_CA)


### Kaweah R Marble Fork Above Horse C Near Lodge Pole ###
site_number <- c("11206820")   
site_name <- site_info$station_nm

raw_daily_USGS <- readNWISdv(site_number, parameter_code, start_date, end_date)

raw_daily_USGS <- drop_columns(raw_daily_USGS, c("agency_cd", "site_no", "X_00060_00003_cd"))

colnames(raw_daily_USGS) <- c("Date", site_name[1])

daily_streamflow_CA <- left_join(daily_streamflow_CA, raw_daily_USGS)
view(daily_streamflow_CA)


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



### All Sites Info ###
site_number <- c("04097540", "04096900", "04101500")
site_info <- readNWISsite(site_number)
site_stats <- readNWISstat(
        site_number,
        parameter_code,
        startDate = "",
        endDate = "",
        convertType = TRUE,
        statReportType = "monthly",
        statType = "mean"
)




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
