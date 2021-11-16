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

### STREAMFLOW (NWIS) - SCOTT VALLEY, CALIFORNIA ###
site_number <- c("11519500", "11519000", "11518200", "11518050")
site_info <- readNWISsite(site_number)
color_sv <- c("aquamarine3", "darkblue", "darkred", "burlywood3")

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




### PLOTS ###

plot(raw_daily_Prairie1$Date, raw_daily_Prairie1$Discharge_cfs,
     type = "l", 
     # main = "Michigan", 
     xlab = "Date", 
     ylab = "Discharge (Cfs)",
     # log = "x,y",
     col = color_mi[1])
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

ggplot(raw_daily_Prairie1, aes(x=Date, y=Discharge_cfs)) +
        geom_line(color = color_mi[1]) +
        scale_y_continuous(trans='log10')







### WATER USE (NWIS) ###
site_number <- "11519500"
site_number <- "04101500" ## St. Joseph
site_number <- "04097540" ## Prairie River
site_number <- "04097540" ## Nottawa Creek
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

attributes <- colnames(waterUse_SV)
view(attributes)

waterUSE_SV_select <- waterUse_SV[ , c(5,18)]

waterUse_StJo <- readNWISuse(
        stateCd = state,
        countyCd = county,
        years = "ALL",
        categories = "ALL",
        convertType = TRUE,
        transform = FALSE
)

waterUSE_StJo_select <- waterUse_StJo[ , c(5,18)]

waterUse_Pr <- readNWISuse(
        stateCd = state,
        countyCd = county,
        years = "ALL",
        categories = "ALL",
        convertType = TRUE,
        transform = FALSE
)

waterUSE_Pr_select <- waterUse_Pr[ , c(5,18)]








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
