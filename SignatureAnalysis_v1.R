library(readr)
library(odbc)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(DataExplorer)
library(dplyr)
library(jsonlite)
library(streamDepletr)
library(dataRetrieval)
library(xts)
library(hydrostats)
library(waterData)
library(lfstat)


setwd("~/GradSchool/DiscoverStreams")
wname <- getwd() # set back to working directory
dname <- paste(wname,"data",sep="/") # will open data file in workikng directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in workikng directory
pname<-paste(wname,"plots",sep="/") # will open plots file in workikng directory

### STREAMFLOW (NWIS) - SCOTT VALLEY, CALIFORNIA ###
sites_ca <- c("11519500", "11519000", "11518200", "11518050")
color_ca <- c("aquamarine3", "darkblue", "darkred", "burlywood3")
## #66cdaa, #01178b, #8a0f00, #cdaa7d

### STREAMFLOW (NWIS) - PRAIRIE RIVER, MICHIGAN ###
sites_mi <- c("04097540", "040975253", "04097526", "04097528", "040975296", "04097529", "040975299", "04097530", "04097540", "0409754049", "0409754132", "0409754153", "0409754167", "04097500" )
color_mi <- c("deepskyblue", "cadetblue2", "deepskyblue4") 
## #01bfff, #8ee5ee, #01688b

### STREAMFLOW (NWIS) - KANSAS ###



### RETRIEVE STREAMFLOW DATA FROM NWIS ###
site_number <- "04097540"
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- ""
end_date <- ""


raw_daily <- readNWISdv(site_number, parameter_code, start_date, end_date)

# REFORMAT DATA
raw_daily <- drop_columns(raw_daily, c("site_no", "agency_cd", "X_00060_00003_cd"))

colnames(raw_daily) <- c("Date", "Discharge_cfs")

streamflow_Prairie <- raw_daily


### PLOTS ###

p <- ggplot(raw_daily, aes(x=Date, y=Discharge_cfs)) +
        geom_line(color = color_ca[4]) +
        scale_y_continuous(trans='log10')

# Set axis limits c(min, max)
min <- as.Date("1950-1-1")
max <- as.Date("2021-12-31")
p + scale_x_date(limits = c(min, max))





### WATER USE (NWIS) ###
site_number <- "11519500" ## Scott River
site_number <- "04097528" ## Prairie River - Branch County
site_number <- "04097540" ## Prairie River - St. Joseph County
site_number <- "04101500" ## St. Joseph River

site_info <- readNWISsite(site_number)

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

waterUSE_SV_select <- waterUse_SV[ , c(5,243)]
# 243 	Irrigation..Total.total.consumptive.use..in.Mgal.d


waterUse_Pr <- readNWISuse(
        stateCd = state,
        countyCd = county,
        years = "ALL",
        categories = "ALL",
        convertType = TRUE,
        transform = FALSE
)

waterUSE_Pr_select <- waterUse_Pr[ , c(5,243)]

# Convert units from Mgal/d to cfs
waterUSE_Pr_select$nwis_irr_cfs <- ((as.numeric(waterUSE_Pr_select$Irrigation..Total.total.consumptive.use..in.Mgal.d)*10^6)/(7.48*86400))








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
