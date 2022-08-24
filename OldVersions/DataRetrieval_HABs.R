library(readr)
library(readxl)
library(plotly)
library(ggthemes)
library(tidyverse)
library(DataExplorer)
library(anomalize)
library(outliers)
library(dplyr)
library(jsonlite)
library(jsonify)
library(dataRetrieval)

setwd("~/GradSchool/CyanoHABs")
wname <- getwd() # set back to working directory
dname <- paste(wname,"data",sep="/") # will open data file in working directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in working directory
pname<-paste(wname,"plots",sep="/") # will open plots file in working directory
fname<-paste(wname,"functions",sep="/") # will open functions file in working directory

# --------------------- IMPORT / RETRIEVE DATA -------------------------
SI_tables <- read_excel("~/GradSchool/CyanoHABs/2016 09 28 Harris SI Tables.xlsx", sheet = "Supplemental Table 3", skip = 2)
realtime_data <- read_excel("~/GradSchool/CyanoHABs/2018 02 15 RealTime data 01-14 Cheney.xlsx")
Cheney_database <- read_excel("~/GradSchool/CyanoHABs/2018 01 25 Cheney Database.xlsx")

site_number <- "07144790"   # Cheney
Cheney_info <- readNWISsite(site_number)
# view(Cheney_info)
parameter_code <- c("00010", "00300", "00400", "62614", "63680", "95202")   
parameter_names <- c("water temperature", "dissolved oxygen", "pH", "water elevation (above NGVD29)", "turbidity", "cyanobacteria")
start_date <- "2001-04-03"
end_date <- "2013-04-03"

raw_daily_USGS <- readNWISdv(site_number, parameter_code, start_date, end_date)
raw_waterquality_USGS_watertemp <- readNWISqw(site_number, parameter_code[0], start_date, end_date)


# --------------------- PREPARE DATA -------------------------
SI_tables <- rename(SI_tables, `DATE` = `Date`)
data_join <- left_join(realtime_data, SI_tables, by = "DATE") 

geosmin_data <- data_join[order(data_join$DATE),]
geosmin_json <- toJSON(data_join[ ,c("TIME", "Geo")], dataframe = "rows", null = "null", na = "null", Date = "epoch")

geosmin <- data_join[ , c("DATE", "Geo")]
write.csv(geosmin, "~/GradSchool/CyanoHABs/JSON/geosmin.csv", row.names = FALSE)

print(geosmin)

geosmin_plot <- ggplot(geosmin)

watertemp_data <- raw_waterquality_USGS_watertemp[ , c("sample_dt", "sample_tm", "result_va")]







q_select_syracuse <- streamflow_monthly_syracuse[-(1) , c(5:7)] %>%
  filter(year_nu > 1949) %>% 
  filter(mean_va > 0)
q_select_syracuse[ , 1:3] <- sapply(q_select_syracuse[ , 1:3], as.numeric)
q_select_gardencity <- streamflow_monthly_gardencity[-(1) , c(5:7)] %>% 
  filter(year_nu > 1949) %>% 
  filter(mean_va > 0)
q_select_gardencity[ , 1:3] <- sapply(q_select_gardencity[ , 1:3], as.numeric)
q_select_dodgecity <- streamflow_monthly_dodgecity[-(1) , c(5:7)] %>% 
  filter(year_nu > 1949) %>% 
  filter(mean_va > 0)
q_select_dodgecity[ , 1:3] <- sapply(q_select_dodgecity[ , 1:3], as.numeric)
q_select_greatbend <- streamflow_monthly_greatbend[-(1) , c(5:7)] %>% 
  filter(year_nu > 1949) %>% 
  filter(mean_va > 0)
q_select_greatbend[ , 1:3] <- sapply(q_select_greatbend[ , 1:3], as.numeric)
q_select_wichita <- streamflow_monthly_wichita[-(1) , c(5:7)] %>% 
  filter(year_nu > 1949) %>% 
  filter(mean_va > 0)
q_select_wichita[ , 1:3] <- sapply(q_select_wichita[ , 1:3], as.numeric)

quantile_q_syracuse <- quantile(q_select_syracuse$mean_va, prob = c(0.10, 0.25, 0.75, 0.90), type = 5)
upperiqr_q_syracuse <- quantile_q_syracuse[3] + 1.5 * (quantile_q_syracuse[3] - quantile_q_syracuse[2])
quantile_q_gardencity <- quantile(q_select_gardencity$mean_va, prob = c(0.10, 0.25, 0.75, 0.90), type = 5)
upperiqr_q_gardencity <- quantile_q_gardencity[3] + 1.5 * (quantile_q_gardencity[3] - quantile_q_gardencity[2])
quantile_q_dodgecity <- quantile(q_select_dodgecity$mean_va, prob = c(0.10, 0.25, 0.75, 0.90), type = 5)
upperiqr_q_dodgecity <- quantile_q_dodgecity[3] + 1.5 * (quantile_q_dodgecity[3] - quantile_q_dodgecity[2])
quantile_q_greatbend <- quantile(q_select_greatbend$mean_va, prob = c(0.10, 0.25, 0.75, 0.90), type = 5)
upperiqr_q_greatbend <- quantile_q_greatbend[3] + (1.5 * (quantile_q_greatbend[3] - quantile_q_greatbend[2]))
quantile_q_wichita <- quantile(q_select_wichita$mean_va, prob = c(0.10, 0.25, 0.75, 0.90), type = 5)
upperiqr_q_wichita <- quantile_q_wichita[3] + (1.5 * (quantile_q_wichita[3] - quantile_q_wichita[2]))


gw_select <- groundwater_monthly_hamilton[ , c(1,4)] %>% 
  filter(Year > 1949)
Year <- gsub("(.{6}.)", "\\1 ", pdsi_annual_climdiv7$`   YearMonth`)[[1]]
pdsi_select <- pdsi_annual_climdiv7[ , ]

wuirr_select <- wateruse_annual_hamilton[ , c(1,2)]
wuirr_select$`Diverted (ft^3/s)` <- (wateruse_annual_hamilton$`Total Acre-Feet Diverted` * 43559.935)/(1*3.154e+7)
wuirr_select <- wuirr_select[ , -(2)]


