library(readr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(DataExplorer)


setwd("~/GradSchool/DiscoverFramework")
wname <- getwd() # set back to working directory
dname <- paste(wname,"data",sep="/") # will open data file in workikng directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in workikng directory
pname<-paste(wname,"plots",sep="/") # will open plots file in workikng directory
fname<-paste(wname,"functions",sep="/") # will open functions file in workikng directory

CSVcolNames <- c("Year", "Month", "Value", "JSON")
CSVtoJSON <- read_excel("~/GradSchool/DiscoverWaterLive/CSVtoJSON_DW.xlsx", col_names = CSVcolNames)
CSVtoJSON <- CSVtoJSON[ , -(3:4)]

pdsi_annual_climdiv7 <- read_delim("~/GradSchool/Data/PDSI_NOAA/2017/ClimDiv7_Monthly_20173112.txt", delim = ", ")


groundwater_winter_barton <- read_excel("~/GradSchool/Data/GW_Level_WIZARD/20200915/Barton_GW_AnnualAvg.xlsx", sheet = "WinterAverages", skip = 2)
groundwater_winter_finney <- read_excel("~/GradSchool/Data/GW_Level_WIZARD/20200915/Finney_GW_AnnualAvg.xlsx", sheet = "WinterAverages", skip = 2)
groundwater_winter_ford <- read_excel("~/GradSchool/Data/GW_Level_WIZARD/20200915/Ford_GW_AnnualAvg.xlsx", sheet = "WinterAverages", skip = 2)
groundwater_winter_hamilton <- read_excel("~/GradSchool/Data/GW_Level_WIZARD/20200915/Hamilton_GW_AnnualAvg.xlsx", sheet = "WinterAverages", skip = 2)
groundwater_winter_sedgwick <- read_excel("~/GradSchool/Data/GW_Level_WIZARD/20200915/Sedgwick_GW_AnnualAvg.xlsx", sheet = "WinterAverages", skip = 2)

irrigation_barton <- read_excel("~/GradSchool/Data/Pumping_WIMAS/20200915/Barton_WaterUseIrrigateation_AnnualSummary.xlsx")
irrigation_finney <- read_excel("~/GradSchool/Data/Pumping_WIMAS/20200915/Finney_WaterUseIrrigateation_AnnualSummary.xlsx")
irrigation_ford <- read_excel("~/GradSchool/Data/Pumping_WIMAS/20200915/Ford_WaterUseIrrigateation_AnnualSummary.xlsx")
irrigation_hamilton <- read_excel("~/GradSchool/Data/Pumping_WIMAS/20200915/Hamilton_WaterUseIrrigateation_AnnualSummary.xlsx")
irrigation_sedgwick <- read_excel("~/GradSchool/Data/Pumping_WIMAS/20200915/Sedgwick_WaterUseIrrigateation_AnnualSummary.xlsx")

streamflow_monthly_syracuse <- read_tsv("~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/7138000_Syracuse_Qmo.csv", skip =35, col_types = cols(agency_cd = "c", site_no = "d", parameter_cd = "d", ts_id = "d", year_nu = "d", month_nu = "d", mean_va = "d"))
spec(streamflow_monthly_syracuse)
streamflow_monthly_dodgecity <- read_tsv("~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/7139500_DodgeCity_Qmo.csv", skip =35, col_types = cols(agency_cd = "c", site_no = "d", parameter_cd = "d", ts_id = "d", year_nu = "d", month_nu = "d", mean_va = "d"))
spec(streamflow_monthly_dodgecity)
streamflow_monthly_gardencity <- read_tsv("~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/7139500_GardenCity_Qmo.csv", skip =35, col_types = cols(agency_cd = "c", site_no = "d", parameter_cd = "d", ts_id = "d", year_nu = "d", month_nu = "d", mean_va = "d"))
spec(streamflow_monthly_gardencity)
streamflow_monthly_greatbend <- read_tsv("~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/7141300_GreatBend_Qmo.csv", skip =35, col_types = cols(agency_cd = "c", site_no = "d", parameter_cd = "d", ts_id = "d", year_nu = "d", month_nu = "d", mean_va = "d"))
spec(streamflow_monthly_greatbend)
streamflow_monthly_wichita <- read_tsv("~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/7144300_Wichita_Qmo.csv", skip =35, col_types = cols(agency_cd = "c", site_no = "d", parameter_cd = "d", ts_id = "d", year_nu = "d", month_nu = "d", mean_va = "d"))
spec(streamflow_monthly_wichita)

# --------------------- IMPORT & PREPARE DATA -------------------------

Year <- gsub("(.{6}.)", "\\1 ", pdsi_annual_climdiv7$`   YearMonth`)[[1]]
pdsi_select <- pdsi_annual_climdiv7[ , ]


### GROUNDWATER (WIZARD) ###
data_join <- left_join(CSVtoJSON, groundwater_winter_barton, by = "Year")

gw_winter_barton <- data_join[ , c(1,2,5)]
write.csv(gw_winter_barton, "~/GradSchool/Data/GW_Level_WIZARD/20200120/Barton_GW_WinterAvg.csv", row.names = FALSE, na = "null")

data_join <- left_join(CSVtoJSON, groundwater_winter_finney, by = "Year")

gw_winter_finney <- data_join[ , c(1,2,5)]
write.csv(gw_winter_finney, "~/GradSchool/Data/GW_Level_WIZARD/20200120/Finney_GW_WinterAvg.csv", row.names = FALSE, na = "null")

data_join <- left_join(CSVtoJSON, groundwater_winter_ford, by = "Year")

gw_winter_ford <- data_join[ , c(1,2,5)]
write.csv(gw_winter_ford, "~/GradSchool/Data/GW_Level_WIZARD/20200120/Ford_GW_WinterAvg.csv", row.names = FALSE, na = "null")

data_join <- left_join(CSVtoJSON, groundwater_winter_hamilton, by = "Year")

gw_winter_hamilton <- data_join[ , c(1,2,5)]
write.csv(gw_winter_hamilton, "~/GradSchool/Data/GW_Level_WIZARD/20200120/Hamilton_GW_WinterAvg.csv", row.names = FALSE, na = "null")

data_join <- left_join(CSVtoJSON, groundwater_winter_sedgwick, by = "Year")

gw_winter_sedgwick <- data_join[ , c(1,2,5)]
write.csv(gw_winter_sedgwick, "~/GradSchool/Data/GW_Level_WIZARD/20200120/Sedgwick_GW_WinterAvg.csv", row.names = FALSE, na = "null")

### IRRIGATION (WIMAS) ###
wuirr_select <- irrigation_barton
wuirr_select$`Diverted (ft^3/s)` <- round(((irrigation_barton$`Total Acre-Feet Diverted` * 43559.935)/31536000), 2)  # convert volume diverted in acre-feet/yr to cubic feet/yr then convert cubic feet/yr to cubic feet/s, round result to 2 places

data_join <- left_join(CSVtoJSON, wuirr_select, by = "Year")

wuirr_barton <- data_join[ , c(1,2,5)]
write.csv(wuirr_barton, "~/GradSchool/Data/Pumping_WIMAS/20200915/Barton_WaterUseIrrigateation_AnnualSummary.csv", row.names = FALSE, na = "null")

wuirr_select <- irrigation_finney
wuirr_select$`Diverted (ft^3/s)` <- round(((irrigation_finney$`Total Acre-Feet Diverted` * 43559.935)/31536000), 2)  # convert volume diverted in acre-feet/yr to cubic feet/yr then convert cubic feet/yr to cubic feet/s, round result to 2 places

data_join <- left_join(CSVtoJSON, wuirr_select, by = "Year")

wuirr_finney <- data_join[ , c(1,2,5)]
write.csv(wuirr_finney, "~/GradSchool/Data/Pumping_WIMAS/20200915/Finney_WaterUseIrrigateation_AnnualSummary.csv", row.names = FALSE, na = "null")

wuirr_select <- irrigation_ford
wuirr_select$`Diverted (ft^3/s)` <- round(((irrigation_ford$`Total Acre-Feet Diverted` * 43559.935)/31536000), 2)  # convert volume diverted in acre-feet/yr to cubic feet/yr then convert cubic feet/yr to cubic feet/s, round result to 2 places

data_join <- left_join(CSVtoJSON, wuirr_select, by = "Year")

wuirr_ford <- data_join[ , c(1,2,5)]
write.csv(wuirr_ford, "~/GradSchool/Data/Pumping_WIMAS/20200915/Ford_WaterUseIrrigateation_AnnualSummary.csv", row.names = FALSE, na = "null")

wuirr_select <- irrigation_hamilton
wuirr_select$`Diverted (ft^3/s)` <- round(((irrigation_hamilton$`Total Acre-Feet Diverted` * 43559.935)/31536000), 2)  # convert volume diverted in acre-feet/yr to cubic feet/yr then convert cubic feet/yr to cubic feet/s, round result to 2 places

data_join <- left_join(CSVtoJSON, wuirr_select, by = "Year")

wuirr_hamilton <- data_join[ , c(1,2,7)]
write.csv(wuirr_hamilton, "~/GradSchool/Data/Pumping_WIMAS/20200915/Hamilton_WaterUseIrrigateation_AnnualSummary.csv", row.names = FALSE, na = "null")

wuirr_select <- irrigation_sedgwick
wuirr_select$`Diverted (ft^3/s)` <- round(((irrigation_sedgwick$`Total Acre-Feet Diverted` * 43559.935)/31536000), 2)  # convert volume diverted in acre-feet/yr to cubic feet/yr then convert cubic feet/yr to cubic feet/s, round result to 2 places

data_join <- left_join(CSVtoJSON, wuirr_select, by = "Year")

wuirr_sedgwick <- data_join[ , c(1,2,5)]
write.csv(wuirr_sedgwick, "~/GradSchool/Data/Pumping_WIMAS/20200915/Sedgwick_WaterUseIrrigateation_AnnualSummary.csv", row.names = FALSE, na = "null")

### STREAMFLOW (NWIS) ###
qColNames = c("Year", "Month", "Discharge")

q_select <- streamflow_monthly_syracuse[-(1) , c(5:7)] %>% 
  filter(year_nu > 1949)
colnames(q_select) = qColNames

data_join <- left_join(CSVtoJSON, q_select)

q_syracuse <- data_join
write.csv(q_syracuse, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/Syracuse_Qmo.csv", row.names = FALSE, na = "null")

q_select <- streamflow_monthly_gardencity[-(1) , c(5:7)] %>% 
  filter(year_nu > 1949)
colnames(q_select) = qColNames

data_join <- left_join(CSVtoJSON, q_select)

q_gardencity <- data_join
write.csv(q_gardencity, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/GardenCity_Qmo.csv", row.names = FALSE, na = "null")

q_select <- streamflow_monthly_dodgecity[-(1) , c(5:7)] %>% 
  filter(year_nu > 1949)
colnames(q_select) = qColNames

data_join <- left_join(CSVtoJSON, q_select)

q_dodgecity <- data_join
write.csv(q_dodgecity, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/DodgeCity_Qmo.csv", row.names = FALSE, na = "null")

q_select <- streamflow_monthly_greatbend[-(1) , c(5:7)] %>% 
  filter(year_nu > 1949)
colnames(q_select) = qColNames

data_join <- left_join(CSVtoJSON, q_select)

q_greatbend <- data_join
write.csv(q_greatbend, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/GreatBend_Qmo.csv", row.names = FALSE, na = "null")

q_select <- streamflow_monthly_wichita[-(1) , c(5:7)] %>% 
  filter(year_nu > 1949)
colnames(q_select) = qColNames

data_join <- left_join(CSVtoJSON, q_select)

q_wichita <- data_join
write.csv(q_wichita, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/Wichita_Qmo.csv", row.names = FALSE, na = "null")
