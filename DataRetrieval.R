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

streamflow_monthly_hamilton <- read_csv("~/GradSchool/Data/Streamflow_Monthly_NWIS/20170626/Syracuse_MonthlyStreamflow_1902-2017.csv")
groundwater_monthly_hamilton <- read_csv("~/GradSchool/Data/GW_Level_WIZARD/Hamilton_AnnualGWLevel_1939-2017.csv")
pdsi_annual_climdiv7 <- read_delim("~/GradSchool/Data/PDSI_NOAA/2017/ClimDiv7_Monthly_20173112.txt", delim = ", ")
wateruse_irrigation_annual_hamilton <- read_csv("~/GradSchool/Data/Pumping_WIMAS/2017/Hamilton_AnnualWaterUseIrrigation_1957-2016.csv")

--------------------- IMPORT & PREPARE DATA -------------------------
q_select <- streamflow_monthly_hamilton[-(1) , c(2, 5:7)] %>% 
  filter(year_nu > 1949)
gw_select <- groundwater_monthly_hamilton[ , c(1,4)] %>% 
  filter(Year > 1949)
Year <- gsub("(.{6}.)", "\\1 ", pdsi_annual_climdiv7$`   YearMonth`)[[1]]
pdsi_select <- pdsi_annual_climdiv7[ , ]

wuirr_select <- wateruse_annual_hamilton[ , c(1,2)]
wuirr_select$`Diverted (ft^3/s)` <- (wateruse_annual_hamilton$`Total Acre-Feet Diverted` * 43559.935)/(1*3.154e+7)
wuirr_select <- wuirr_select[ , -(2)]


