# data manipulation libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)
# data retrieval libraries
library(dataRetrieval)

######################################################################################
## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00065")
parameter_names <- c("Gage height, feet")
start_date <- ""    
end_date <- "2021-09-30"


## Gap fill Dodge City gage station with gage height values since discharge parameter hasn't been measured since 2007
site_number <- ws_110300KS_all[5, 1]

raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)

df <- huc110300KS_sf_all
df[`ARKANSAS R AT DODGE CITY, KS`][huc110300KS_sf_all[`ARKANSAS R AT DODGE CITY, KS`] == NA] <- 0

#################################################################################
##### PREPARE WATERSHED DATAFRAMES WITH COLUMNS FOR STATS & ANALYTIC VALUES #####
## SET watershed object to work with based on time period
ws_040500MI <- ws_040500MI_hd
ws_110300KS <- ws_110300KS_hd
ws_180102CA <- ws_180102CA_hd

ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_long
ws_180102CA <- ws_180102CA_long

ws_040500MI <- ws_040500MI_ep
ws_110300KS <- ws_110300KS_ep
ws_180102CA <- ws_180102CA_ep


## ADD column for county -- grouped by time period
## HD stations
ws_040500MI$county <- c("Elkhart", "Elkhart", "Berrien", "Steuben", "St. Joseph", "St. Joseph", "Berrien", "Calhoun", "Calhoun", "Calhoun", "Kalamazoo", "Kalamazoo", "Jackson", "Eaton", "Ingham", "Ingham", "Ingham", "Clinton", "Ionia", "Clinton", "Ionia", "Kent", "Kent", "Barry")
ws_110300KS$county <- c("Hamilton", "Hamilton", "Finney", "Barton", "Pawnee", "Barton", "Stafford", "Reno", "Rice", "Sedgwick", "Sedgwick", "Cowley", "Sumner", "Butler", "Cowley")
ws_180102CA$county <- c("Klamath", "Klamath", "Klamath", "Klamath", "Klamath", "Klamath", "Siskiyou", "Siskiyou", "Siskiyou", "Siskiyou", "Siskiyou", "Humboldt", "Del Norte", "Siskiyou", "Humboldt", "Trinity", "Trinity", "Trinity")

## Long stations
ws_040500MI$county <- c("Ingham", "Ingham", "Kent")
ws_110300KS$county <- c("Hamilton", "Finney", "Ford", "Sedgwick", "Cowley", "Cowley")
ws_180102CA$county <- c("Klamath", "Klamath", "Klamath", "Siskiyou", "Siskiyou", "Del Norte", "Siskiyou", "Humboldt", "Trinity", "Trinity")

## EP stations
ws_040500MI$county <- c("Ingham", "Ingham", "Kent")
ws_110300KS$county <- c("Hamilton", "Finney", "Pawnee", "Pawnee", "Sedgwick", "Cowley", "Cowley")
ws_180102CA$county <- c("Klamath", "Klamath", "Klamath", "Klamath", "Klamath", "Klamath", "Siskiyou", "Siskiyou", "Del Norte", "Siskiyou", "Humboldt", "Trinity")

## ADD column id for indexing --- ESSENTIAL FOR MAPPING
ws_040500MI$id <- 1:nrow(ws_040500MI)
ws_110300KS$id <- 1:nrow(ws_110300KS)
ws_180102CA$id <- 1:nrow(ws_180102CA)

## ADD columns for entire record calculation, p, and tau values for each station from Mann-Kendall Test on Q10
ws_040500MI$Q10 <- NA
ws_040500MI$p_Q10 <- NA
ws_040500MI$tau_Q10 <- NA
ws_110300KS$Q10 <- NA
ws_110300KS$p_Q10 <- NA
ws_110300KS$tau_Q10 <- NA
ws_180102CA$Q10 <- NA
ws_180102CA$p_Q10 <- NA
ws_180102CA$tau_Q10 <- NA

## ADD columns for entire record calculation, p, and tau values for each station from Mann-Kendall Test on Mean Discharge (Q)
ws_040500MI$MeanQ <- NA
ws_040500MI$p_MeanQ <- NA
ws_040500MI$tau_MeanQ <- NA
ws_110300KS$MeanQ <- NA
ws_110300KS$p_MeanQ <- NA
ws_110300KS$tau_MeanQ <- NA
ws_180102CA$MeanQ <- NA
ws_180102CA$p_MeanQ <- NA
ws_180102CA$tau_MeanQ <- NA

## ADD columns for entire record calculation, p, and tau values for each station from Mann-Kendall Test on Q50
ws_040500MI$Q50 <- NA
ws_040500MI$p_Q50 <- NA
ws_040500MI$tau_Q50 <- NA
ws_110300KS$Q50 <- NA
ws_110300KS$p_Q50 <- NA
ws_110300KS$tau_Q50 <- NA
ws_180102CA$Q50 <- NA
ws_180102CA$p_Q50 <- NA
ws_180102CA$tau_Q50 <- NA

## ADD columns for entire record calculation, p, and tau values for each station from Mann-Kendall Test on Mean Baseflow
ws_040500MI$Baseflow <- NA
ws_040500MI$p_Baseflow <- NA
ws_040500MI$tau_Baseflow <- NA
ws_110300KS$Baseflow <- NA
ws_110300KS$p_Baseflow <- NA
ws_110300KS$tau_Baseflow <- NA
ws_180102CA$Baseflow <- NA
ws_180102CA$p_Baseflow <- NA
ws_180102CA$tau_Baseflow <- NA

## ADD columns for entire record calculation, p, and tau values for each station from Mann-Kendall Test on MAM7
ws_040500MI$MAM7 <- NA
ws_040500MI$p_MAM7 <- NA
ws_040500MI$tau_MAM7 <- NA
ws_110300KS$MAM7 <- NA
ws_110300KS$p_MAM7 <- NA
ws_110300KS$tau_MAM7 <- NA
ws_180102CA$MAM7 <- NA
ws_180102CA$p_MAM7 <- NA
ws_180102CA$tau_MAM7 <- NA

## ADD columns for entire record calculation, p, and tau values for each station from Mann-Kendall Test on Q90
ws_040500MI$Q90 <- NA
ws_040500MI$p_Q90 <- NA
ws_040500MI$tau_Q90 <- NA
ws_110300KS$Q90 <- NA
ws_110300KS$p_Q90 <- NA
ws_110300KS$tau_Q90 <- NA
ws_180102CA$Q90 <- NA
ws_180102CA$p_Q90 <- NA
ws_180102CA$tau_Q90 <- NA

## ADD columns for entire record calculation, p, and tau values for each station from Mann-Kendall Test on Q95
ws_040500MI$Q95 <- NA
ws_040500MI$p_Q95 <- NA
ws_040500MI$tau_Q95 <- NA
ws_110300KS$Q95 <- NA
ws_110300KS$p_Q95 <- NA
ws_110300KS$tau_Q95 <- NA
ws_180102CA$Q95 <- NA
ws_180102CA$p_Q95 <- NA
ws_180102CA$tau_Q95 <- NA


## SAVE resulting watershed object to dataframe
ws_040500MI_hd <- ws_040500MI
ws_110300KS_hd <- ws_110300KS
ws_180102CA_hd <- ws_180102CA

ws_040500MI_long <- ws_040500MI
ws_110300KS_long <- ws_110300KS
ws_180102CA_long <- ws_180102CA

ws_040500MI_ep <- ws_040500MI
ws_110300KS_ep <- ws_110300KS
ws_180102CA_ep <- ws_180102CA


