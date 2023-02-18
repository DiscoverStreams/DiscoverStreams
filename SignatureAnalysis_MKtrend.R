# database connection/retrieval libraries
library(readr)
library(dataRetrieval)

# data manipulation libraries
# library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(stringr)
library(patchwork)

# data analysis/statistical libraries
library(lfstat)
library(trend)
library(modifiedmk)
library(zoo)



### RETRIEVE watershed dataframes from csv file ###
ws_040500MI_hd <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_040500MI_hd_1960-2021.csv", header = TRUE, row.names = 1)
ws_110300KS_hd <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_110300KS_hd_1963-2021.csv", header = TRUE, row.names = 1)
ws_180102CA_hd <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_180102CA_hd_1962-2021.csv", header = TRUE, row.names = 1)

ws_040500MI_long <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_040500MI_long.csv", header = TRUE, row.names = 1)
ws_110300KS_long <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_110300KS_long.csv", header = TRUE, row.names = 1)
ws_180102CA_long <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_180102CA_long.csv", header = TRUE, row.names = 1)

ws_040500MI_ep <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_040500MI_ep.csv", header = TRUE, row.names = 1)
ws_110300KS_ep <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_110300KS_ep.csv", header = TRUE, row.names = 1)
ws_180102CA_ep <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_180102CA_ep.csv", header = TRUE, row.names = 1)



## SET watershed dataframe to work with based on time period
ws_040500MI <- ws_040500MI_hd
ws_110300KS <- ws_110300KS_mhd_mmk
ws_180102CA <- ws_180102CA_hd

ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_mlong_mmk
ws_180102CA <- ws_180102CA_long

ws_040500MI <- ws_040500MI_ep
ws_110300KS <- ws_110300KS_ep
ws_180102CA <- ws_180102CA_ep


########## STREAM DEPLETION METRICS ##########
## Run once, CREATE separate df of consecutive years to ensure all years represented in plot when NA years are dropped
sf_metrics_year_MI_hd <- data.frame(seq(1960, 2021, by = 1))
colnames(sf_metrics_year_MI_hd) <- "hyear"
sf_metrics_year_KS_hd <- data.frame(seq(1963, 2021, by = 1))
colnames(sf_metrics_year_KS_hd) <- "hyear"
sf_metrics_year_CA_hd <- data.frame(seq(1962, 2021, by = 1))
colnames(sf_metrics_year_CA_hd) <- "hyear"

sf_metrics_year_MI_long <- data.frame(seq(1901, 2021, by = 1))
colnames(sf_metrics_year_MI_long) <- "hyear"
sf_metrics_year_KS_long <- data.frame(seq(1902, 2021, by = 1))
colnames(sf_metrics_year_KS_long) <- "hyear"
sf_metrics_year_CA_long <- data.frame(seq(1904, 2021, by = 1))
colnames(sf_metrics_year_CA_long) <- "hyear"

sf_metrics_year_MI_ep <- data.frame(seq(1901, 1940, by = 1))
colnames(sf_metrics_year_MI_ep) <- "hyear"
sf_metrics_year_KS_ep <- data.frame(seq(1902, 1940, by = 1))
colnames(sf_metrics_year_KS_ep) <- "hyear"
sf_metrics_year_CA_ep <- data.frame(seq(1904, 1940, by = 1))
colnames(sf_metrics_year_CA_ep) <- "hyear"

## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- "1959-10-01"  # Michigan HD data
start_date <- "1962-10-01"  # Kansas HD data
start_date <- "1961-10-01"  # California HD data
start_date <- "1951-10-01"  # All 1951 data
start_date <- ""  # Long and EP data
end_date <- "2022-09-30" # HD, long, and 1951 data
end_date <- "1940-09-30" # EP data


## GAGE STATIONS OF INTEREST
i = 1 ## KS_long - Syracuse
i = 2 ## KS_long - Garden City
i = 3 ## KS_long - Dodge City
i = 6 ## KS_long - Walnut R

i = 2 ## KS_hd - Syracuse
i = 3 ## KS_hd - Garden City
i = 5 ## KS_hd - Pawnee
i = 7 ## KS_hd - Rattlesnake C 07142300
i = 12 ## KS_hd - Arkansas City
i = 15 ## KS_hd - Walnut R


## LOOP 1 - metrics calculated over entire time period -> RETRIEVE streamflow data to get dataframe without NAs -- NA's end up being coerced by createlfdataframe -- SET start date and end date before running for loop
# for (i in 1:nrow(ws_040500MI)) {
#         site_number <- ws_040500MI$site_no[i]
for (i in 1:nrow(ws_110300KS)) {
      site_number <- ws_110300KS$site_no[i]
# for (i in 1:nrow(ws_180102CA)) {
#   site_number <- ws_180102CA$site_no[i]
  
  ## RETRIEVE site info and streamflow data for gage station
  site_info <- dataRetrieval::readNWISsite(site_number)
  site_name <- site_info$station_nm
  raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)
  
  ## PREPARE dataframe for calculations 
  sf_select <- subset(raw_daily, select = c(3,4))
  colnames(sf_select) <- c("Date", "Discharge")
  sf_select$year <- as.numeric(format(sf_select[[1]], "%Y"))
  
  ## CONVERT from ft^3/s to m^3/s
  sf_select$Discharge <- sf_select$Discharge / 35.3146667
  
  ## PREPARE data for use with lfstat - needs timestamp broken into columns for day, month, year, flow
  sf <- separate(sf_select, "Date", c("year", "month", "day"), "-")
  colnames(sf) <- c("year","month", "day", "flow")
  sf <- createlfobj(sf, hyearstart = 10)
  sf$hyear <- as.numeric(sf$hyear)
  sf$year <- as.numeric(sf$year)
  
  ## JOIN hydrologic year to sf_select to calculate sf metrics on water years instead of calendar years
  sf_select <- left_join(sf_select, sf[ ,c(3,5)], by = "year")
  
  ## CALCULATE Q90 - 90th percentile of flow that is exceeded 10% of the time
  # Q10 <- sf_select %>% 
  #   summarize(Q10 = quantile(Discharge, probs = 0.9, na.rm = TRUE)) %>% 
  #   #https://www.tutorialspoint.com/how-to-extract-initial-last-or-middle-characters-from-a-string-in-r
  #   as.numeric(str_sub(Q10,-4,-2))
  Q10 <- Qxx(sf, Qxx = 10, yearly = FALSE) 
  
  ## CALCULATE annual mean discharge
  # meanQ <- sf_select %>%
  #   na.omit(sf_select) %>%
  #   summarize(meanQ = mean(Discharge))
  # meanQ <- str_sub(meanQ,1,-1) %>%
  #   as.numeric(meanQ)
  meanQ <- meanflow(sf, yearly = FALSE)
  
  ## CALCULATE Q50 - 50th percentile of flow that is exceeded 50% of the time
  # Q50 <- sf_select %>% 
  #   summarize(Q50 = quantile(Discharge, probs = 0.5, na.rm = TRUE)) %>% 
  #   as.numeric(str_sub(Q50,-4,-2))
  Q50 <- Qxx(sf, Qxx = 50, yearly = FALSE) 
  
  ## CALCULATE Annual Mean Baseflow - Average baseflow for each year
  baseflow <- sf %>%
    na.omit(sf) %>%  
    summarise(baseflow = mean(Baseflow)) 
  baseflow <- str_sub(baseflow,1,-1) %>% 
    as.numeric(baseflow)
  
  ## CALCULATE MAM7 - Mean Annual Minimum over 7-day period -> 7-day low flow
  MAM7 <- lfstat::MAM(sf, n=7, yearly = TRUE)
  colnames(MAM7) <- c("hyear","MAM7")
  MAM7 <- MAM7 %>%
    summarise(MAM7 = mean(MAM7))
  MAM7 <- str_sub(MAM7,1,-1) %>%
    as.numeric(MAM7)
  
  ## CALCULATE Q90 - 10th percentile of flow that is exceeded 90% of the time
  # Q90 <- sf_select %>% 
  #   summarize(Q90 = quantile(Discharge, probs = 0.1, na.rm = TRUE)) %>% 
  #   as.numeric(str_sub(Q90,-4,-1))
  Q90 <- Qxx(sf, Qxx = 90, yearly = FALSE) 
  
  ## CALCULATE Q90 - 90th percentile of flow that is exceeded 95% of the time
  # Q95 <- sf_select %>% 
  #   summarize(Q95= quantile(Discharge, probs = 0.05, na.rm = TRUE)) %>% 
  #   as.numeric(str_sub(Q95,-4,-2))
  Q95 <- Qxx(sf, Qxx = 95, yearly = FALSE)
  
  ## CALCULATE scaled Q90-Q10 spread
  Diff <- (Q10 - Q90) / Q10
  
  
  
  ## CALCULATE BFI - Baseflow Index for each year
  # BFI <- lfstat::BFI(sf, year = "any", breakdays = NULL, yearly = TRUE)
  # BFI <- data.frame(sf_metrics$Year, BFI)
  # colnames(BFI) <- c("Year", "BFI") 
  # sf_metrics <- left_join(sf_metrics, BFI, by = "Year")
  
  
  ## APPEND metric results to WS dataframe, CHOOSE corresponding watershed before running for loop
  # ws_040500MI$Q10[i] <- as.numeric(Q10)
  # ws_040500MI$MeanQ[i] <- as.numeric(meanQ)
  # ws_040500MI$Q50[i] <- as.numeric(Q50)
  # ws_040500MI$Baseflow[i] <- as.numeric(baseflow)
  # ws_040500MI$MAM7[i] <- as.numeric(MAM7)
  # ws_040500MI$Q90[i] <- as.numeric(Q90)
  # ws_040500MI$Q95[i] <- as.numeric(Q95)
  
  ws_110300KS$Q10[i] <- as.numeric(Q10)
  ws_110300KS$MeanQ[i] <- as.numeric(meanQ)
  ws_110300KS$Q50[i] <- as.numeric(Q50)
  ws_110300KS$Baseflow[i] <- as.numeric(baseflow)
  ws_110300KS$MAM7[i] <- as.numeric(MAM7)
  ws_110300KS$Q90[i] <- as.numeric(Q90)
  ws_110300KS$Q95[i] <- as.numeric(Q95)
  ws_110300KS$Diff[i] <- as.numeric(Diff)
  
  # ws_180102CA$Q10[i] <- as.numeric(Q10)
  # ws_180102CA$MeanQ[i] <- as.numeric(meanQ)
  # ws_180102CA$Q50[i] <- as.numeric(Q50)
  # ws_180102CA$Baseflow[i] <- as.numeric(baseflow)
  # ws_180102CA$MAM7[i] <- as.numeric(MAM7)
  # ws_180102CA$Q90[i] <- as.numeric(Q90)
  # ws_180102CA$Q95[i] <- as.numeric(Q95)
  
}



## LOOP 2 - yearly metrics -> RETRIEVE streamflow data to get dataframe without NAs -- NA's end up being coerced by create lf object -- SET start date and end date before running for loop

i = 3

# for (i in 1:nrow(ws_040500MI)) {
#         site_number <- ws_040500MI$site_no[i]
for (i in 1:nrow(ws_110300KS)) {
      site_number <- ws_110300KS$site_no[i]
# for (i in 1:nrow(ws_180102CA)) {
#   site_number <- ws_180102CA$site_no[i]
  
  ## RETRIEVE site info and streamflow data for gage station
  site_info <- dataRetrieval::readNWISsite(site_number)
  site_name <- site_info$station_nm
  raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)
 
  ## PREPARE dataframe for calculations 
  sf_select <- subset(raw_daily, select = c(3,4))
  colnames(sf_select) <- c("Date", "Discharge")
  sf_select$year <- as.numeric(format(sf_select[[1]], "%Y"))
  
  ## CONVERT from ft^3/s to m^3/s
  sf_select$Discharge <- sf_select$Discharge / 35.3146667
  
  ## PREPARE data for use with lfstat to calculate baseflow and water year - needs timestamp broken into columns for day, month, year, flow
  sf <- separate(sf_select, "Date", c("year", "month", "day"), "-")
  colnames(sf) <- c("year","month", "day", "flow")
  sf <- lfstat::createlfobj(sf, hyearstart = 10)
  sf$hyear <- as.numeric(sf$hyear)
  sf$year <- as.numeric(sf$year)
  
  
  ## CALCULATE Q10 - probability flow will be exceeded 10% of the time (P90)
  # Q10 <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(Q10 = quantile(Discharge, probs = 0.9, na.rm = TRUE))
  
  Q10 <- lfstat::Qxx(sf, Qxx = 10, yearly = TRUE)
  colnames(Q10) <- c("hyear", "Q10")
  
  sf_metrics <- Q10
 
  
  ## CALCULATE annual mean discharge
  # meanQ <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(MeanQ = mean(Discharge))
  meanQ <- lfstat::meanflow(sf, yearly = TRUE)
  colnames(meanQ) <- c("hyear", "MeanQ")
  
  sf_metrics <- left_join(sf_metrics, meanQ, by = "hyear")
  
  
  ## CALCULATE Q50 - probability flow will be exceeded 50% of the time (P50)
  # Q50 <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(Q50 = quantile(Discharge, probs = 0.5, na.rm = TRUE))
 
  Q50 <- lfstat::Qxx(sf, Qxx = 50, yearly = TRUE)
  colnames(Q50) <- c("hyear", "Q50")
  
  sf_metrics <- left_join(sf_metrics, Q50, by = "hyear")
  
  
  ## CALCULATE Annual Mean Baseflow  using sf from lfstat - Average baseflow for each year
  baseflow <- sf %>% 
    group_by(hyear) %>%
    na.omit(sf) %>% 
    summarize(MeanBF = mean(Baseflow))
  baseflow$hyear <- as.numeric(baseflow$hyear)
  sf_metrics <- left_join(sf_metrics, baseflow, by = "hyear")
  
  
  ## CALCULATE MAM7 - Mean Annual Minimum over 7-day period -> 7-day low flow
  MAM7 <- lfstat::MAM(sf, n=7, yearly = TRUE)
  colnames(MAM7) <- c("hyear","MAM7")
  sf_metrics <- left_join(sf_metrics, MAM7, by = "hyear")
  
  
  ## CALCULATE Q10 - probability flow will be exceeded 90% of the time (P10)
  # Q90 <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(Q90 = quantile(Discharge, probs = 0.1, na.rm = TRUE))
  
  Q90 <- lfstat::Qxx(sf, Qxx = 90, yearly = TRUE)
  colnames(Q90) <- c("hyear", "Q90")
  
  sf_metrics <- left_join(sf_metrics, Q90, by = "hyear")
  
  
  ## CALCULATE Q95 - probability flow will be exceeded 95% of the time (P05)
  # Q95 <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(Q95= quantile(Discharge, probs = 0.05, na.rm = TRUE))
  
  Q95 <- lfstat::Qxx(sf, Qxx = 95, yearly = TRUE)
  colnames(Q95) <- c("hyear", "Q95")
  
  sf_metrics <- left_join(sf_metrics, Q95, by = "hyear")
  
  
  ## CALCULATE Q90Q10 spread to look for trends in how the difference between the highest and lowest flows have changed across the period of record
  # sf_metrics$Diff <- sf_metrics$Q10 - sf_metrics$Q90
  
  # j=1
  
  for (j in 1:nrow(sf_metrics)) {
    if (sf_metrics$Q10[j] == 0) {
      sf_metrics$Diff[j] <- NA
    } else if (sf_metrics$Q10[j] != 0) {
      sf_metrics$Diff[j] <- ((sf_metrics$Q10[j]) - (sf_metrics$Q90[j])) / ((sf_metrics$Q10[j]))
    }
    
  }
  

  ## CALCULATE moving-average of Diff metric, then join with sf_metrics df
  # sf_metrics$Diffma5yr <- zoo::rollapply(sf_metrics$Diff, 5, mean, align = "right", fill = NA)
  sf_metrics$DiffMA3yr <- zoo::rollapply(sf_metrics$Diff, 3, mean, align = "right", fill = NA)
  
  
  ## CALCULATE Percent Dry Days
  PerDry <- sf %>%
    dplyr::group_by(hyear) %>%
    dplyr::summarise(NFF = sum(flow == 0, na.rm=TRUE) / 365)

  sf_metrics <- left_join(sf_metrics, PerDry, by = "hyear")

  ## CALCULATE Mann-Kendall Test
  mk_test_Baseflow <- trend::mk.test(na.omit(sf_metrics$MeanBF))
  mk_test_MeanQ <- trend::mk.test(na.omit(sf_metrics$MeanQ))
  mk_test_MAM7 <- trend::mk.test(na.omit(sf_metrics$MAM7))
  mk_test_Q10 <- trend::mk.test(as.numeric(sf_metrics$Q10))
  mk_test_Q50 <- trend::mk.test(as.numeric(sf_metrics$Q50))
  mk_test_Q90 <- trend::mk.test(as.numeric(sf_metrics$Q90))
  mk_test_Q95 <- trend::mk.test(as.numeric(sf_metrics$Q95))

  
  
  ## CALCULATE Modified Mann-Kendall Test
  # mk_test_Baseflow <- modifiedmk::mmky(na.omit(sf_metrics$MeanBF))
  # mk_test_MeanQ <- modifiedmk::mmky(na.omit(sf_metrics$MeanQ))
  # mk_test_MAM7 <- modifiedmk::mmky(na.omit(sf_metrics$MAM7))
  # mk_test_Q10 <- modifiedmk::mmky(as.numeric(sf_metrics$Q10))
  # mk_test_Q50 <- modifiedmk::mmky(as.numeric(sf_metrics$Q50))
  # mk_test_Q90 <- modifiedmk::mmky(as.numeric(sf_metrics$Q90))
  # mk_test_Q95 <- modifiedmk::mmky(as.numeric(sf_metrics$Q95))
 
  
 
  
  # ## APPEND MK results to WS dataframe, CHOOSE corresponding watershed before running for loop
  # # ws_040500MI$p_Q10[i] <- mk_test_Q10$p.value[1]
  # # if(mk_test_Q10$estimates[3] < 0)
  # #   ws_040500MI$tau_Q10[i] <- "-"
  # # if(mk_test_Q10$estimates[3] > 0)
  # #   ws_040500MI$tau_Q10[i] <- "+"
  # # ws_040500MI$p_MeanQ[i] <- mk_test_MeanQ$p.value[1]
  # # if(mk_test_MeanQ$estimates[3] < 0)
  # #   ws_040500MI$tau_MeanQ[i] <- "-"
  # # if(mk_test_MeanQ$estimates[3] > 0)
  # #   ws_040500MI$tau_MeanQ[i] <- "+"
  # # ws_040500MI$p_Q50[i] <- mk_test_Q50$p.value[1]
  # # if(mk_test_Q50$estimates[3] < 0)
  # #   ws_040500MI$tau_Q50[i] <- "-"
  # # if(mk_test_Q50$estimates[3] > 0)
  # #   ws_040500MI$tau_Q50[i] <- "+"
  # # ws_040500MI$p_Baseflow[i] <- mk_test_Baseflow$p.value[1]
  # # if(mk_test_Baseflow$estimates[3] < 0)
  # #   ws_040500MI$tau_Baseflow[i] <- "-"
  # # if(mk_test_Baseflow$estimates[3] > 0)
  # #   ws_040500MI$tau_Baseflow[i] <- "+"
  # # ws_040500MI$p_MAM7[i] <- mk_test_MAM7$p.value[1]
  # # if(mk_test_MAM7$estimates[3] < 0)
  # #   ws_040500MI$tau_MAM7[i] <- "-"
  # # if(mk_test_MAM7$estimates[3] > 0)
  # #   ws_040500MI$tau_MAM7[i] <- "+"
  # # ws_040500MI$p_Q90[i] <- mk_test_Q90$p.value[1]
  # # if(mk_test_Q90$estimates[3] < 0)
  # #   ws_040500MI$tau_Q90[i] <- "-"
  # # if(mk_test_Q90$estimates[3] > 0)
  # #   ws_040500MI$tau_Q90[i] <- "+"
  # # ws_040500MI$p_Q95[i] <- mk_test_Q95$p.value[1]
  # # if(mk_test_Q95$estimates[3] < 0)
  # #   ws_040500MI$tau_Q95[i] <- "-"
  # # if(mk_test_Q95$estimates[3] > 0)
  # #   ws_040500MI$tau_Q95[i] <- "+"
  # 
  
  ws_110300KS$p_Q10[i] <- mk_test_Q10$p.value[1]
  if(mk_test_Q10$estimates[3] < 0) {
    ws_110300KS$tau_Q10[i] <- "-" }
  if(mk_test_Q10$estimates[3] > 0) {
    ws_110300KS$tau_Q10[i] <- "+" }
  ws_110300KS$p_MeanQ[i] <- mk_test_MeanQ$p.value[1]
  if(mk_test_MeanQ$estimates[3] < 0) {
    ws_110300KS$tau_MeanQ[i] <- "-" }
  if(mk_test_MeanQ$estimates[3] > 0) {
    ws_110300KS$tau_MeanQ[i] <- "+" }
  ws_110300KS$p_Q50[i] <- mk_test_Q50$p.value[1]
  if(mk_test_Q50$estimates[3] < 0) {
    ws_110300KS$tau_Q50[i] <- "-" }
  if(mk_test_Q50$estimates[3] > 0) {
    ws_110300KS$tau_Q50[i] <- "+" }
  ws_110300KS$p_Baseflow[i] <- mk_test_Baseflow$p.value[1]
  if(mk_test_Baseflow$estimates[3] < 0) {
    ws_110300KS$tau_Baseflow[i] <- "-" }
  if(mk_test_Baseflow$estimates[3] > 0) {
    ws_110300KS$tau_Baseflow[i] <- "+" }
  ws_110300KS$p_MAM7[i] <- mk_test_MAM7$p.value[1]
  if(mk_test_MAM7$estimates[3] == "NaN")
    ws_110300KS$tau_MAM7[i] <- "NA"
  if(!is.na(mk_test_MAM7$estimates[3]) < 0)
    ws_110300KS$tau_MAM7[i] <- "-"
  if(!is.na(mk_test_MAM7$estimates[3]) > 0)
    ws_110300KS$tau_MAM7[i] <- "+"
  ws_110300KS$p_Q90[i] <- mk_test_Q90$p.value[1]
  if(mk_test_Q90$estimates[3] < 0) {
    ws_110300KS$tau_Q90[i] <- "-" }
  if(mk_test_Q90$estimates[3] > 0) {
    ws_110300KS$tau_Q90[i] <- "+" }
  ws_110300KS$p_Q95[i] <- mk_test_Q95$p.value[1]
  if(mk_test_Q95$estimates[3] == "NaN") {
    ws_110300KS$tau_Q95[i] <- "NA" }
  if(!is.na(mk_test_Q95$estimates[3]) < 0) {
    ws_110300KS$tau_Q95[i] <- "-" }
  if(!is.na(mk_test_Q95$estimates[3]) > 0) {
    ws_110300KS$tau_Q95[i] <- "+" }

  
  # ws_110300KS$p_Q10[i] <- mk_test_Q10[2]
  # if(mk_test_Q10[6] < 0) {
  #   ws_110300KS$tau_Q10[i] <- "-" }
  # if(mk_test_Q10[6] > 0) {
  #   ws_110300KS$tau_Q10[i] <- "+" }
  # ws_110300KS$p_MeanQ[i] <- mk_test_MeanQ[2]
  # if(mk_test_MeanQ[6] < 0) {
  #   ws_110300KS$tau_MeanQ[i] <- "-" }
  # if(mk_test_MeanQ[6] > 0) {
  #   ws_110300KS$tau_MeanQ[i] <- "+" }
  # ws_110300KS$p_Q50[i] <- mk_test_Q50[2]
  # if(mk_test_Q50[6] < 0) {
  #   ws_110300KS$tau_Q50[i] <- "-" }
  # if(mk_test_Q50[6] > 0) {
  #   ws_110300KS$tau_Q50[i] <- "+" }
  # ws_110300KS$p_Baseflow[i] <- mk_test_Baseflow[2]
  # if(mk_test_Baseflow[6] < 0) {
  #   ws_110300KS$tau_Baseflow[i] <- "-" }
  # if(mk_test_Baseflow[6] > 0) {
  #   ws_110300KS$tau_Baseflow[i] <- "+" }
  # ws_110300KS$p_MAM7[i] <- mk_test_MAM7[2]
  # if(mk_test_MAM7[6] == "NaN")
  #   ws_110300KS$tau_MAM7[i] <- "NA"
  # if(!is.na(mk_test_MAM7[6]) < 0)
  #   ws_110300KS$tau_MAM7[i] <- "-"
  # if(!is.na(mk_test_MAM7[6]) > 0)
  #   ws_110300KS$tau_MAM7[i] <- "+"
  # ws_110300KS$p_Q90[i] <- mk_test_Q90[2]
  # if(mk_test_Q90[6] < 0) {
  #   ws_110300KS$tau_Q90[i] <- "-" }
  # if(mk_test_Q90[6] > 0) {
  #   ws_110300KS$tau_Q90[i] <- "+" }
  # ws_110300KS$p_Q95[i] <- mk_test_Q95[2]
  # if(mk_test_Q95[6] == "NaN") {
  #   ws_110300KS$tau_Q95[i] <- "NA" }
  # if(!is.na(mk_test_Q95[6]) < 0) {
  #   ws_110300KS$tau_Q95[i] <- "-" }
  # if(!is.na(mk_test_Q95[6]) > 0) {
  #   ws_110300KS$tau_Q95[i] <- "+" }
  # 
  

  # ws_180102CA$p_Q10[i] <- mk_test_Q10$p.value[1]
  # if(mk_test_Q10$estimates[3] < 0)
  #   ws_180102CA$tau_Q10[i] <- "-"
  # if(mk_test_Q10$estimates[3] > 0)
  #   ws_180102CA$tau_Q10[i] <- "+"
  # ws_180102CA$p_MeanQ[i] <- mk_test_MeanQ$p.value[1]
  # if(mk_test_MeanQ$estimates[3] < 0)
  #   ws_180102CA$tau_MeanQ[i] <- "-"
  # if(mk_test_MeanQ$estimates[3] > 0)
  #   ws_180102CA$tau_MeanQ[i] <- "+"
  # ws_180102CA$p_Q50[i] <- mk_test_Q50$p.value[1]
  # if(mk_test_Q50$estimates[3] < 0)
  #   ws_180102CA$tau_Q50[i] <- "-"
  # if(mk_test_Q50$estimates[3] > 0)
  #   ws_180102CA$tau_Q50[i] <- "+"
  # ws_180102CA$p_Baseflow[i] <- mk_test_Baseflow$p.value[1]
  # if(mk_test_Baseflow$estimates[3] < 0)
  #   ws_180102CA$tau_Baseflow[i] <- "-"
  # if(mk_test_Baseflow$estimates[3] > 0)
  #   ws_180102CA$tau_Baseflow[i] <- "+"
  # ws_180102CA$p_MAM7[i] <- mk_test_MAM7$p.value[1]
  # if(mk_test_MAM7$estimates[3] < 0)
  #   ws_180102CA$tau_MAM7[i] <- "-"
  # if(mk_test_MAM7$estimates[3] > 0)
  #   ws_180102CA$tau_MAM7[i] <- "+"
  # ws_180102CA$p_Q90[i] <- mk_test_Q90$p.value[1]
  # if(mk_test_Q90$estimates[3] < 0)
  #   ws_180102CA$tau_Q90[i] <- "-"
  # if(mk_test_Q90$estimates[3] > 0)
  #   ws_180102CA$tau_Q90[i] <- "+"
  # ws_180102CA$p_Q95[i] <- mk_test_Q95$p.value[1]
  # if(mk_test_Q95$estimates[3] < 0)
  #   ws_180102CA$tau_Q95[i] <- "-"
  # if(mk_test_Q95$estimates[3] > 0)
  #   ws_180102CA$tau_Q95[i] <- "+"
  
  
  
  ## SAVE CSV of streamgage metrics, CHOOSE watershed before running for loop --> in the future these should write to SQL database
  # write.csv(sf_metrics, paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/sfmetrics_MI/MI_hd/MI_", i, "_metrics_hd.csv"))
  # write.csv(sf_metrics, paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/sfmetrics_KS/KS_hd/KS_", i, "_metrics_hd.csv"))
  # write.csv(sf_metrics, paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/sfmetrics_CA/CA_hd/CA_", i, "_metrics_hd.csv"))
  
  # write.csv(sf_metrics, paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/sfmetrics_MI/MI_long/MI_", i, "_metrics_long.csv"))
  # write.csv(sf_metrics, paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/sfmetrics_KS/KS_long/KS_", i, "_metrics_long.csv"))
  # write.csv(sf_metrics, paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/sfmetrics_CA/CA_long/CA_", i, "_metrics_long.csv"))
  
  # write.csv(sf_metrics, paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/sfmetrics_MI/MI_ep/MI_", i, "_metrics_ep.csv"))
  # write.csv(sf_metrics, paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/sfmetrics_KS/KS_ep/KS_", i, "_metrics_ep.csv"))
  # write.csv(sf_metrics, paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/sfmetrics_CA/CA_ep/CA_", i, "_metrics_ep.csv"))
  
  
 ## ALIGN "Year" column so that NA years are preserved for plotting, CHOOSE watershed & time frame before running for loop
  # sf_metrics <- left_join(sf_metrics_year_MI_hd, sf_metrics, by = "hyear")
  sf_metrics <- left_join(sf_metrics_year_KS_hd, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_CA_hd, sf_metrics, by = "hyear")
  
  # sf_metrics <- left_join(sf_metrics_year_MI_long, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_KS_long, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_CA_long, sf_metrics, by = "hyear")
  
  # sf_metrics <- left_join(sf_metrics_year_MI_ep, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_KS_ep, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_CA_ep, sf_metrics, by = "hyear")

# }
  

  # ## PREPARE resulting metrics for plotting
  # # ## EXCLUDE Q50 and Q95 from plotting to reduce clutter, then melt dataframe
  # # sf_metrics_melt <- sf_metrics[ , c(1,2,3,5,6,7)] 
  # ## Only plot Q10, baseflow, and Q90 to reduce clutter, then melt dataframe
  # sf_metrics_melt <- sf_metrics[ , c(1,2,5,7)]
  # # ## Only plot Q10, mean Q, and Q90 to reduce clutter, then melt dataframe
  # # sf_metrics_melt <- sf_metrics[ , c(1,2,3,6)]
  # 
  # sf_metrics_melt <- reshape2::melt(sf_metrics_melt, measure.vars = 2:ncol(sf_metrics_melt), variable.name = "Metric", value.name = "Discharge")
  # colnames(sf_metrics_melt) <- c("Year", "Metric", "Discharge")
  # # sf_metrics_melt <- data_melt[!is.na(data_melt$Discharge), ]
  # 
  # ## Only plot Diff and Diffma, then melt dataframe
  # sf_metrics_melt2 <- sf_metrics[ , c(1,9,11)]
  # 
  # sf_metrics_melt2 <- reshape2::melt(sf_metrics_melt2, measure.vars = 2:ncol(sf_metrics_melt2), variable.name = "Metric", value.name = "Value")
  # colnames(sf_metrics_melt2) <- c("Year", "Metric", "Value")
  # 

#   ## SAVE plot of streamgage metrics, CHOOSE watershed before running for loop
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_MI/MI_hd/MI_", i, "_metrics_hd.png", sep = ""), width = 757, height = 464, unit = "px")
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_KS/KS_hd/KS_", i, "_metrics_hd.png", sep = ""), width = 757, height = 464, unit = "px")
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_KS/KS_hd_metric_3metrics_Diff/KS_", i, "_metrics_hd.png", sep = ""), width = 757, height = 464, unit = "px")
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_CA/CA_hd/CA_", i, "_metrics_hd.png", sep = ""), width = 757, height = 464, unit = "px")
#   
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_MI/MI_long/MI_", i, "_metrics_long.png", sep = ""), width = 757, height = 464, unit = "px")
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_KS/KS_long/KS_", i, "_metrics_long.png", sep = ""), width = 787, height = 464, unit = "px")
#   png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_KS/KS_long_metric_3metrics_Diff/KS_", i, "_metrics_long.png", sep = ""), width = 757, height = 464, unit = "px")
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_CA/CA_long/CA_", i, "_metrics_long.png", sep = ""), width = 757, height = 464, unit = "px")
#   
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_MI/MI_ep/MI_", i, "_metrics_ep.png", sep = ""), width = 757, height = 464, unit = "px")
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_KS/KS_ep/KS_", i, "_metrics_ep.png", sep = ""), width = 757, height = 464, unit = "px")
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_KS/KS_ep_metric_3metrics_Diff/KS_", i, "_metrics_hd.png", sep = ""), width = 757, height = 464, unit = "px")
#   # png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/sfmetrics_CA/CA_ep/CA_", i, "_metrics_ep.png", sep = ""), width = 757, height = 464, unit = "px")
#   
# # ## PLOT 5 metrics  
# # p_metrics_melt <- ggplot2::ggplot(sf_metrics_melt, aes(x = Year, y = Discharge, color = Metric, linetype = Metric, size = Metric)) +
# #     geom_ribbon(data = sf_metrics, mapping = aes(x = hyear, ymin = Q90, ymax = Q10), fill = "grey80", inherit.aes = FALSE) +
# #     geom_line() +
# #     scale_color_manual(values = c("black", "deepskyblue3", "royalblue", "orchid", "black")) +
# #     scale_linetype_manual(values = c(1, 1, 1, 1, 1)) +
# #     scale_size_manual(values = c(0.5, 1.1, 1.1, 1.1, 0.5)) +
# #     scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
# #     # scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
# #     scale_x_continuous(breaks = seq(1900, 2020, by = 20)) +
# #     # scale_x_continuous(breaks = seq(1900, 1940, by = 10)) +
# #     # ggtitle(paste(site_name, " METRICS")) +
# #     labs(title = stringr::str_wrap(site_name, 30)) +
# #     theme(axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30), axis.text = element_text(size = 34), plot.title = element_text(size = 32), legend.position = "none") +
# #     ylab(bquote("Discharge " (ft^3/s)))
# 
#   ## PLOT 3 metrics (p1) + Q90Q10 spread (p2) ---> CHANGE x-axis scaling for hd, long, or ep; REMOVE geom_vline in p1 if not long data
#   p1 <- ggplot2::ggplot(sf_metrics_melt, aes(x = Year, y = Discharge, color = Metric, linetype = Metric, size = Metric)) +
#     geom_ribbon(data = sf_metrics, mapping = aes(x = hyear, ymin = Q90, ymax = Q10), fill = "grey80", inherit.aes = FALSE) +
#     geom_line() +
#     geom_vline(xintercept = 1963, linetype = 2) +  ## long
#     scale_color_manual(values = c("black", "royalblue", "black", "darkcyan")) +
#     scale_linetype_manual(values = c(1, 1, 1, 1)) +
#     scale_size_manual(values = c(0.5, 1, 0.5, 1)) +
#     scale_y_log10(labels = scales::number_format(accuracy = 0.001)) +
#     # scale_x_continuous(breaks = seq(1955, 2025, by = 10)) +  ## hd
#     scale_x_continuous(breaks = seq(1905, 2025, by = 20)) +   ## long
#     # scale_x_continuous(breaks = seq(1905, 1945, by = 10)) +   ## ep
#     labs(title = stringr::str_wrap(site_name, 33)) +
#     theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 26), axis.text = element_text(size = 22, hjust = 1), plot.title = element_text(size = 28), legend.title = element_text(size=22), legend.text = element_text(size = 20)) +
#     ylab(bquote("Discharge " (m^3/s)))
#   p2 <- ggplot2::ggplot(sf_metrics_melt2, aes(x = Year, y = Value, color = Metric, linetype = Metric, size = Metric)) +
#     geom_line() +
#     geom_vline(xintercept = 1963, linetype = 2) +  ## long
#     scale_color_manual(values = c("darkcyan", "tan4")) +
#     scale_linetype_manual(values = c(1, 1)) +
#     scale_size_manual(values = c(1, 1)) +
#     # scale_x_continuous(breaks = seq(1955, 2025, by = 10)) +  ## hd
#     scale_x_continuous(breaks = seq(1905, 2025, by = 20)) +   ## long
#     theme (axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 22, hjust = 1), legend.title = element_blank(), legend.text = element_text(size = 20))
#   
#   ## COMBINE p1 & p2 
#   pcombo <- p1 + p2 + plot_layout(nrow = 2, heights = c(3, 1), guides = "collect")
#   
#   ## WRITE plot to local device
#   print(pcombo)
# 
#   ## CLOSE PNG connection
#   dev.off()
  
  ## PLOT metrics
  # p_metrics <- ggplot2::ggplot(sf_metrics, aes(x = hyear)) +
  #   geom_line(aes(y = Q10)) +
  #   # geom_line(aes(y = Baseflow)) +
  #   # geom_line(aes(y = Q90)) +
  #   # geom_ribbon(mapping = aes(x = hyear, ymin = Q90, ymax = Q10), fill = "grey80", inherit.aes = FALSE) +
  #   scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
  #   scale_x_continuous(breaks = seq(1955, 2025, by = 10)) +
  #   # scale_x_continuous(breaks = seq(1905, 2025, by = 20)) +
  #   # scale_x_continuous(breaks = seq(1905, 1945, by = 10)) +
  #   # ggtitle(paste(site_name, " METRICS")) +
  #   labs(title = stringr::str_wrap(site_name, 30)) +
  #   theme(axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30), axis.text = element_text(size = 28, hjust = 1), plot.title = element_text(size = 32), legend.position = "none") +
  #   ylab(bquote("Discharge " (ft^3/s)))
  # print(p_metrics)
  
}

## CALCUATE percent of the time flow exceeds 10 m3/s 
# sf_metrics_select <- sf_metrics[1:61, ]
# sf_metrics_select <- sf_metrics[62:nrow(sf_metrics), ]
# nrow(sf_metrics_select[(sf_metrics_select$Q10 >= 10),])
# nrow(sf_metrics[(sf_metrics$Q10 >= 10),])
# nrow(sf_metrics)
 

## SAVE resulting watershed dataframe to dataframe
ws_040500MI_hd <- ws_040500MI
ws_110300KS_mhd_mk <- ws_110300KS
ws_180102CA_hd <- ws_180102CA

ws_040500MI_long <- ws_040500MI
ws_110300KS_mlong_mk <- ws_110300KS
ws_180102CA_long <- ws_180102CA

ws_040500MI_ep <- ws_040500MI
ws_110300KS_ep <- ws_110300KS
ws_180102CA_ep <- ws_180102CA

## SAVE CSV of watershed data
write.csv(ws_040500MI, "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_040500MI_hd_1960-2021.csv")
write.csv(ws_110300KS, "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_110300KS_mhd_1963-2021_mk.csv")
write.csv(ws_180102CA,"C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_180102CA_hd_1962-2021.csv")

write.csv(ws_040500MI, "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_040500MI_long.csv")
write.csv(ws_110300KS, "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_110300KS_mlong_mk.csv")
write.csv(ws_180102CA,"C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_180102CA_long.csv")

write.csv(ws_040500MI, "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_040500MI_ep.csv")
write.csv(ws_110300KS, "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_110300KS_ep.csv")
write.csv(ws_180102CA,"C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/outputs/watershed_info/ws_180102CA_ep.csv")




