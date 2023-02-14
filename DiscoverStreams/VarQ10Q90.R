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

# data analysis/statistical libraries
library(xts)
library(lfstat)
library(trend)


### Source R script files from deprecated lfstat package
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/1NAs.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/2processes.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/3lfindices.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/4flowduration.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/5extreme.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/6streamdef.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/7multistation.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/hyear.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/import.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/lfobj.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/methods.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/misc.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/options.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/pooling.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/readsheets.R")
source("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/lfstat/recession.R")


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
ws_110300KS <- ws_110300KS_hd
ws_180102CA <- ws_180102CA_hd

ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_long
ws_180102CA <- ws_180102CA_long

ws_040500MI <- ws_040500MI_ep
ws_110300KS <- ws_110300KS_ep
ws_180102CA <- ws_180102CA_ep


########## STREAM DEPLETION METRICS ##########

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

## LOOP 1 - metrics calculated over entire time period -> RETRIEVE streamflow data to get dataframe without NAs -- NA's end up being coerced by createlfdataframe -- SET start date and end date before running for loop
i = 1

# for (i in 1:nrow(ws_040500MI)) {
#         site_number <- ws_040500MI$site_no[i]
for (i in 1:nrow(ws_110300KS)) {
  
  ## CALCULATE Q90-Q10 spread
  Q90Q10 <- Q90-Q10 
  
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
  # MAM7 <- MAM(sf, n=7, yearly = TRUE)
  # colnames(MAM7) <- c("hyear","MAM7")
  # MAM7 <- MAM7 %>% 
  #   summarise(MAM7 = mean(MAM7))
  # MAM7 <- str_sub(MAM7,1,-1) %>% 
  #   as.numeric(MAM7)
  
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
  # ws_110300KS$MAM7[i] <- as.numeric(MAM7)
  ws_110300KS$MAM7[i] <- NA
  ws_110300KS$Q90[i] <- as.numeric(Q90)
  ws_110300KS$Q95[i] <- as.numeric(Q95)
  
  # ws_180102CA$Q10[i] <- as.numeric(Q10)
  # ws_180102CA$MeanQ[i] <- as.numeric(meanQ)
  # ws_180102CA$Q50[i] <- as.numeric(Q50)
  # ws_180102CA$Baseflow[i] <- as.numeric(baseflow)
  # ws_180102CA$MAM7[i] <- as.numeric(MAM7)
  # ws_180102CA$Q90[i] <- as.numeric(Q90)
  # ws_180102CA$Q95[i] <- as.numeric(Q95)
  
}