# data manipulation libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)
# data retrieval libraries
library(dataRetrieval)


## Gap fill Dodge City gage station with gage height values since discharge parameter hasn't been measured since 2007
## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00065")
parameter_names <- c("Gage height, feet")
start_date <- ""    
end_date <- "2021-09-30"
site_number <- huc110300KS_ws_all[5, 1]

raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)

df <- huc110300KS_sf_all
df[`ARKANSAS R AT DODGE CITY, KS`][huc110300KS_sf_all[`ARKANSAS R AT DODGE CITY, KS`] == NA] <- 0

#################################################################################
##### PREPARE WATERSHED DATAFRAMES WITH COLUMNS FOR STATS & ANALYTIC VALUES #####
## SET watershed object to work with 
huc040500MI_ws <- huc040500MI_ws_long
huc110300KS_ws <- huc110300KS_ws_long
huc180102CA_ws <- huc180102CA_ws_long

## ADD column id for indexing --- ESSENTIAL FOR MAPPING
huc040500MI_ws$id <- 1:nrow(huc040500MI_ws)
huc110300KS_ws$id <- 1:nrow(huc110300KS_ws)
huc180102CA_ws$id <- 1:nrow(huc180102CA_ws)

## ADD columns for p and tau values for each station from Mann-Kendall Test on MAM7
huc040500MI_ws$p_MAM7 <- NA
huc040500MI_ws$tau_MAM7 <- NA
huc110300KS_ws$p_MAM7 <- NA
huc110300KS_ws$tau_MAM7 <- NA
huc180102CA_ws$p_MAM7 <- NA
huc180102CA_ws$tau_MAM7 <- NA

## ADD columns for p and tau values for each station from Mann-Kendall Test on Mean Baseflow
huc040500MI_ws$p_Baseflow <- NA
huc040500MI_ws$tau_Baseflow <- NA
huc110300KS_ws$p_Baseflow <- NA
huc110300KS_ws$tau_Baseflow <- NA
huc180102CA_ws$p_Baseflow <- NA
huc180102CA_ws$tau_Baseflow <- NA

## ADD columns for p and tau values for each station from Mann-Kendall Test on Mean Discharge (Q)
huc040500MI_ws$p_MeanQ <- NA
huc040500MI_ws$tau_MeanQ <- NA
huc110300KS_ws$p_MeanQ <- NA
huc110300KS_ws$tau_MeanQ <- NA
huc180102CA_ws$p_MeanQ <- NA
huc180102CA_ws$tau_MeanQ <- NA

## ADD columns for p and tau values for each station from Mann-Kendall Test on Q10
huc040500MI_ws$p_Q10 <- NA
huc040500MI_ws$tau_Q10 <- NA
huc110300KS_ws$p_Q10 <- NA
huc110300KS_ws$tau_Q10 <- NA
huc180102CA_ws$p_Q10 <- NA
huc180102CA_ws$tau_Q10 <- NA

## ADD columns for p and tau values for each station from Mann-Kendall Test on Q50
huc040500MI_ws$p_Q50 <- NA
huc040500MI_ws$tau_Q50 <- NA
huc110300KS_ws$p_Q50 <- NA
huc110300KS_ws$tau_Q50 <- NA
huc180102CA_ws$p_Q50 <- NA
huc180102CA_ws$tau_Q50 <- NA

## ADD columns for p and tau values for each station from Mann-Kendall Test on Q90
huc040500MI_ws$p_Q90 <- NA
huc040500MI_ws$tau_Q90 <- NA
huc110300KS_ws$p_Q90 <- NA
huc110300KS_ws$tau_Q90 <- NA
huc180102CA_ws$p_Q90 <- NA
huc180102CA_ws$tau_Q90 <- NA

## ADD columns for p and tau values for each station from Mann-Kendall Test on Q95
huc040500MI_ws$p_Q95 <- NA
huc040500MI_ws$tau_Q95 <- NA
huc110300KS_ws$p_Q95 <- NA
huc110300KS_ws$tau_Q95 <- NA
huc180102CA_ws$p_Q95 <- NA
huc180102CA_ws$tau_Q95 <- NA


## SAVE resulting watershed object 
huc040500MI_ws_long <- huc040500MI_ws
huc110300KS_ws_long <- huc110300KS_ws
huc180102CA_ws_long <- huc180102CA_ws


######################################################################################
##### COUNT STATIONS WITH DATA OVER TIME ######
## CREATE streamflow object to work with and RESET as needed without re-running for loops
sf040500MI <- huc040500MI_sf_all
sf110300KS <- huc110300KS_sf_all
sf180102CA <- huc180102CA_sf_all

## CREATE column for count of gages with data on that day
sf040500MI$Count <- apply(!is.na(sf040500MI), 1, sum)
sf110300KS$Count <- apply(!is.na(sf110300KS), 1, sum)
sf180102CA$Count <- apply(!is.na(sf180102CA), 1, sum)

## CREATE column for % of total watershed gages with data on that day
sf040500MI$Percent <- round((sf040500MI$Count/ncol(huc040500MI_ws)) * 100, 1)
sf110300KS$Percent <- round((sf110300KS$Count/ncol(huc110300KS_ws)) * 100, 1)
sf180102CA$Percent <- round((sf180102CA$Count/ncol(huc180102CA_ws)) * 100, 1)

## ADD watershed id column
sf040500MI$Watershed <- "St. Joseph (040500)"
sf110300KS$Watershed <- "Middle Arkanasas (110300)"
sf180102CA$Watershed <- "Klamath (180102)"

## SUBSET to select only columns needed for plots - Date, Percent, Watershed
sf040500MI <- subset(sf040500MI, select = c(1, length(sf040500MI)-1, length(sf040500MI)))
sf110300KS <- subset(sf110300KS, select = c(1, length(sf110300KS)-1, length(sf110300KS)))
sf180102CA <- subset(sf180102CA, select = c(1, length(sf180102CA)-1, length(sf180102CA)))

## COMBINE watersheds for plotting
sf_count <- rbind(sf040500MI, sf110300KS)
sf_count <- rbind(sf_count, sf180102CA)

## SCATTER PLOT year vs. % data available in watershed
ggplot(sf180102CA, aes(x = Date, y = Percent, color = Watershed)) + 
  geom_point() +
  scale_color_manual(values = c("tan3")) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  ylab("Percent of Stations in Watershed with Data") + 
  geom_vline(xintercept = as.Date("1951-10-01"), color = "black")

##############################################################################
##### CALCULATE PERCENT DATA & PLOT HEATMAPS FOR EACH STATION OVER TIME ######
## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- "1958-10-01"    
end_date <- "2021-09-30"

## SET watershed dataframe for loop
huc040500MI_ws <- huc040500MI_ws_long
huc110300KS_ws <- huc110300KS_ws_long 
huc180102CA_ws <- huc180102CA_ws_long

## CREATE dataframe for percent of days with data
percentDataMI <- data.frame()
percentDataKS <- data.frame()
percentDataCA <- data.frame()
# percentData_all <- data.frame()

## RUN code within for loop for i = 1, then run for loop for i >= 2 
i = 1
  
## CHOOSE watershed before running for loop
for (i in 2:nrow(huc040500MI_ws)) {
  site_number <- huc040500MI_ws$site_no[i]
# for (i in 2:nrow(huc110300KS_ws)) {
#   site_number <- huc110300KS_ws$site_no[i]
# for (i in 2:nrow(huc180102CA_ws)) {
#   site_number <- huc180102CA_ws$site_no[i]
  
  
  ## RETRIEVE site info and streamflow data for gage station
  site_info <- dataRetrieval::readNWISsite(site_number)
  site_name <- site_info$station_nm
  
  raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)
  raw_daily <- subset(raw_daily, select = c(3,4))
  raw_daily$Year <- as.numeric(format(raw_daily$Date, "%Y"))
  
  
  ## CALCULATE number of days with data at this station
  countData <- raw_daily %>% 
    group_by(Year) %>% 
    tally()
  
  ## CALCULATE percent of days in the year with data at this station
  percentData <- data.frame(countData$Year, round((countData[ , 2]/365) * 100))
  
  ## REFORMAT dataframe for joining
  percentData$SiteName <- site_name
  colnames(percentData) <- c("Year", "PercentData", "SiteName")
  
  ## only run for i = 1 to prime dataframe for joining, then comment out and run for loop
  # percentDataMI <- percentData
  # percentDataKS <- percentData
  # percentDataCA <- percentData
  
  ## RUN for i >=2, JOIN station data to watershed dataframe
  percentDataMI <- rbind(percentDataMI, percentData)
  # percentDataKS <- rbind(percentDataKS, percentData)
  # percentDataCA <- rbind(percentDataCA, percentData)

}

## SAVE resulting dataframes for percent of days with data
percentDataMI_1958_2021_sel <- percentDataMI
percentDataKS_long <- percentDataKS
percentDataCA_long <- percentDataCA

## PLOT tiled heatmap of percent data for year by gage station
ggplot(percentDataMI, aes(x = Year, y = SiteName, fill = PercentData)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "springgreen4") +
  scale_x_continuous(breaks = seq(1900, 2022, 20), minor_breaks = seq(1900, 2022, 5)) +
  theme(panel.grid.major.x = element_line(size = 0.75), panel.grid.minor = element_line(size = 0.1)) +
  geom_vline(xintercept = 1965, color = "black")
  
ggplot(percentDataKS, aes(x = Year, y = SiteName, fill = PercentData)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "tan4") +
  scale_x_continuous(breaks = seq(1900, 2022, 20), minor_breaks = seq(1900, 2022, 5)) +
  theme(panel.grid.major.x = element_line(size = 0.75), panel.grid.minor = element_line(size = 0.1)) +
  geom_vline(xintercept = 1963, color = "black")

ggplot(percentDataCA, aes(x = Year, y = SiteName, fill = PercentData)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "plum4") +
  scale_x_continuous(breaks = seq(1900, 2022, 20), minor_breaks = seq(1900, 2022, 5)) +
  theme(panel.grid.major.x = element_line(size = 0.75), panel.grid.minor = element_line(size = 0.1)) +
  geom_vline(xintercept = 1964, color = "black")
