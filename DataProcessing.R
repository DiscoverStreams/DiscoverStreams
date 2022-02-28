# data manipulation libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)
# database connection/retrieval libraries
library(dataRetrieval)

## Run once, no need to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- ""
end_date <- ""

## CREATE streamflow object to work with and RESET as needed without re-running for loops
sf040500MI <- huc040500MI_sf
sf110300KS <- huc110300KS_sf
sf180102CA <- huc180102CA_sf

## CREATE column for count of gages with data on that day
sf040500MI$Count <- apply(!is.na(sf040500MI), 1, sum)
sf110300KS$Count <- apply(!is.na(sf110300KS), 1, sum)
sf180102CA$Count <- apply(!is.na(sf180102CA), 1, sum)

## CREATE column for % of total watershed gages with data on that day
sf040500MI$Percent <- round((sf040500MI$Count/nrow(huc040500MI_ws)) * 100, 1)
sf110300KS$Percent <- round((sf110300KS$Count/nrow(huc110300KS_ws)) * 100, 1)
sf180102CA$Percent <- round((sf180102CA$Count/nrow(huc180102CA_ws)) * 100, 1)

## ADD watershed id column
sf040500MI$Watershed <- "St. Joseph (040500)"
sf110300KS$Watershed <- "Middle Arkanasas (110300)"
sf180102CA$Watershed <- "Klamath (180102)"

## SUBSET to select only columns needed for plots - Date, Count, Watershed
sf040500MI <- subset(sf040500MI, select = c(1, 55, 56))
sf110300KS <- subset(sf110300KS, select = c(1, 33, 34))
sf180102CA <- subset(sf180102CA, select = c(1, 35, 36))

## COMBINE watersheds for plotting
sf_count <- rbind(sf040500MI, sf110300KS)
sf_count <- rbind(sf_count, sf180102CA)

## SCATTER PLOT year vs. % data available in watershed
ggplot(sf_count, aes(x = Date, y = Percent, color = Watershed)) + 
  geom_point() +
  scale_color_manual(values = c("plum4", "tan3", "springgreen4")) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  ylab("Percent of Stations in Watershed with Data")

##############################################

## CREATE dataframe for percent of days with data
percentDataMI <- data.frame()
percentDataKS <- data.frame()
percentDataCA <- data.frame()
# percentData_all <- data.frame()

i = 1
# for (i in 2:nrow(huc040500MI_ws)) {
# for (i in 2:nrow(huc110300KS_ws)) {
for (i in 2:nrow(huc180102CA_ws)) {
  
  ## CHOOSE watershed before running for loop
  # site_number <- huc040500MI_ws$site_no[i]
  # site_number <- huc110300KS_ws$site_no[i]
  site_number <- huc180102CA_ws$site_no[i]
  
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
  
  ## JOIN station data to watershed dataframe
  # percentDataMI <- rbind(percentDataMI, percentData)
  # percentDataKS <- rbind(percentDataKS, percentData)
  percentDataCA <- rbind(percentDataCA, percentData)

}


## PLOT tiled heatmap of percent data for year by gage station
ggplot(percentDataMI, aes(x = Year, y = SiteName, fill = PercentData)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "springgreen4") +
  # scale_x_discrete(breaks = c("1900", "1910", "1920", "1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000", "2010", "2022")) +
  theme(panel.grid.major = element_line(size = 1), panel.grid.minor = element_line(size = 0.5)) +
  geom_vline (xintercept = 1965, color = "black")
  
ggplot(percentDataKS, aes(x = Year, y = SiteName, fill = PercentData)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "tan4") +
  theme(panel.grid.major = element_line(size = 1), panel.grid.minor = element_line(size = 0.5)) +
  geom_vline (xintercept = 1965, color = "black")

ggplot(percentDataCA, aes(x = Year, y = SiteName, fill = PercentData)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "plum4") +
  theme(panel.grid.major = element_line(size = 1), panel.grid.minor = element_line(size = 0.5)) +
  geom_vline (xintercept = 1965, color = "black")
