# data manipulation libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)
# data retrieval libraries
library(dataRetrieval)


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
  ylab("Percent of Stations in Watershed with Data") +
  geom_vline(xintercept = as.Date("1951-10-01"), color = "black")

###############################################################################

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
  
  
  # ## START loop at i = 2 because i = 1 is column "Date"
  # i = 2
  # 
  # ## UN-COMMENT corresponding watershed lines and save location before running for loop
  # # for (i in 2:ncol(huc040500MI_sf)) {
  # #   site_name <- colnames(huc040500MI_sf[i])
  # #   sf_select <- data.frame(huc040500MI_sf[[1]], huc040500MI_sf[[i]])
  # for (i in 2:ncol(huc110300KS_sf)) {
  #   site_name <- colnames(huc110300KS_sf[i])
  #   sf_select <- data.frame(huc110300KS_sf[[1]], huc110300KS_sf[[i]])
  # # for (i in 2:ncol(huc180102CA_sf)) {
  # #   site_name <- colnames(huc180102CA_sf[i])
  # #   sf_select <- data.frame(huc180102CA_sf[[1]], huc180102CA_sf[[i]])
  # 
  #   ## PREPARE dataframe for calculations
  #   colnames(sf_select) <- c("Date", "Discharge")
  #   sf_select$Year <- as.numeric(format(sf_select[[1]], "%Y"))
  
    
    
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
