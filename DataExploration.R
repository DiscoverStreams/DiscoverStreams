######################################################################################
##### COUNT STATIONS WITH DATA OVER TIME ######
## SELECT streamflow object to work with and RESET as needed without re-running for loops
## SAVE resulting streamflow object to dataframe
count_040500MI <- sf_040500MI_all
count_110300KS <- sf_110300KS_all
count_180102CA <- sf_180102CA_all


## CREATE column for count of gages with data on that day
count_040500MI$Count <- apply(!is.na(count_040500MI), 1, sum)
count_110300KS$Count <- apply(!is.na(count_110300KS), 1, sum)
count_180102CA$Count <- apply(!is.na(count_180102CA), 1, sum)

## CREATE column for % of total watershed gages with data on that day
count_040500MI$Percent <- round((count_040500MI$Count/ncol(count_040500MI)) * 100, 1)
count_110300KS$Percent <- round((count_110300KS$Count/ncol(count_110300KS)) * 100, 1)
count_180102CA$Percent <- round((count_180102CA$Count/ncol(count_180102CA)) * 100, 1)

## ADD watershed id column
count_040500MI$Watershed <- "SE Lake Michigan (040500)"
count_110300KS$Watershed <- "Middle Arkanasas (110300)"
count_180102CA$Watershed <- "Klamath (180102)"

## SUBSET to select only when percent is > 50%
# count_040500MI <- count_040500MI[count_040500MI$Percent >= 50, ]
# count_110300KS <- count_110300KS[count_110300KS$Percent >= 50, ]
# count_180102CA <- count_180102CA[count_180102CA$Percent >= 50, ]

## CALCULATE minimum year when at least 50% of stations have data
# count_040500MI_min <- min(count_040500MI$Date)
# count_110300KS_min <- min(count_110300KS$Date)
# count_180102CA_min <- min(count_180102CA$Date)

## SUBSET to select only columns needed for plots - Date, Percent, Watershed
count_040500MI <- subset(count_040500MI, select = c(1, length(count_040500MI)-1, length(count_040500MI)))
count_110300KS <- subset(count_110300KS, select = c(1, length(count_110300KS)-1, length(count_110300KS)))
count_180102CA <- subset(count_180102CA, select = c(1, length(count_180102CA)-1, length(count_180102CA)))

## COMBINE watersheds for plotting
sf_count <- rbind(count_040500MI, count_110300KS)
sf_count <- rbind(sf_count, count_180102CA)

## SCATTER PLOT year vs. % data available in watershed
ggplot(sf_count, aes(x = Date, y = Percent, color = Watershed)) + 
  geom_point() +
  scale_color_manual(values = c("plum4", "tan3", "springgreen4")) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  ylab("Percent of Stations in Watershed with Data") +
  # geom_vline(xintercept = as.Date("1951-10-01"), color = "black") +
  geom_vline(xintercept = as.Date(count_040500MI_min), color = "plum4") +
  geom_vline(xintercept = as.Date(count_110300KS_min), color = "tan3") +
  geom_vline(xintercept = as.Date(count_180102CA_min), color = "springgreen4")

ggplot(count_110300KS, aes(x = Date, y = Percent, color = Watershed)) + 
  geom_point() +
  scale_color_manual(values = c("tan3")) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  ylab("Percent of Stations in Watershed with Data") 



##############################################################################
##### CALCULATE PERCENT DATA & PLOT HEATMAPS FOR EACH STATION OVER TIME ######
## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- "1959-10-01"  # Michigan HD data
start_date <- "1962-10-01"  # Kansas HD data
start_date <- "1961-10-01"  # California HD data
start_date <- "1951-10-01"  # All 1951 data
start_date <- ""  # Long and EP data
end_date <- ""  # Long data
end_date <- "2022-09-30" # HD and 1951 data
end_date <- "1940-09-30" # EP data

## SET watershed dataframe for loop
ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_mhd_mmk 
ws_180102CA <- ws_180102CA_long

## CREATE dataframe for percent of days with data
percentDataMI <- data.frame()
percentDataKS <- percentDataKS_hd
percentDataCA <- data.frame()
# percentData_all <- data.frame()

## RUN code within for loop for i = 1, then run for loop for i >= 2 
i = 1

## CHOOSE watershed before running for loop-- SET start date and end date before running for loop
# for (i in 2:nrow(ws_040500MI)) {
#   site_number <- ws_040500MI$site_no[i]
  for (i in 2:nrow(ws_110300KS)) {
    site_number <- ws_110300KS$site_no[i]
  # for (i in 2:nrow(ws_180102CA)) {
  #   site_number <- ws_180102CA$site_no[i]
  
  
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
  # percentDataMI <- rbind(percentDataMI, percentData)
  percentDataKS <- rbind(percentDataKS, percentData)
  # percentDataCA <- rbind(percentDataCA, percentData)
  
}

## SAVE resulting dataframes for percent of days with data
percentDataMI_1958_2021_sel <- percentDataMI
percentDataKS_hd <- percentDataKS
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
  theme(panel.grid.major.x = element_line(size = 0.75), panel.grid.minor = element_line(size = 0.1), axis.title.x = element_text(size = 28), axis.title.y = element_text(size = 28), axis.text = element_text(size = 20), legend.text = element_text(size = 26), legend.title = element_text(size = 26)) + 
  geom_vline(xintercept = 1963, color = "black", linetype = 2)

ggplot(percentDataCA, aes(x = Year, y = SiteName, fill = PercentData)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "plum4") +
  scale_x_continuous(breaks = seq(1900, 2022, 20), minor_breaks = seq(1900, 2022, 5)) +
  theme(panel.grid.major.x = element_line(size = 0.75), panel.grid.minor = element_line(size = 0.1)) +
  geom_vline(xintercept = 1964, color = "black")
