# data manipulation libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(reshape2)
# database connection/retrieval libraries
library(readr)
library(odbc)
library(RMariaDB)
library(DBI)
library(dataRetrieval)
library(rnoaa)


## CONNECT to SQL database on UWaterloo server
## --> need mySQL but not exposed to internet 
con_UW <- odbc::dbConnect(odbc(),
                 Driver = "MySQL ODBC 8.0 Unicode Driver", 
                 Server = "interactiveviz.uwaterloo.ca",
                 Database = "interact_water",
                 UID = "webconnect",
                 PWD = "F-9hZ#Q8G}c{",
                 # PWD = rstudioapi::askForPassword("Database user password"),
                 Port = 3306)

## CONNECT to SQL database on KU server
con_KS <- dbConnect(odbc(),
                 Driver = "MySQL ODBC 8.0 Unicode Driver", 
                 Server = "itprdcpandb01.cc.ku.edu",
                 Database = "interact_water",
                 UID = "interact_HillRG", ##"interact_webuser"
                 PWD =  rstudioapi::askForPassword("Database user password"),
                 Port = 3306)


## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- ""    
end_date <- ""


### WATERSHED STATION INFO - all gage stations within each watershed ###
huc040500MI_NWISsites <- c("04100222", "04100500", "04101000", "04101370", "04101225", "04101500", "04099510", "04099750", "04098430", "040975296", "040975299", "04096767", "04097540", "04098620", "04098636", "04098980", "04099000", "04098757", "0409754132", "04097500", "04096590", "04096681", "04096970", "0409651496", "04096272", "04096312", "04096325", "04096340", "04096400", "04096405", "04096500", "04096515", "04096600", "04096900", "04097060", "04097170", "04097195", "04097200", "04097528", "040975299", "04097970", "04098000", "04098500", "04099610", "04099808", "04099850", "04100000", "04100252", "04100295", "04100377", "04100465", "041015313", "04101535", "04101590", "04101800", "04102000", "04102085", "04102148", "04102320", "04102420", "04102500", "04102533", "04102700", "04102776", "04108800", "04108801", "04108862", "04108872", "041027908", "04102798", "04102988", "04103490", "04103500", "041035285", "04104945", "04105000", "04105500", "04105700", "04106000", "04106500", "04106400", "04106320", "04108600", "04108645", "04108660", "04102850", "04103010", "04104000", "04104500", "04105800", "04106180", "04106190", "04106300", "04106906", "04107850", "04108000", "04108500", "04108670", "04109000", "04111000", "04112700", "04112850", "04111379", "04112000", "04112500", "04113000", "04114498", "04114000", "04109500", "04110000", "04111500", "04112904", "04113097", "04114500", "04115000", "04115265", "04116000", "04116500", "04118500", "04118564", "04119000", "04119055", "04119070", "04119160", "04119345", "04119365", "04119400", "04120194", "04119300", "04120250", "04117004", "04117500", "04118000", "04118105", "04117000")

# huc040500MI_counties <- c("Muskegon", "Montcalm", "Gratiot", "Ottawa", "Kent", "Ionia", "Clinton", "Shiawassee", "Allegan", "Barry", "Eaton", "Ingham", "Livingston", "Van Buren", "Kalamazoo", "Calhoun", "Jackson", "Berrien", "Cass", "St. Joseph", "Branch", "Hillsdale")
# huc040500IN_counties <- c("St. Joseph", "Elkhart", "LaGrange", "Steuben", "Noble")

huc110300KS_NWISsites <- c("07137000", "07137500", "07137010", "07138000", "07138020", "07138050", "375615101170800", "07138070", "07138064", "07138075", "07139000", "07137500", "07138062", "07138063", "07138065", "07138650", "07138660", "07139500", "07141220", "07141300", "07139800", "07140000", "07140500", "07141200", "07140700", "07140850", "07141000", "07140900", "07140885", "07140880", "07140890", "07141175", "07141780", "07141900", "07142019", "07141770", "07142015", "07142020", "07142300", "07142575", "07142620", "07142680", "07143330", "07143340", "07143350", "07143375", "07142800", "07143400", "07143300", "07143310", "07142860", "07142900", "07143665", "07143672", "07144050", "375348097262800", "07144100", "07144201", "07144200","07143600", "07143680", "07144000", "375350097262800", "07144300", "07144325", "07144330", "07144340", "07144480", "07144485", "07144486", "07144490", "07144550", "07144570", "07145600", "07145700", "07146500", "07144301", "07144470", "07144780", "07144790", "07144601", "07144660", "07144680", "07144730", "07144795", "07144800", "07145200", "07144850", "07144910", "07145500", "07146800", "07147070", "07146570", "07146623", "07146830", "07146895", "07146990", "07147050", "07147060", "07147190", "07147800", "07147900", "07147600")

huc180102CA_NWISsites <- c("11502500", "11502550", "11493500", "11492200", "11491400", "11492400", "11497550", "11501000", "11495800","11497500", "11499100", "421401121480900", "11507200", "11504270", "11504260", "423456121562900", "11504115", "11504290", "11503001", "11504000", "11504100", "11504107", "11505600", "11505700", "11507500", "11507501", "421015121471800", "11509105", "420853121505500", "420853121505501", "11509370", "420741121554001", "420448121503100", "11509200", "420450121504500", "420451121510000", "420219121474500", "420219121465200", "420218121455900", "420134121444800", "420029121461700", "420007121464200", "420009121485700", "420014121493200", "420036121333700", "415954121312100", "421010121271200", "420535121143800", "11483500", "11484000", "11486000", "11486990", "11488510", "11488700", "11507400", "11508500", "11509250", "11509340", "420524121515200", "420615121533600", "420615121533601", "420732121501100", "421131121465900", "421209121463000", "421209121463001", "421330121474700", "11489500", "11490000", "11490500", "11509500", "11510700", "11516528", "11516530", "11517800", "11520500", "11510000", "11510500", "11512500", "11512920", "11514500", "11516600", "14339400", "420523122042000", "420743121565400", "420903122010900", "11517500", "11517000", "11516750", "11516900", "11519500", "11517900", "11517950", "11518000", "11518050", "11518200", "11518300", "11518310", "11518600", "11519000", "11520000", "11520800", "11521500", "11522200", "11523000", "11530500", "11521000", "11522260", "11523000", "11523030", "11523050", "11530150", "11530300", "11522300", "11522400", "11522500", "11530000", "11529800", "11527000", "11526400", "11526250", "11525854", "11525670", "11525655", "11525530", "11525500", "11523200", "11523700", "11524000", "11525535", "11525540", "11525600", "11525630", "11525750", "11525800", "11525900", "11526000", "11526300", "11526500", "11527400", "11527500", "11528000", "11529500", "11530020", "11528700", "11528100", "11528200", "11528400", "11528440", "11528500", "11529000")


## RUN corresponding huc dataframe for each HUC06, also set HUC06 in for loop
huc040500MI <- data.frame()
huc110300KS <- data.frame()
huc180102CA <- data.frame()

## CLEAR site select data frame before running for loop for each HUC06 
site_select <- data.frame()

## CHANGE HUC08 name then RUN for loop
# for (i in seq_along(huc040500MI_NWISsites)) {
#   site_number <- huc040500MI_NWISsites[i]
  # for (i in seq_along(huc110300KS_NWISsites)) {
  #         site_number <- huc110300KS_NWISsites[i]
  for (i in seq_along(huc180102CA_NWISsites)) {
          site_number <- huc180102CA_NWISsites[i]
  
  site_info <- dataRetrieval::readNWISsite(site_number)
  # keep columns 2, 3, 7, 8, 24
  site_info <- site_info[ , c(2, 3, 7, 8, 24)]
  
  raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)
  if (length(raw_daily)==0)
    site_info$startDate <- NA
  if (length(raw_daily)==0)
    site_info$endDate <- NA
  if (length(raw_daily)==0)
    site_info$yearsAvail <- NA
  if (length(raw_daily)!=0)
    site_info$startDate <- first(as.Date(raw_daily$Date))
  if (length(raw_daily)!=0)
    site_info$endDate <- last(as.Date(raw_daily$Date))
  if (length(raw_daily)!=0)
    site_info$yearsAvail <- year(site_info$endDate) - year(site_info$startDate)
  raw_daily <- na.omit(raw_daily) ## print message for how many were omitted for NAs, maybe in future change so only omits if a certain % of NAs
  
  site_select <- rbind(site_select, site_info)
  site_select <- site_select[!duplicated(site_select), ]
  site_select <- subset(site_select, yearsAvail >= 30, select = c(1:8))
  if(count(raw_daily$X_00060_00003==NA))
  # site_select <- subset(site_select, yearsAvail >= 100, select = c(1:8))
  
  ## APPEND HUC06 site selected to corresponding HUC06 data frame, CHOOSE HUC06 by commenting out other HUC06 watersheds
  # huc040500MI <- rbind(huc040500MI, site_select)
  # huc040500MI <- huc040500MI[!duplicated(huc040500MI), ]
  # huc110300KS <- rbind(huc110300KS, site_select)
  # huc110300KS <- huc110300KS[!duplicated(huc110300KS), ]
  huc180102CA <- rbind(huc180102CA, site_select)
  huc180102CA <- huc180102CA[!duplicated(huc180102CA), ]

}

## SUBSET by start and end dates
huc040500MI_ws_1962_sel <- subset(huc040500MI_ws, startDate == "1962-10-01", select = c(1:8))
huc040500MI_ws_1962_2021_sel <- subset(huc040500MI_ws_1962_sel, endDate == "2021-09-30", select = c(1:8))
huc110300KS_ws_1962_sel <- subset(huc110300KS_ws, startDate == "1962-10-01", select = c(1:8))
huc110300KS_ws_1962_2021_sel <- subset(huc110300KS_ws_1962_sel, endDate == "2021-09-30", select = c(1:8))
huc180102CA_ws_1962_sel <- subset(huc180102CA_ws, startDate == "1962-10-01", select = c(1:8))
huc180102CA_ws_1962_2021_sel <- subset(huc180102CA_ws_1962_sel, endDate == "2021-09-30", select = c(1:8))

huc040500MI_ws_ep <- subset(huc040500MI_ws, startDate == "1962-10-01", select = c(1:8))
huc040500MI_ws_ep <- subset(huc040500MI_ws_1962_sel, endDate == "2021-09-30", select = c(1:8))
huc110300KS_ws_ep <- subset(huc110300KS_ws, startDate == "1962-10-01", select = c(1:8))
huc110300KS_ws_ep <- subset(huc110300KS_ws_1962_sel, endDate == "2021-09-30", select = c(1:8))
huc180102CA_ws_ep <- subset(huc180102CA_ws, startDate == "1962-10-01", select = c(1:8))
huc180102CA_ws_ep <- subset(huc180102CA_ws_1962_sel, endDate == "2021-09-30", select = c(1:8))


## CREATE/SAVE watershed object to work with and RESET as needed without re-running for loops
huc040500MI_ws <- huc040500MI_ws_long
huc110300KS_ws <- huc110300KS_ws_long
huc180102CA_ws <- huc180102CA_ws_long


## optional WRITE watershed site information to CSV for use with GIS and webGIS
write.csv(huc040500MI_ws, "~/GradSchool/DiscoverStreams/outputs/watershed_info/huc040500MI_1962-2021.csv")
write.csv(huc110300KS_ws, "~/GradSchool/DiscoverStreams/outputs/watershed_info/huc110300KS_1962-2021.csv")
write.csv(huc180102CA_ws, "~/GradSchool/DiscoverStreams/outputs/watershed_info/huc180102CA_1962-2021.csv")

## optional WRTIE watershed site information to mySQL database for use with webapp
dbWriteTable(con_UW, "interact_sitedata_SELakeMI", huc040500MI_ws, overwrite = TRUE)
dbWriteTable(con_UW, "interact_sitedata_MidArkansas", huc110300KS_ws, overwrite = TRUE)
dbWriteTable(con_UW, "interact_sitedata_Klamath", huc180102CA_ws, overwrite = TRUE)



### STREAMFLOW from NWIS ###
## CLEAR corresponding streamflow dataframe for each HUC06, also set HUC06 in dataframe setup and in for loop AND DOUBLE-CHECK START & END DATES
huc040500MI_sf <- data.frame()
huc110300KS_sf <- data.frame()
huc180102CA_sf <- data.frame()

## CHANGE HUC06 name in for loop then RUN
## RUN code within for loop for i = 1, then run for loop for i >= 2 
i = 1

## CHOOSE watershed before running for loop
# for (i in 2:nrow(huc040500MI_ws)) {
#         site_number <- huc040500MI_ws$site_no[i]
# for (i in 2:nrow(huc110300KS_ws)) {
#       site_number <- huc110300KS_ws$site_no[i]
for (i in 2:nrow(huc180102CA_ws)) {
  site_number <- huc180102CA_ws$site_no[i]
  
  ## RETRIEVE site info and streamflow data for gage station
  site_info <- dataRetrieval::readNWISsite(site_number)
  site_name <- site_info$station_nm
  
  raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)
  raw_daily <- subset(raw_daily, select = c(3,4))
  colnames(raw_daily) <- c("Date", site_name)
  
  ## only run for i = 1 to prime dataframe for joining, then comment out and run for loop
  # huc040500MI_sf <- raw_daily
  # huc110300KS_sf <- raw_daily
  # huc180102CA_sf <- raw_daily
  
  ## RUN for i >= 2, APPEND streamflow data for site selected to corresponding HUC06 streamflow data frame, CHOOSE HUC06 by commenting out other HUC06 watersheds
  # huc040500MI_sf <- full_join(huc040500MI_sf, raw_daily, by = "Date")
  # huc110300KS_sf <- full_join(huc110300KS_sf, raw_daily, by = "Date")
  huc180102CA_sf <- full_join(huc180102CA_sf, raw_daily, by = "Date")
  
}

## SAVE resulting streamflow object to dataframe
huc040500MI_sf_long <- huc040500MI_sf
huc110300KS_sf_long <- huc110300KS_sf
huc180102CA_sf_long <- huc180102CA_sf

## WRITE/UPDATE data to SQL database of corresponding watershed
dbWriteTable(con_UW, "interact_streamflow_SELakeMI", huc040500MI_sf, overwrite = TRUE)
dbWriteTable(con_UW, "interact_streamflow_MidArkansas", huc110300KS_sf, overwrite = TRUE)
dbWriteTable(con_UW, "interact_streamflow_Klamath", huc180102CA_sf, overwrite = TRUE)


### WATER USE (NWIS) ###
## RUN corresponding streamflow dataframe for each HUC06, also set HUC06 in dataframe setup and in for loop
huc040500MI_wu <- data.frame()
huc110300KS_wu <- data.frame()
huc180102CA_wu <- data.frame()

## SETUP streamflow dataframe with data from i=1 
site_number <- huc180102CA_ws$site_no[i=1]

site_info <- dataRetrieval::readNWISsite(site_number)
state <- site_info$state_cd
county <- site_info$county_cd

## RETRIEVE Water Use
waterUse <- dataRetrieval::readNWISuse(
  stateCd = state,
  countyCd = county,
  years = "ALL",
  categories = "ALL",
  convertType = TRUE,
  transform = FALSE
)

waterUse <- waterUse[ , c(5,243)]
# 243 	Irrigation..Total.total.consumptive.use..in.Mgal.d
colnames(waterUse) <- c("Date", huc180102CA_ws$station_nm[i])
huc180102CA_wu <- waterUse

## CHANGE HUC06 name in for loop then RUN
for (i in seq_along(huc180102CA_ws$site_no)) {  
  site_number <- huc180102CA_ws$site_no[i]
  
  ## RETRIEVE site info necessary for water use data retrieval
  site_info <- dataRetrieval::readNWISsite(site_number)
  state <- site_info$state_cd
  county <- site_info$county_cd
  
  ## RETRIEVE Water Use
  waterUse <- dataRetrieval::readNWISuse(
    stateCd = state,
    countyCd = county,
    years = "ALL",
    categories = "ALL",
    convertType = TRUE,
    transform = FALSE
  )
  
  waterUse <- waterUse[ , c(5,243)]
  # 243 	Irrigation..Total.total.consumptive.use..in.Mgal.d
  colnames(waterUse) <- c("Date", huc180102CA_ws$station_nm[i])
  
  if (!duplicated(huc180102CA_wu))
    ## APPEND streamflow data for site selected to corresponding HUC06 streamflow data frame, CHOOSE HUC06 by commenting out other HUC06 watersheds
    # huc040500MI_wu <- full_join(huc040500MI_wu, waterUse, by = "Date")
    # huc110300KS_wu <- full_join(huc110300KS_wu, waterUse, by = "Date")
    huc180102CA_wu <- full_join(huc180102CA_wu, waterUse, by = "Date")
}

######################################################  
### CLIMATE (NOAA) ###
## RUN corresponding streamflow dataframe for each HUC06, also set HUC06 in dataframe setup and in for loop
huc040500MI_cl <- data.frame()
huc110300KS_cl <- data.frame()
huc180102CA_cl <- data.frame()

MI_ncdc_datasets <- rnoaa::ncdc_datasets(locationid = "HUC:040500", datasetid = "NORMAL_DLY")
MI_ncdc <- rnoaa::ncdc(datasetid = "GHCND", datatypeid = "PRCP", locationid = "HUC:040500", startdate = "2021-01-01", enddate = "2022-01-01", add_units = TRUE)
ncdc_plot(MI_ncdc, breaks = "1 month", dateformat = "%d/%m")

station_data <- ghcnd_stations()
MI_8 <- data.frame(name = huc040500MI_ws[8,2], lat = huc040500MI_ws[8,3], lon = huc040500MI_ws[8,4], station_data = station_data, radius = 10)

MI_meteo_stations <- rnoaa::meteo_nearby_stations(MI_8, lat_colname = "lat", lon_colname = "lon", station_data = ghcnd_stations())
