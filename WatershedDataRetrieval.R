# data manipulation libraries
library(tidyverse)
library(lubridate)
# data retrieval libraries
library(dataRetrieval)




### WATERSHED STATION INFO - all gage stations within each watershed ###
NWISsites_040500MI <- c("04100222", "04100500", "04101000", "04101370", "04101225", "04101500", "04099510", "04099750", "04098430", "040975296", "040975299", "04096767", "04097540", "04098620", "04098636", "04098980", "04099000", "04098757", "0409754132", "04097500", "04096590", "04096681", "04096970", "0409651496", "04096272", "04096312", "04096325", "04096340", "04096400", "04096405", "04096500", "04096515", "04096600", "04096900", "04097060", "04097170", "04097195", "04097200", "04097528", "040975299", "04097970", "04098000", "04098500", "04099610", "04099808", "04099850", "04100000", "04100252", "04100295", "04100377", "04100465", "041015313", "04101535", "04101590", "04101800", "04102000", "04102085", "04102148", "04102320", "04102420", "04102500", "04102533", "04102700", "04102776", "04108800", "04108801", "04108862", "04108872", "041027908", "04102798", "04102988", "04103490", "04103500", "041035285", "04104945", "04105000", "04105500", "04105700", "04106000", "04106500", "04106400", "04106320", "04108600", "04108645", "04108660", "04102850", "04103010", "04104000", "04104500", "04105800", "04106180", "04106190", "04106300", "04106906", "04107850", "04108000", "04108500", "04108670", "04109000", "04111000", "04112700", "04112850", "04111379", "04112000", "04112500", "04113000", "04114498", "04114000", "04109500", "04110000", "04111500", "04112904", "04113097", "04114500", "04115000", "04115265", "04116000", "04116500", "04118500", "04118564", "04119000", "04119055", "04119070", "04119160", "04119345", "04119365", "04119400", "04120194", "04119300", "04120250", "04117004", "04117500", "04118000", "04118105", "04117000")

# huc040500MI_counties <- c("Muskegon", "Montcalm", "Gratiot", "Ottawa", "Kent", "Ionia", "Clinton", "Shiawassee", "Allegan", "Barry", "Eaton", "Ingham", "Livingston", "Van Buren", "Kalamazoo", "Calhoun", "Jackson", "Berrien", "Cass", "St. Joseph", "Branch", "Hillsdale")
# huc040500IN_counties <- c("St. Joseph", "Elkhart", "LaGrange", "Steuben", "Noble")

NWISsites_110300KS <- c("07137000", "07137500", "07137010", "07138000", "07138020", "07138050", "375615101170800", "07138070", "07138064", "07138075", "07139000", "07137500", "07138062", "07138063", "07138065", "07138650", "07138660", "07139500", "07141220", "07141300", "07139800", "07140000", "07140500", "07141200", "07140700", "07140850", "07141000", "07140900", "07140885", "07140880", "07140890", "07141175", "07141780", "07141900", "07142019", "07141770", "07142015", "07142020", "07142300", "07142575", "07142620", "07142680", "07143330", "07143340", "07143350", "07143375", "07142800", "07143400", "07143300", "07143310", "07142860", "07142900", "07143665", "07143672", "07144050", "375348097262800", "07144100", "07144201", "07144200","07143600", "07143680", "07144000", "375350097262800", "07144300", "07144325", "07144330", "07144340", "07144480", "07144485", "07144486", "07144490", "07144550", "07144570", "07145600", "07145700", "07146500", "07144301", "07144470", "07144780", "07144790", "07144601", "07144660", "07144680", "07144730", "07144795", "07144800", "07145200", "07144850", "07144910", "07145500", "07146800", "07147070", "07146570", "07146623", "07146830", "07146895", "07146990", "07147050", "07147060", "07147190", "07147800", "07147900", "07147600")

NWISsites_180102CA <- c("11502500", "11502550", "11493500", "11492200", "11491400", "11492400", "11497550", "11501000", "11495800","11497500", "11499100", "421401121480900", "11507200", "11504270", "11504260", "423456121562900", "11504115", "11504290", "11503001", "11504000", "11504100", "11504107", "11505600", "11505700", "11507500", "11507501", "421015121471800", "11509105", "420853121505500", "420853121505501", "11509370", "420741121554001", "420448121503100", "11509200", "420450121504500", "420451121510000", "420219121474500", "420219121465200", "420218121455900", "420134121444800", "420029121461700", "420007121464200", "420009121485700", "420014121493200", "420036121333700", "415954121312100", "421010121271200", "420535121143800", "11483500", "11484000", "11486000", "11486990", "11488510", "11488700", "11507400", "11508500", "11509250", "11509340", "420524121515200", "420615121533600", "420615121533601", "420732121501100", "421131121465900", "421209121463000", "421209121463001", "421330121474700", "11489500", "11490000", "11490500", "11509500", "11510700", "11516528", "11516530", "11517800", "11520500", "11510000", "11510500", "11512500", "11512920", "11514500", "11516600", "14339400", "420523122042000", "420743121565400", "420903122010900", "11517500", "11517000", "11516750", "11516900", "11519500", "11517900", "11517950", "11518000", "11518050", "11518200", "11518300", "11518310", "11518600", "11519000", "11520000", "11520800", "11521500", "11522200", "11523000", "11530500", "11521000", "11522260", "11523000", "11523030", "11523050", "11530150", "11530300", "11522300", "11522400", "11522500", "11530000", "11529800", "11527000", "11526400", "11526250", "11525854", "11525670", "11525655", "11525530", "11525500", "11523200", "11523700", "11524000", "11525535", "11525540", "11525600", "11525630", "11525750", "11525800", "11525900", "11526000", "11526300", "11526500", "11527400", "11527500", "11528000", "11529500", "11530020", "11528700", "11528100", "11528200", "11528400", "11528440", "11528500", "11529000")



## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- "1959-10-01"  # Michigan HD data
start_date <- "1962-10-01"  # Kansas HD data
start_date <- "1961-10-01"  # California HD data
start_date <- "1951-10-01"  # All 1951 data
start_date <- ""  # Long and EP data
end_date <- ""  # Long data
end_date <- "2021-09-30" # HD and 1951 data
end_date <- "1940-09-30" # EP data

## RUN corresponding huc dataframe for each HUC06, also set HUC06 in for loop
huc040500MI <- data.frame()
huc110300KS <- data.frame()
huc180102CA <- data.frame()

## CLEAR site select data frame before running for loop for each HUC06 
site_select <- data.frame()

## CHANGE HUC08 name then RUN for loop-- SET start date and end date before running for loop
# for (i in seq_along(NWISsites_040500MI)) {
#   site_number <- NWISsites_040500MI[i]
# for (i in seq_along(NWISsites_110300KS)) {
#         site_number <- NWISsites_110300KS[i]
for (i in seq_along(NWISsites_180102CA)) {
        site_number <- NWISsites_180102CA[i]
  
  site_info <- dataRetrieval::readNWISsite(site_number)
  ## keep columns 2, 3, 7, 8, 24
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
  
  ## SELECT sites with more than 15 years (for EP data), more than 30 years data (for all records and HD data), or more than 100 years data (for long data)
  # site_select <- subset(site_select, yearsAvail >= 15, select = c(1:8))
  site_select <- subset(site_select, yearsAvail >= 30, select = c(1:8))
  # site_select <- subset(site_select, yearsAvail >= 100, select = c(1:8))
  
  ## APPEND HUC06 site selected to corresponding HUC06 data frame, CHOOSE HUC06 by commenting out other HUC06 watersheds
  # huc040500MI <- rbind(huc040500MI, site_select)
  # huc040500MI <- huc040500MI[!duplicated(huc040500MI), ]
  # huc110300KS <- rbind(huc110300KS, site_select)
  # huc110300KS <- huc110300KS[!duplicated(huc110300KS), ]
  huc180102CA <- rbind(huc180102CA, site_select)
  huc180102CA <- huc180102CA[!duplicated(huc180102CA), ]

}


## SUBSET entire record, by specific start and end dates, OR trim data using start date and end date from for loop
huc040500MI_1951sel <- huc040500MI
huc040500MI_1960sel <- subset(huc040500MI, startDate == "1959-10-01", select = c(1:8))

huc110300KS_1951sel <- huc110300KS
huc110300KS_1963sel <- subset(huc110300KS, startDate == "1962-10-01", select = c(1:8))

huc180102CA_1951sel <- huc180102CA
huc180102CA_1962sel <- subset(huc180102CA, startDate == "1961-10-01", select = c(1:8))


## SUBSET high-density (HD) data from climate neutral year, SAVE to watershed object to build on with relevant metrics while preserving original selected data
huc040500MI_hd <- subset(huc040500MI, startDate == "1959-10-01", select = c(1:8))
huc040500MI_hd <- subset(huc040500MI_hd, endDate == "2021-09-30", select = c(1:8))
ws_040500MI_hd <- huc040500MI_hd

huc110300KS_hd <- subset(huc110300KS, startDate == "1962-10-01", select = c(1:8))
huc110300KS_hd <- subset(huc110300KS_hd, endDate == "2021-09-30", select = c(1:8))
ws_110300KS_hd <- huc110300KS_hd

huc180102CA_hd <- subset(huc180102CA, startDate == "1961-10-01", select = c(1:8))
huc180102CA_hd <- subset(huc180102CA_hd, endDate == "2021-09-30", select = c(1:8))
ws_180102CA_hd <- huc180102CA_hd

## SUBSET by long-period data and SAVE to watershed object to build on while preserving original selected data
huc040500MI_all <- huc040500MI
ws_040500MI_all <- huc040500MI_all

huc110300KS_all <- huc110300KS
ws_110300KS_all <- huc110300KS_all

huc180102CA_all <- huc180102CA
ws_180102CA_all <- huc180102CA_all

## SUBSET by long-period data and SAVE to watershed object to build on while preserving original selected data
huc040500MI_long <- huc040500MI
ws_040500MI_long <- huc040500MI_long

huc110300KS_long <- huc110300KS
ws_110300KS_long <- huc110300KS_long

huc180102CA_long <- huc180102CA
ws_180102CA_long <- huc180102CA_long

## SUBSET early period (EP) data and SAVE to watershed object to build on with relevant metrics while preserving original selected data
huc040500MI_ep <- huc040500MI
ws_040500MI_ep <- huc040500MI_ep

huc110300KS_ep <- huc110300KS
ws_110300KS_ep <- huc110300KS_ep

huc180102CA_ep <- huc180102CA
ws_180102CA_ep <- huc180102CA_ep



## optional WRITE watershed site information to CSV for use with GIS and webGIS
write.csv(ws_040500MI, "~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_040500MI.csv")
write.csv(ws_110300KS, "~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_110300KS.csv")
write.csv(ws_180102CA, "~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_180102CA.csv")




###########################################################################################
### STREAMFLOW from NWIS ###
## CHOOSE watershed object to work with by choosing watershed object or selected data of different time periods
ws_040500MI <- ws_040500MI_hd
ws_110300KS <- ws_110300KS_hd
ws_180102CA <- ws_180102CA_hd

ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_long
ws_180102CA <- ws_180102CA_long

ws_040500MI <- ws_040500MI_ep
ws_110300KS <- ws_110300KS_ep
ws_180102CA <- ws_180102CA_ep

ws_040500MI <- ws_040500MI_all
ws_110300KS <- ws_110300KS_all
ws_180102CA <- ws_180102CA_all

## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- "1959-10-01"  # Michigan HD data
start_date <- "1962-10-01"  # Kansas HD data
start_date <- "1961-10-01"  # California HD data
start_date <- "1951-10-01"  # All 1951 data
start_date <- ""  # Long and EP data
end_date <- ""  # Long data
end_date <- "2021-09-30" # HD and 1951 data
end_date <- "1940-09-30" # EP data

## CLEAR corresponding streamflow dataframe for each HUC06, also set HUC06 in dataframe setup and in for loop AND DOUBLE-CHECK START & END DATES
sf_040500MI <- data.frame()
sf_110300KS <- data.frame()
sf_180102CA <- data.frame()

## CHANGE HUC06 name in for loop then RUN
## RUN code within for loop for i = 1, then run for loop for i >= 2 -- SET start date and end date before running for loop
i = 1

## CHOOSE watershed before running for loop
for (i in 2:nrow(ws_040500MI)) {
        site_number <- ws_040500MI$site_no[i]
# for (i in 2:nrow(ws_110300KS)) {
#       site_number <- ws_110300KS$site_no[i]
# for (i in 2:nrow(ws_180102CA)) {
#   site_number <- ws_180102CA$site_no[i]
  
  ## RETRIEVE site info and streamflow data for gage station
  site_info <- dataRetrieval::readNWISsite(site_number)
  site_name <- site_info$station_nm
  
  raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)
  raw_daily <- subset(raw_daily, select = c(3,4))
  colnames(raw_daily) <- c("Date", site_name)
  
  ## only run for i = 1 to prime dataframe for joining, then comment out and run for loop
  # sf_040500MI <- raw_daily
  # sf_110300KS <- raw_daily
  # sf_180102CA <- raw_daily
  
  ## RUN for i >= 2, APPEND streamflow data for site selected to corresponding HUC06 streamflow data frame, CHOOSE HUC06 by commenting out other HUC06 watersheds
  sf_040500MI <- full_join(sf_040500MI, raw_daily, by = "Date")
  # sf_110300KS <- full_join(sf_110300KS, raw_daily, by = "Date")
  # sf_180102CA <- full_join(sf_180102CA, raw_daily, by = "Date")
  
}

## SAVE resulting streamflow object to dataframe
sf_040500MI_hd <- sf_040500MI
sf_110300KS_hd <- sf_110300KS
sf_180102CA_hd <- sf_180102CA

sf_040500MI_long <- sf_040500MI
sf_110300KS_long <- sf_110300KS
sf_180102CA_long <- sf_180102CA

sf_040500MI_ep <- sf_040500MI
sf_110300KS_ep <- sf_110300KS
sf_180102CA_ep <- sf_180102CA

sf_040500MI_all <- sf_040500MI
sf_110300KS_all <- sf_110300KS
sf_180102CA_all <- sf_180102CA


######################################################################################
### WATER USE (NWIS) ###
## CHOOSE watershed object to work with by choosing watershed object or selected data of different time periods
ws_040500MI <- ws_040500MI_hd
ws_110300KS <- ws_110300KS_hd
ws_180102CA <- ws_180102CA_hd

ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_long
ws_180102CA <- ws_180102CA_long

ws_040500MI <- ws_040500MI_ep
ws_110300KS <- ws_110300KS_ep
ws_180102CA <- ws_180102CA_ep

## RUN corresponding streamflow dataframe for each HUC06, also set HUC06 in dataframe setup and in for loop
wu_040500MI_NWIS <- data.frame()
wu_110300KS_NWIS <- data.frame()
wu_180102CA_NWIS <- data.frame()

## RUN code within for loop for i = 1, then run for loop for i >= 2 -- SET start date and end date before running for loop
i = 1

## CHANGE HUC06 name in for loop then RUN
# for (i in 2:nrow(ws_040500MI)) {
#   site_number <- ws_040500MI$site_no[i]
# for (i in 2:nrow(ws_110300KS)) {
#       site_number <- ws_110300KS$site_no[i]
for (i in 2:nrow(ws_180102CA)) {
  site_number <- ws_180102CA$site_no[i]
  
  ## RETRIEVE site info necessary for water use data retrieval
  site_info <- dataRetrieval::readNWISsite(site_number)
  state <- site_info$state_cd
  county <- site_info$county_cd
  
  ## RETRIEVE Water Use
  raw_wu <- dataRetrieval::readNWISuse(
    stateCd = state,
    countyCd = county,
    years = "ALL",
    categories = "ALL",
    convertType = TRUE,
    transform = FALSE
  )
  
  ## SELECT relevant columns from raw water use data, rename columns--CHOOSE corresponding watershed before running for loop
  raw_wu <- raw_wu[ , c(5,243)]
  ## 243 	Irrigation..Total.total.consumptive.use..in.Mgal.d
  # colnames(raw_wu) <- c("Date", ws_040500MI$station_nm[i])
  # colnames(raw_wu) <- c("Date", ws_110300KS$station_nm[i])
  colnames(raw_wu) <- c("Date", ws_180102CA$station_nm[i])
  
  ## only run for i = 1 to prime dataframe for joining, then comment out and run for loop
  # wu_040500MI <- raw_wu
  # wu_110300KS <- raw_wu
  # wu_180102CA <- raw_wu
  
    ## APPEND streamflow data for site selected to corresponding HUC06 streamflow data frame, CHOOSE HUC06 by commenting out other HUC06 watersheds
    # wu_040500MI <- full_join(wu_040500MI, raw_wu, by = "Date")
    # wu_110300KS <- full_join(wu_110300KS, raw_wu, by = "Date")
    wu_180102CA <- full_join(wu_180102CA, raw_wu, by = "Date")
}


## SAVE resulting water use object to dataframe
wu_040500MI_hd <- wu_040500MI
wu_110300KS_hd <- wu_110300KS
wu_180102CA_hd <- wu_180102CA

wu_040500MI_long <- wu_040500MI
wu_110300KS_long <- wu_110300KS
wu_180102CA_long <- wu_180102CA

wu_040500MI_ep <- wu_040500MI
wu_110300KS_ep <- wu_110300KS
wu_180102CA_ep <- wu_180102CA

