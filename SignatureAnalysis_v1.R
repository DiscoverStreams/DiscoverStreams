library(readr)
library(odbc)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(DataExplorer)
library(jsonlite)
library(streamDepletr)
library(dataRetrieval)
library(xts)
library(hydrostats)
library(lfstat)


setwd("~/GradSchool/DiscoverStreams")
wname <- getwd() # set back to working directory
dname <- paste(wname,"data",sep="/") # will open data file in workikng directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in workikng directory
pname<-paste(wname,"plots",sep="/") # will open plots file in workikng directory


# Run once, no need to change parameters for streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- ""
end_date <- ""


### WATERSHED STATION INFO ###
huc04050001MI_sites <- c("04100222", "04100500", "04101000", "04101370", "04101225", "04101500", "04099510", "04099750", "04098430", "040975296", "040975299", "04096767", "04097540", "04098620", "04098636", "04098980", "04099000", "04098757", "0409754132", "04097500", "04096590", "04096681", "04096970", "0409651496", "04096272", "04096312", "04096325", "04096340", "04096400", "04096405", "04096500", "04096515", "04096600", "04096900", "04097060", "04097170", "04097195", "04097200", "04097528", "040975299", "04097970", "04098000", "04098500", "04099610", "04099808", "04099850", "04100000", "04100252", "04100295", "04100377", "04100465", "041015313", "04101535", "04101590", "04101800", "04102000", "04102085", "04102148", "04102320", "04102420", "04102500", "04102533")     
huc04050002MI_sites <- c("04102700", "04102776", "04108800", "04108801", "04108862", "04108872")
huc04050003MI_sites <- c("041027908", "04102798", "04102988", "04103490", "04103500", "041035285", "04104945", "04105000", "04105500", "04105700", "04106000", "04106500", "04106400", "04106320", "04108600", "04108645", "04108660", "04102850", "04103010", "04104000", "04104500", "04105800", "04106180", "04106190", "04106300", "04106906", "04107850", "04108000", "04108500", "04108670")
huc04050004MI_sites <- c("04109000", "04111000", "04112700", "04112850", "04111379", "04112000", "04112500", "04113000", "04114498", "04114000", "04109500", "04110000", "04111500", "04112904", "04113097", "04114500")
huc04050005MI_sites <- c("04115000", "04115265") 
huc04050006MI_sites <- c("04116000", "04116500", "04118500", "04118564", "04119000", "04119055", "04119070", "04119160", "04119345", "04119365", "04119400", "04120194", "04119300", "04120250")
huc04050007MI_sites <- c("04117004", "04117500", "04118000", "04118105", "04117000")                          
huc11030001KS_sites <- c("07137000", "07137500", "07137010", "07138000", "07138020", "07138050", "375615101170800", "07138070", "07138064", "07138075", "07139000", "07137500", "07138062", "07138063", "07138065")
huc11030002KS_sites <- c("07138650", "07138660")
huc11030003KS_sites <- c("07139500")
huc11030004KS_sites <- c("07141220", "07141300", "07139800", "07140000", "07140500")
huc11030005KS_sites <- c("07141200", "07140700")
huc11030006KS_sites <- c("07140850", "07141000", "07140900", "07140885", "07140880", "07140890", "07141175")
huc11030007KS_sites <- c()
huc11030008KS_sites <- c("07141780", "07141900", "07142019", "07141770", "07142015", "07142020")
huc11030009KS_sites <- c("07142300", "07142575", "07142620")
huc11030010KS_sites <- c("07142680", "07143330", "07143340", "07143350", "07143375", "07142800", "07143400")
huc11030011KS_sites <- c("07143300", "07143310", "07142860", "07142900")
huc11030012KS_sites <- c("07143665", "07143672", "07144050", "375348097262800", "07144100", "07144201", "07144200","07143600", "07143680", "07144000", "375350097262800")
huc11030013KS_sites <- c("07144300", "07144325", "07144330", "07144340", "07144480", "07144485", "07144486", "07144490", "07144550", "07144570", "07145600", "07145700", "07146500", "07144301", "07144470")
huc11030014KS_sites <- c("07144780", "07144790", "07144601", "07144660", "07144680", "07144730", "07144795", "07144800")
huc11030015KS_sites <- c("07145200", "07144850", "07144910")
huc11030016KS_sites <- c("07145500")
huc11030017KS_sites <- c("07146800", "07147070", "07146570", "07146623", "07146830", "07146895", "07146990", "07147050", "07147060", "07147190")
huc11030018KS_sites <- c("07147800", "07147900", "07147600")

huc18010201CA_sites <- c("11502500", "11502550", "11493500", "11492200", "11491400", "11492400")
huc18010202CA_sites <- c("11497550", "11501000", "11495800","11497500", "11499100")
huc18010203CA_sites <- c("421401121480900", "11507200", "11504270", "11504260", "423456121562900", "11504115", "11504290", "11503001", "11504000", "11504100", "11504107", "11505600", "11505700")
huc18010204CA_sites <- c("11507500", "11507501", "421015121471800", "11509105", "420853121505500", "420853121505501", "11509370", "420741121554001", "420448121503100", "11509200", "420450121504500", "420451121510000", "420219121474500", "420219121465200", "420218121455900", "420134121444800", "420029121461700", "420007121464200", "420009121485700", "420014121493200", "420036121333700", "415954121312100", "421010121271200", "420535121143800", "11483500", "11484000", "11486000", "11486990", "11488510", "11488700", "11507400", "11508500", "11509250", "11509340", "420524121515200", "420615121533600", "420615121533601", "420732121501100", "421131121465900", "421209121463000", "421209121463001", "421330121474700")
huc18010205CA_sites <- c("11489500", "11490000", "11490500")
huc18010206CA_sites <- c("11509500", "11510700", "11516528", "11516530", "11517800", "11520500", "11510000", "11510500", "11512500", "11512920", "11514500", "11516600", "14339400", "420523122042000", "420743121565400", "420903122010900")
huc18010207CA_sites <- c("11517500", "11517000", "11516750", "11516900")
huc18010208CA_sites <- c("11519500", "11517900", "11517950", "11518000", "11518050", "11518200", "11518300", "11518310", "11518600", "11519000", "11520000")
huc18010209CA_sites <- c("11520800", "11521500", "11522200", "11523000", "11530500", "11521000", "11522260", "11523000", "11523030", "11523050", "11530150", "11530300")
huc18010210CA_sites <- c("11522300", "11522400", "11522500")
huc18010211CA_sites <- c("11530000", "11529800", "11527000", "11526400", "11526250", "11525854", "11525670", "11525655", "11525530", "11525500", "11523200", "11523700", "11524000", "11525535", "11525540", "11525600", "11525630", "11525750", "11525800", "11525900", "11526000", "11526300", "11526500", "11527400", "11527500", "11528000", "11529500", "11530020")
huc18010212CA_sites <- c("11528700", "11528100", "11528200", "11528400", "11528440", "11528500", "11529000")

# MIhucsites <- c("huc04050001MI_sites", "huc04050002MI_sites", "huc04050003MI_sites", "huc04050004MI_sites", "huc04050005MI_sites", "huc04050006MI_sites", "huc04050007MI_sites")
# KShucsites <- c(huc11030001KS_sites, huc11030002KS_sites, huc11030003KS_sites, huc11030004KS_sites, huc11030005KS_sites, huc11030006KS_sites, huc11030007KS_sites, huc11030008KS_sites, huc11030009KS_sites, huc11030010KS_sites, huc11030011KS_sites, huc11030012KS_sites, huc11030013KS_sites, huc11030014KS_sites, huc11030015KS_sites, huc11030016KS_sites, huc11030017KS_sites, huc11030018KS_sites)
# CAhucsites <- c(huc18010201CA_sites, huc18010202CA_sites, huc18010203CA_sites, huc18010204CA_sites, huc18010205CA_sites, huc18010206CA_sites, huc18010207CA_sites, huc18010208CA_sites, huc18010209CA_sites, huc18010210CA_sites, huc18010211CA_sites, huc18010212CA_sites)


## RUN corresponding huc dataframe for each HUC06, also set HUC06 in for loop
huc040500MI <- data.frame()
huc110300KS <- data.frame()
huc180102CA <- data.frame()

## CLEAR site select data frame before running for loop for each HUC08
site_select <- data.frame()

## CHANGE HUC08 name in for loop then RUN
for (i in seq_along(huc18010212CA_sites)) {
        site_number <- huc18010212CA_sites[i]
        site_info <- readNWISsite(site_number)
        # keep columns 2, 3, 7, 8, 24
        site_info <- site_info[ , c(2, 3, 7, 8, 24)]
        
        raw_daily <- readNWISdv(site_number, parameter_code, start_date, end_date)
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
                        
        site_select <- rbind(site_select, site_info)
        site_select <- site_select[!duplicated(site_select), ]
        site_select <- subset(site_select, yearsAvail >= 30, select = c(1:8))
        
        # hucname <- substr(MIhucsites[i=3], 1, nchar(MIhucsites[i=3])-6)
        # paste(hucname,"_set") <- data.frame()
        
        ## APPEND HUC08 site selected to corresponding HUC06 data frame, CHOOSE HUC08 by commenting out other HUC06 watersheds
        # huc040500MI <- rbind(huc040500MI, site_select)
        # huc040500MI <- huc040500MI[!duplicated(huc040500MI), ]
        # huc110300KS <- rbind(huc110300KS, site_select)
        # huc110300KS <- huc110300KS[!duplicated(huc110300KS), ]
        huc180102CA <- rbind(huc180102CA, site_select)
        huc180102CA <- huc180102CA[!duplicated(huc180102CA), ]
        
}

## optional CREATE data frame for corresponding HUC08 from for loop
huc04050001MI <- site_select
huc04050002MI <- site_select
huc04050003MI <- site_select
huc04050004MI <- site_select
huc04050005MI <- site_select
huc04050006MI <- site_select
huc04050007MI <- site_select

huc11030001KS <- site_select
huc11030002KS <- site_select
huc11030003KS <- site_select
huc11030004KS <- site_select
huc11030005KS <- site_select
huc11030006KS <- site_select
huc11030007KS <- site_select
huc11030008KS <- site_select
huc11030009KS <- site_select
huc11030010KS <- site_select
huc11030011KS <- site_select
huc11030012KS <- site_select
huc11030013KS <- site_select
huc11030014KS <- site_select
huc11030015KS <- site_select
huc11030016KS <- site_select
huc11030017KS <- site_select
huc11030018KS <- site_select

huc18010201CA <- site_select
huc18010202CA <- site_select
huc18010203CA <- site_select
huc18010204CA <- site_select
huc18010205CA <- site_select
huc18010206CA <- site_select
huc18010207CA <- site_select
huc18010208CA <- site_select
huc18010209CA <- site_select
huc18010210CA <- site_select
huc18010211CA <- site_select
huc18010212CA <- site_select









### STREAMFLOW (NWIS) - SCOTT VALLEY, CALIFORNIA ###
sites_ca <- c("11519500", "11519000", "11518200", "11518050")
color_ca <- c("aquamarine3", "darkblue", "darkred", "burlywood3")
## #66cdaa, #01178b, #8a0f00, #cdaa7d

### STREAMFLOW (NWIS) - PRAIRIE RIVER, MICHIGAN ###
sites_mi <- c("04097540", "040975253", "04097526", "04097528", "040975296", "04097529", "040975299", "04097530", "04097540", "0409754049", "0409754132", "0409754153", "0409754167", "04097500" )
color_mi <- c("deepskyblue", "cadetblue2", "deepskyblue4") 
## #01bfff, #8ee5ee, #01688b

### STREAMFLOW (NWIS) - KANSAS ###



### RETRIEVE STREAMFLOW DATA FROM NWIS ###
# SELECT site, then retrieve data
site_number <- "11519500" ## Scott R
site_number <- "04097540" ## Prairie R


# RETRIEVE streamflow data from NWIS
raw_daily <- readNWISdv(site_number, parameter_code, start_date, end_date)
raw_daily <- drop_columns(raw_daily, c("site_no", "agency_cd", "X_00060_00003_cd"))

# REFORMAT data - Scott River, CA
colnames(raw_daily) <- c("Date", "Scott_Discharge_cfs")
streamflow_SR <- raw_daily

# REFORMAT data - Prairie River, MI
colnames(raw_daily) <- c("Date", "Prairie_Discharge_cfs")
streamflow_PR <- raw_daily

# COMBINE sites into dataframe
streamflow <- left_join(streamflow_SR, streamflow_PR, by = "Date") 



### PLOT ###

p <- ggplot(raw_daily, aes(x=Date, y=Discharge_cfs)) +
        geom_line(color = color_ca[4]) +
        scale_y_continuous(trans='log10')

# Set axis limits c(min, max)
min <- as.Date("1950-1-1")
max <- as.Date("2021-12-31")
p + scale_x_date(limits = c(min, max))





### WATER USE (NWIS) ###
## SELECT site, then retrieve data
site_number <- "11519500" ## Scott River
site_number <- "04097540" ## Prairie River - St. Joseph County
# site_number <- "04097528" ## Prairie River - Branch County

## RETRIEVE site info necessary for water use data retrieval
site_info <- readNWISsite(site_number)
state <- site_info$state_cd
county <- site_info$county_cd

## RETRIEVE Water Use - Scott Valley
waterUse_SV <- readNWISuse(
        stateCd = state,
        countyCd = county,
        years = "ALL",
        categories = "ALL",
        convertType = TRUE,
        transform = FALSE
)

# attributes <- colnames(waterUse_SV)
# view(attributes)

waterUSE_SV_select <- waterUse_SV[ , c(5,243)]
# 243 	Irrigation..Total.total.consumptive.use..in.Mgal.d
colnames(waterUSE_SV_select) <- c("Date", "Scott_Irrigation_total_Mgal_d")

# Convert units from Mgal/d to cfs
waterUSE_SV_select$Scott_Irrigation_total_cfs <- ((as.numeric(waterUSE_SV_select$`Scott_Irrigation_total_Mgal_d`)*10^6)/(7.48*86400))

# RETRIEVE Water Use - Prairie River
waterUse_Pr <- readNWISuse(
        stateCd = state,
        countyCd = county,
        years = "ALL",
        categories = "ALL",
        convertType = TRUE,
        transform = FALSE
)

waterUSE_Pr_select <- waterUse_Pr[ , c(5,243)]
colnames(waterUSE_Pr_select) <- c("Date", "Prairie_Irrigation_total_Mgal_d")

# Convert units from Mgal/d to cfs
waterUSE_Pr_select$Prairie_Irrigation_total_cfs <- ((as.numeric(waterUSE_Pr_select$`Prairie_Irrigation_total_Mgal_d`)*10^6)/(7.48*86400))


# COMBINE sites into dataframe
irrigation <- left_join(waterUSE_SV_select, waterUSE_Pr_select, by = "Date") 


###########################################################
###########################################################



### LFSTAT ###
### PREPARE data - needs timestamp broken into columns for day, month, year, flow
lf_SR <- separate(streamflow_SR, "Date", c("year", "month", "day"), "-")
colnames(lf_SR) <- c("year","month", "day", "flow")
lf_SR <- createlfobj(lf_SR)

lf_PR <- separate(streamflow_PR, "Date", c("year", "month", "day"), "-")
colnames(lf_PR) <- c("year","month", "day", "flow")
lf_PR <- createlfobj(lf_PR)


# CALCULATE low flow statistics for daily stream flow data
lfstats <- multistationsreport(lf_PR, lf_SR, indices = c("meanflow", "Q95", "MAM1", "MAM7", 
                                            "MAM10", "MAM30", "MAM90", "baseflowindex", "recession"), 
            recessionmethod = "IRS", recessionseglength = 7, recessionthreshold = 70, 
            recessiontrimIRS = 0.1, lflist = NULL)


### HYDROSTATS ###
PrairieStats <- streamflow_PR
PrairieStats <- ts.format(streamflow_PR, format = "%Y-%m-%d", cols = c(1,2))
PrairieStats <- hydro.year(PrairieStats, hydro.year = "hydro", year.only = FALSE)
PrairieStats <- low.spells(streamflow_PR, quant = 0.1, threshold=NULL, 
                        duration = T, volume = T, plot = T, ann.stats = T, ann.stats.only = F, 
                        hydro.year=FALSE)
### Error in tapply(flow.ts.comp[["Q"]], flow.ts.comp[["hydro.year"]], length) : arguments must have same length


### STREAMDEPLETR ###
## need aquifer parameters

########
# qColNames = c("Year", "Month", "Discharge")
# 
# q_select <- streamflow_monthly_syracuse[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_syracuse <- data_join
# write.csv(q_syracuse, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/Syracuse_Qmo.csv", row.names = FALSE, na = "null")
# 
# q_select <- streamflow_monthly_gardencity[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_gardencity <- data_join
# write.csv(q_gardencity, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/GardenCity_Qmo.csv", row.names = FALSE, na = "null")
# 
# q_select <- streamflow_monthly_dodgecity[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_dodgecity <- data_join
# write.csv(q_dodgecity, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/DodgeCity_Qmo.csv", row.names = FALSE, na = "null")
# 
# q_select <- streamflow_monthly_greatbend[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_greatbend <- data_join
# write.csv(q_greatbend, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/GreatBend_Qmo.csv", row.names = FALSE, na = "null")
# 
# q_select <- streamflow_monthly_wichita[-(1) , c(5:7)] %>% 
#   filter(year_nu > 1949)
# colnames(q_select) = qColNames
# 
# data_join <- left_join(CSVtoJSON, q_select)
# 
# q_wichita <- data_join
# write.csv(q_wichita, "~/GradSchool/Data/Streamflow_Monthly_NWIS/20200915/Wichita_Qmo.csv", row.names = FALSE, na = "null")
