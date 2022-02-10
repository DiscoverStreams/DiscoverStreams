#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dataRetrieval)


# Run once, no need to change parameters for streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- ""
end_date <- ""


### RETRIEVE STREAMFLOW DATA FROM NWIS###
## RETRIEVE streamflow data from NWIS
site_number <- "11519500" ## Scott R
raw_daily <- readNWISdv(site_number, parameter_code, start_date, end_date)
raw_daily <- drop_columns(raw_daily, c("site_no", "agency_cd", "X_00060_00003_cd"))

## REFORMAT data - Scott River, CA
colnames(raw_daily) <- c("Date", "Scott_Discharge_cfs")
raw_daily$Scott_Discharge_cfs <- as.numeric(raw_daily$Scott_Discharge_cfs)
streamflow_SR <- raw_daily

## RETRIEVE streamflow data from NWIS
site_number <- "04097540" ## Prairie R
raw_daily <- readNWISdv(site_number, parameter_code, start_date, end_date)
raw_daily <- drop_columns(raw_daily, c("site_no", "agency_cd", "X_00060_00003_cd"))

## REFORMAT data - Prairie River, MI
colnames(raw_daily) <- c("Date", "Prairie_Discharge_cfs")
streamflow_PR <- raw_daily

## COMBINE sites into dataframe
streamflow <- left_join(streamflow_SR, streamflow_PR, by = "Date") 


### WATER USE (NWIS) ###
## SELECT site, then RETRIEVE site info necessary for water use data retrieval
site_number <- "11519500" ## Scott River

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

waterUSE_SV_select <- waterUse_SV[ , c(5,243)]
# 243 	Irrigation..Total.total.consumptive.use..in.Mgal.d
colnames(waterUSE_SV_select) <- c("Date", "Scott_Irrigation_total_Mgal_d")

# Convert units from Mgal/d to cfs
waterUSE_SV_select$Scott_Irrigation_total_cfs <- ((as.numeric(waterUSE_SV_select$`Scott_Irrigation_total_Mgal_d`)*10^6)/(7.48*86400))


## SELECT site, then RETRIEVE site info necessary for water use data retrieval
site_number <- "04097540" ## Prairie River - St. Joseph County

site_info <- readNWISsite(site_number)
state <- site_info$state_cd
county <- site_info$county_cd

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


### SHINY APP ###
dataset <- streamflow
dataset2 <- irrigation

fluidPage(
    
    titlePanel("DiscoverStreams"),
    
    sidebarPanel(
        
        # sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
        #             value=min(1000, nrow(dataset)), step=500, round=0),
        
        # selectInput('x1', 'X - Streamflow', names(dataset)),
        selectInput('y1', 'Y - Streamflow', names(dataset), names(dataset)[[2]]),
        
        checkboxInput('log', 'Streamflow Log10 Y-axis'),
        
        # selectInput('x2', 'X - Water Use', names(dataset2)),
        selectInput('y2', 'Y - Water Use', names(dataset2), names(dataset2)[[2]]),
        # selectInput('color', 'Color', c('None', names(dataset))),
        
        # textOutput('summary1')
        
        
    ),
    
    mainPanel(
        plotOutput('plot1'),
        plotOutput('plot2')
    )
)
