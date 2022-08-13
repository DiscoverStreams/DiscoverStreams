# database connection/retrieval libraries
library(readr)
library(odbc)
library(RMariaDB)
library(DBI)

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

## WRTIE watershed site information to mySQL database for use with webapp
dbWriteTable(con_UW, "interact_sitedata_SELakeMI", ws_040500MI, overwrite = TRUE)
dbWriteTable(con_UW, "interact_sitedata_MidArkansas", ws_110300KS, overwrite = TRUE)
dbWriteTable(con_UW, "interact_sitedata_Klamath", ws_180102CA, overwrite = TRUE)

## WRITE/UPDATE daily streamflow data to SQL database of corresponding watershed
dbWriteTable(con_UW, "interact_streamflow_SELakeMI", sf_040500MI, overwrite = TRUE)
dbWriteTable(con_UW, "interact_streamflow_MidArkansas", sf_110300KS, overwrite = TRUE)
dbWriteTable(con_UW, "interact_streamflow_Klamath", sf_180102CA, overwrite = TRUE)