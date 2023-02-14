# database connection/retrieval libraries
library(dataRetrieval)

# data manipulation libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(patchwork)



###############################################################
#### KS WIMAS Database ####

## READ csv files containing wateruse data by county
wusum_Hamilton <- readr::read_csv("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/Data/WIMAS/Hamilton_wusesum.csv")
wusum_Finney <- readr::read_csv("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/Data/WIMAS/Finney_wusesum.csv")
wusum_Stafford <- read.csv("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/Data/WIMAS/Stafford_wusesum.csv")
wusum_Cowley <- readr::read_csv("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/Data/WIMAS/Cowley_wuseirr.csv")

## CONVERT from acres to km^2 --> 1 Acre feet = 1233.48 Cubic meters
wusum_Hamilton$UsageWIMASm3 <- wusum_Hamilton$AF_USED * 1233.48
wusum_Finney$UsageWIMASm3 <- wusum_Finney$AF_USED * 1233.48
wusum_Stafford$UsageWIMASm3 <- wusum_Stafford$AF_USED * 1233.48
wusum_Cowley$UsageWIMASm3 <- wusum_Cowley$AF_USED * 1233.48


##############################################################
#### USGS Database ####

## SET watershed dataframe to work with based on time period
ws_040500MI <- ws_040500MI_hd
ws_110300KS <- ws_110300KS_mhd_mmk
ws_180102CA <- ws_180102CA_hd


## GAGE STATIONS OF INTEREST
i = 1 ## KS_long - Syracuse
i = 2 ## KS_long - Garden City
i = 3 ## KS_long - Dodge City
i = 6 ## KS_long - Walnut R

i = 2 ## KS_hd - Syracuse
i = 3 ## KS_hd - Garden City
i = 5 ## KS_hd - Pawnee
i = 7 ## KS_hd - Rattlesnake C 07142300
i = 12 ## KS_hd - Arkansas City
i = 15 ## KS_hd - Walnut R

site_number <- ws_110300KS$site_no[i]

## RETRIEVE site info necessary for water use data retrieval
site_info <- dataRetrieval::readNWISsite(site_number)
state <- site_info$state_cd
county <- site_info$county_cd

## RETRIEVE Water Use
wu_raw <- dataRetrieval::readNWISuse(
  stateCd = state,
  countyCd = county,
  years = "ALL",
  categories = "ALL",
  convertType = TRUE,
  transform = FALSE
)

## SELECT relevant columns from raw water use data, rename columns--CHOOSE corresponding watershed before running for loop
## 243 	Irrigation..Total.total.consumptive.use..in.Mgal.d
## 231  Irrigation..Total.self.supply.groundwater.withdrawls..fresh..in.Mgal.d
wu_raw <- wu_raw[ , c(5,231)]
## CHANGE - in usage data to NA for missing data and FORCE numeric
wu_raw[wu_raw == "-"] <-  NA
wu_raw[[1]] <- as.numeric(wu_raw[[1]])
wu_raw[[2]] <- as.numeric(wu_raw[[2]])
## CHANGE column names for joining
# colnames(wu_raw) <- c("Date", ws_040500MI$station_nm[i])
colnames(wu_raw) <- c("YEAR", "UsageUSGS")
## CONVERT acre-feet to Mgal/d to m^3/yr
wu_raw$UsageUSGSm3 <- wu_raw$UsageUSGS * 4404.8838 * 365


## MERGE WIMAS and USGS water use dfs
wusum_Hamilton <- left_join(wusum_Hamilton, wu_raw, by = "YEAR")
wusum_Finney <- left_join(wusum_Finney, wu_raw, by = "YEAR")
wusum_Stafford <- left_join(wusum_Stafford, wu_raw, by = "YEAR")
wusum_Cowley <- left_join(wusum_Cowley, wu_raw, by = "YEAR")


## PLOT wateruse 
p_wu_Hamilton <- ggplot2::ggplot(wusum_Hamilton, aes(x = YEAR, y = UsageWIMASm3, group = 1)) +
  geom_line() +
  geom_point(aes(x = YEAR, y = UsageUSGSm3), size = 3) +
  scale_x_continuous(breaks = seq(1955, 2025, 10)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) +
  labs(title = "Hamilton County Groundwater Use (Arkansas River at Syracuse, KS)") + 
  ylab(bquote("Water Use " (m^3))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 20), axis.text = element_text(size = 18), plot.title = element_text(size = 24), legend.title = element_text(size=22), legend.text = element_text(size = 20))
# print(p_wu_Hamilton)
p_wu_Finney <- ggplot2::ggplot(wusum_Finney, aes(x = YEAR, y = UsageWIMASm3, group = 1)) +
  geom_line() +
  geom_point(aes(x = YEAR, y = UsageUSGSm3), size = 3) +
  scale_x_continuous(breaks = seq(1955, 2025, 10)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) +
  labs(title = "Finney County Groundwater Use (Arkansas River at Garden City, KS)") + 
  ylab(bquote("Water Use " (m^3))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 20), axis.text = element_text(size = 18), plot.title = element_text(size = 24), legend.title = element_text(size=22), legend.text = element_text(size = 20))
# print(p_wu_Finney)
p_wu_Stafford <- ggplot2::ggplot(wusum_Stafford, aes(x = YEAR, y = UsageWIMASm3, group = 1)) +
  geom_line() +
  geom_point(aes(x = YEAR, y = UsageUSGSm3), size = 3) +
  scale_x_continuous(breaks = seq(1955, 2025, 10)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) +
  labs(title = "Stafford County Groundwater Use (Rattlesnake Creek at Macksville, KS)") + 
  ylab(bquote("Water Use " (m^3))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 20), axis.text = element_text(size = 18), plot.title = element_text(size = 24), legend.title = element_text(size=22), legend.text = element_text(size = 20))
# print(p_wu_Stafford)
p_wu_Cowley <- ggplot2::ggplot(wusum_Cowley, aes(x = YEAR, y = UsageWIMASm3, group = 1)) +
  geom_line() +
  geom_point(aes(x = YEAR, y = UsageUSGSm3), size = 3) +
  scale_x_continuous(breaks = seq(1955, 2025, 10)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) +
  labs(title = "Cowley County Groundwater Use (Walnut River at Winfield, KS)") + 
  ylab(bquote("Water Use " (m^3))) +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text = element_text(size = 18), plot.title = element_text(size = 24), legend.title = element_text(size=22), legend.text = element_text(size = 20))
# print(p_wu_Cowley)
p_wu_combo <- p_wu_Hamilton + p_wu_Finney + p_wu_Stafford + p_wu_Cowley + plot_layout(nrow = 4, guides = "collect")
print(p_wu_combo)



##########################################################
## SELECT columns for plotting
wusum_Hamilton_select <- wusum_Hamilton[ , c(1, 4, 6)]
colnames(wusum_Hamilton_select) <- c("YEAR", "WIMAS", "USGS")

## MELT df for plotting
wusum_Hamilton_melt <- reshape2::melt(wusum_Hamilton_select, measure.vars = 2:ncol(wusum_Hamilton_select), variable.name = "Database", value.name = "Usage")

p_wu_Hamilton <- ggplot2::ggplot(wusum_Hamilton_melt, aes(x = YEAR, y = Usage, group = Database)) +
  geom_line() +
  geom_point(data = wusum_Hamilton, aes(x = YEAR, y = UsageUSGSm3), size = 3, inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(1955, 2025, 10)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) +
  labs(title = "Hamilton County Groundwater Used (Arkansas River at Syracuse, KS)") + 
  ylab(bquote("Water Use " (m^3))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 20), axis.text = element_text(size = 18), plot.title = element_text(size = 24), legend.title = element_text(size=22), legend.text = element_text(size = 20))
print(p_wu_Hamilton)

