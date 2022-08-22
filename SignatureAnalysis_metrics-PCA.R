# data manipulation libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggfortify)
library(ggthemes)
library(reshape2)
library(stringr)
# data retrieval libraries
library(dataRetrieval)
# data analysis/statistical libraries
library(xts)
library(lfstat)
library(trend)



## SET watershed object to work with based on time period
ws_040500MI <- ws_040500MI_hd
ws_110300KS <- ws_110300KS_hd
ws_180102CA <- ws_180102CA_hd

ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_long
ws_180102CA <- ws_180102CA_long

ws_040500MI <- ws_040500MI_ep
ws_110300KS <- ws_110300KS_ep
ws_180102CA <- ws_180102CA_ep


########## STREAM DEPLETION METRICS ##########
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



## RETRIEVE streamflow data to get dataframe without NAs -- NA's end up being coerced by createlfobject -- SET start date and end date before running for loop

i = 1

# for (i in 1:nrow(ws_040500MI)) {
#         site_number <- ws_040500MI$site_no[i]
for (i in 1:nrow(ws_110300KS)) {
      site_number <- ws_110300KS$site_no[i]
# for (i in 1:nrow(ws_180102CA)) {
#   site_number <- ws_180102CA$site_no[i]
  
  ## RETRIEVE site info and streamflow data for gage station
  site_info <- dataRetrieval::readNWISsite(site_number)
  site_name <- site_info$station_nm
  raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)
 
  ## PREPARE dataframe for calculations 
  sf_select <- subset(raw_daily, select = c(3,4))
  colnames(sf_select) <- c("Date", "Discharge")
  sf_select$year <- as.numeric(format(sf_select[[1]], "%Y"))
  
  ## PREPARE data for use with lfstat - needs timestamp broken into columns for day, month, year, flow
  sf <- separate(sf_select, "Date", c("year", "month", "day"), "-")
  colnames(sf) <- c("year","month", "day", "flow")
  sf <- lfstat::createlfobj(sf, hyearstart = 10)
  sf$hyear <- as.numeric(sf$hyear)
  sf$year <- as.numeric(sf$year)
  
  ## JOIN hydrologic year to sf_select to calculate sf metrics on water years instead of calendar years
  sf_select <- left_join(sf_select, sf[ ,c(3,5)], by = "year")
  
  ## CALCULATE Q90 - 90th percentile of flow that is exceeded 10% of the time
  # Q10 <- sf_select %>% 
  #   summarize(Q10 = quantile(Discharge, probs = 0.9, na.rm = TRUE)) %>% 
  #   #https://www.tutorialspoint.com/how-to-extract-initial-last-or-middle-characters-from-a-string-in-r
  #   as.numeric(str_sub(Q10,-4,-2))
  Q10 <- lfstat::Qxx(sf, Qxx = 10, yearly = FALSE) 
    
  ## CALCULATE annual mean discharge
  meanQ <- sf_select %>%
    na.omit(sf_select) %>% 
    summarize(meanQ = mean(Discharge)) %>% 
    as.numeric(str_sub(meanQ,5,-1))
  
  ## CALCULATE Q50 - 50th percentile of flow that is exceeded 50% of the time
  # Q50 <- sf_select %>% 
  #   summarize(Q50 = quantile(Discharge, probs = 0.5, na.rm = TRUE)) %>% 
  #   as.numeric(str_sub(Q50,-4,-2))
  Q50 <- lfstat::Qxx(sf, Qxx = 50, yearly = FALSE) 
  
  ## CALCULATE Annual Mean Baseflow - Average baseflow for each year
  baseflow <- sf %>%
    na.omit(sf) %>%  
    summarise(MeanBaseflow = mean(baseflow)) %>% 
    as.numeric(str_sub(meanQ,8,-1))
  
  ## CALCULATE MAM7 - Mean Annual Minimum over 7-day period -> 7-day low flow
  MAM7 <- lfstat::MAM(sf, n=7, yearly = TRUE)
  colnames(MAM7) <- c("hyear","MAM7")
  MAM7 <- MAM7 %>% 
    summarise(MAM7 = mean(MAM7)) %>% 
    as.numeric(str_sub(Q95,4,-2))
  
  ## CALCULATE Q90 - 10th percentile of flow that is exceeded 90% of the time
  # Q90 <- sf_select %>% 
  #   summarize(Q90 = quantile(Discharge, probs = 0.1, na.rm = TRUE)) %>% 
  #   as.numeric(str_sub(Q90,-4,-1))
  Q90 <- lfstat::Qxx(sf, Qxx = 90, yearly = FALSE) 
  
  ## CALCULATE Q90 - 90th percentile of flow that is exceeded 95% of the time
  # Q95 <- sf_select %>% 
  #   summarize(Q95= quantile(Discharge, probs = 0.05, na.rm = TRUE)) %>% 
  #   as.numeric(str_sub(Q95,-4,-2))
  Q95 <- lfstat::Qxx(sf, Qxx = 95, yearly = FALSE) 
  
  ## CALCULATE BFI - Baseflow Index for each year
  # BFI <- lfstat::BFI(sf, year = "any", breakdays = NULL, yearly = TRUE)
  # BFI <- data.frame(sf_metrics$Year, BFI)
  # colnames(BFI) <- c("Year", "BFI") 
  # sf_metrics <- left_join(sf_metrics, BFI, by = "Year")
  
  
  ## APPEND metric results to WS dataframe, CHOOSE corresponding watershed before running for loop
  # ws_040500MI$Q10[i] <- as.numeric(Q10)
  # ws_040500MI$MeanQ[i] <- as.numeric(meanQ)
  # ws_040500MI$Q50[i] <- as.numeric(Q50)
  # ws_040500MI$Baseflow[i] <- as.numeric(baseflow)
  # ws_040500MI$MAM7[i] <- as.numeric(MAM7)
  # ws_040500MI$Q90[i] <- as.numeric(Q90)
  # ws_040500MI$Q95[i] <- as.numeric(Q95)
  
  ws_110300KS$Q10[i] <- as.numeric(Q10)
  ws_110300KS$MeanQ[i] <- as.numeric(meanQ)
  ws_110300KS$Q50[i] <- as.numeric(Q50)
  ws_110300KS$Baseflow[i] <- as.numeric(baseflow)
  ws_110300KS$MAM7[i] <- as.numeric(MAM7)
  ws_110300KS$Q90[i] <- as.numeric(Q90)
  ws_110300KS$Q95[i] <- as.numeric(Q95)
 
  # ws_180102CA$Q10[i] <- as.numeric(Q10)
  # ws_180102CA$MeanQ[i] <- as.numeric(meanQ)
  # ws_180102CA$Q50[i] <- as.numeric(Q50)
  # ws_180102CA$Baseflow[i] <- as.numeric(baseflow)
  # ws_180102CA$MAM7[i] <- as.numeric(MAM7)
  # ws_180102CA$Q90[i] <- as.numeric(Q90)
  # ws_180102CA$Q95[i] <- as.numeric(Q95)
  
  
  
  ## SAVE CSV of streamgage metrics, CHOOSE watershed before running for loop --> can also write to SQL database using SQLconnect R script
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_MI_hd/MI_", i, "_metrics_hd.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_KS_hd/KS_", i, "_metrics_hd.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_CA_hd/CA_", i, "_metrics_hd.csv"))
  
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_MI_long/MI_", i, "_metrics_long.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_KS_long/KS_", i, "_metrics_long.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_CA_long/CA_", i, "_metrics_long.csv"))
  
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_MI_ep/MI_", i, "_metrics_ep.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_KS_ep/KS_", i, "_metrics_ep.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_CA_ep/CA_", i, "_metrics_ep.csv"))

  
}


## SAVE resulting watershed object to dataframe
ws_040500MI_hd <- ws_040500MI
ws_110300KS_hd <- ws_110300KS
ws_180102CA_hd <- ws_180102CA

ws_040500MI_long <- ws_040500MI
ws_110300KS_long <- ws_110300KS
ws_180102CA_long <- ws_180102CA

ws_040500MI_ep <- ws_040500MI
ws_110300KS_ep <- ws_110300KS
ws_180102CA_ep <- ws_180102CA


## SAVE CSV of watershed data, CHOOSE watershed before running for loop
write.csv(ws_040500MI, "~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_040500MI_hd_1960-2021.csv")
write.csv(ws_110300KS, "~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_110300KS_hd_1963-2021.csv")
write.csv(ws_180102CA,"~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_180102CA_hd_1962-2021.csv")

write.csv(ws_040500MI, "~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_040500MI_long.csv")
write.csv(ws_110300KS, "~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_110300KS_long.csv")
write.csv(ws_180102CA,"~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_180102CA_long.csv")

write.csv(ws_040500MI, "~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_040500MI_ep.csv")
write.csv(ws_110300KS, "~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_110300KS_ep.csv")
write.csv(ws_180102CA,"~/GradSchool/DiscoverStreams/outputs/watershed_info/ws_180102CA_ep.csv")



#############################################################
### PCA ###
## SET watershed object to work with based on time period
ws_040500MI <- ws_040500MI_hd
ws_110300KS <- ws_110300KS_hd
ws_180102CA <- ws_180102CA_hd

ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_long
ws_180102CA <- ws_180102CA_long

ws_040500MI <- ws_040500MI_ep
ws_110300KS <- ws_110300KS_ep
ws_180102CA <- ws_180102CA_ep

## ISOLATE station names, metrics labels, station IDs, and metrics for PCA & plotting options
pca_stations <- ws_040500MI$station_nm
pca_metrics <- colnames(ws_040500MI[ , 24:29])
pca_ids <- ws_040500MI$id
pca_select <- ws_040500MI[ , c("Q10", "MeanQ", "Q50", "Baseflow", "MAM7", "Q90", "Q95")]

pca_stations <- ws_110300KS$station_nm
pca_metrics <- colnames(ws_110300KS[ , c("Q10", "MeanQ", "Q50", "Baseflow", "MAM7", "Q90", "Q95")])
pca_ids <- ws_110300KS$id
pca_select <- ws_110300KS[ , c("Q10", "MeanQ", "Q50", "Baseflow", "MAM7", "Q90", "Q95")]

pca_stations <- ws_180102CA$station_nm
pca_metrics <- colnames(ws_180102CA[ , 24:29])
pca_ids <- ws_180102CA$id
pca_select <- ws_180102CAS[ , c("Q10", "MeanQ", "Q50", "Baseflow", "MAM7", "Q90", "Q95")]


## REFORMAT columns from lists to numerics for compatibility with princomp function
pca_select$Q90 <- as.numeric(pca_select$Q90)
pca_select$MeanQ <- as.numeric(pca_select$MeanQ)
pca_select$Baseflow <- as.numeric(pca_select$Baseflow)
pca_select$MAM7 <- as.numeric(pca_select$MAM7)

## COMPUTE PCA on metrics per station for each watershed time period 
pca_040500MI_hd <- princomp(pca_select, cor = TRUE)
pca_040500MI_long <- princomp(pca_select, cor = TRUE)
pca_040500MI_ep <- princomp(pca_select, cor = TRUE)

pca_110300KS_hd <- princomp(pca_select, cor = TRUE)
pca_110300KS_long <- princomp(pca_select, cor = TRUE)
pca_110300KS_ep <- princomp(pca_select, cor = TRUE)

pca_180102CA_hd <- princomp(pca_select, cor = TRUE)
pca_180102CA_long <- princomp(pca_select, cor = TRUE)
pca_180102CA_ep <- princomp(pca_select, cor = TRUE)

## SUMMARY of PCA results
summary(pca_040500MI_hd)
loadings(pca_040500MI_hd)
str(pca_040500MI_hd)

summary(pca_110300KS_hd)
loadings(pca_110300KS_hd)
str(pca_110300KS_hd)

summary(pca_180102CA_hd)
loadings(pca_180102CA_hd)
str(pca_180102CA_hd)

summary(pca_040500MI_long)
loadings(pca_040500MI_long)
str(pca_040500MI_long)

summary(pca_110300KS_long)
loadings(pca_110300KS_long)
str(pca_110300KS_long)

summary(pca_180102CA_long)
loadings(pca_180102CA_long)
str(pca_180102CA_long)

summary(pca_040500MI_ep)
loadings(pca_040500MI_ep)
str(pca_040500MI_ep)

summary(pca_110300KS_ep)
loadings(pca_110300KS_ep)
str(pca_110300KS_ep)

summary(pca_180102CA_ep)
loadings(pca_180102CA_ep)
str(pca_180102CA_ep)


## SELECT PCA results for plotting
pca_results <- pca_040500MI_hd
pca_results <- pca_110300KS_hd
pca_results <- pca_180102CA_hd

pca_results <- pca_040500MI_long
pca_results <- pca_110300KS_long
pca_results <- pca_180102CA_long

pca_results <- pca_040500MI_ep
pca_results <- pca_110300KS_ep
pca_results <- pca_180102CA_ep

## SCREE PLOT of PCA components
screeplot(pca_results, type = "lines", main = "Screeplot of PCA Variances for Streamflow Metrics")

## SCREE PLOT with Variance Explained on y-axis instead of # of variances
## https://www.statology.org/principal-components-analysis-in-r/
pca_var_explained <- pca_results$sdev^2 / sum(pca_results$sdev^2)
qplot(x = c(1:7), pca_var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot of PCA Variances for Streamflow Metrics") +
  scale_x_continuous(breaks = 1:6) +
  ylim(0, 1)

## PLOT PCA scores
autoplot(pca_results, data = pca_select, colour = as.factors(pca_stations))

## MULTIPLY by -1 to adjust for R package calculations
pca_results$loadings <- pca_results$loadings
## PLOT PCA loadings
plot(pca_results$loadings, pch = 16, col = c("#9451cc", "#06ccd6", "#d19d43", "#0939a5", "#259f3d", "#a11437", "#12173e"), main = "PCA  Loadings for Streamflow Metrics in Middle Arkansas River Watershed", xlim = c(-1, 1), ylim = c(-1, 1))
segments(c(-1, 0), c(0, -1), c(1, 0), c(0, 1))
text(pca_results$loadings,
     labels = pca_metrics,
     cex = 1, pos = 3)
legend("topright", legend = pca_metrics, pch = 16, col = c("#9451cc", "#06ccd6", "#d19d43", "#0939a5", "#259f3d", "#a11437", "#12173e"))


## PLOT PCA biplot of scores and loadings
biplot(pca_results, xlabs = pca_ids)



