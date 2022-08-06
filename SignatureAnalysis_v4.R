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


# SET streamflow dataframe for loop
huc040500MI_ws <- huc040500MI_ws_long
huc110300KS_ws <- huc110300KS_ws_long
huc180102CA_ws <- huc180102CA_ws_long

# ## SET watershed object to work with 
huc040500MI_ws <- huc040500MI_ws_1958_2021_sel
huc110300KS_ws <- huc110300KS_ws_1962_2021_sel
huc180102CA_ws <- huc180102CA_ws_1962_2021_sel


########## STREAM DEPLETION METRICS ##########
## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- "1960-01-01"  # Michigan
start_date <- "1963-01-01"  # Kansas
start_date <- "1962-01-01"  # California
start_date <- ""
end_date <- "2021-12-31"

## CREATE separate df of consecutive years to ensure all years represented in plot when NA years are dropped
sf_metrics_year_MI_hd <- data.frame(seq(1960, 2021, by = 1))
colnames(sf_metrics_year_MI_hd) <- "Year"
sf_metrics_year_KS_hd <- data.frame(seq(1963, 2021, by = 1))
colnames(sf_metrics_year_KS_hd) <- "Year"
sf_metrics_year_CA_hd <- data.frame(seq(1962, 2021, by = 1))
colnames(sf_metrics_year_CA_hd) <- "Year"

sf_metrics_year_MI_long <- data.frame(seq(1901, 2021, by = 1))
colnames(sf_metrics_year_MI_long) <- "Year"
sf_metrics_year_KS_long <- data.frame(seq(1902, 2021, by = 1))
colnames(sf_metrics_year_KS_long) <- "Year"
sf_metrics_year_CA_long <- data.frame(seq(1904, 2021, by = 1))
colnames(sf_metrics_year_CA_long) <- "Year"

## START loop at i = 2 because i = 1 is column "Date"
# i = 2

## UN-COMMENT corresponding watershed lines and save location before running for loop
# for (i in 2:ncol(huc040500MI_sf)) {
#   site_name <- colnames(huc040500MI_sf[i])
#   sf_select <- data.frame(huc040500MI_sf[[1]], huc040500MI_sf[[i]])
# for (i in 2:ncol(huc110300KS_sf)) {
#   site_name <- colnames(huc110300KS_sf[i])
#   sf_select <- data.frame(huc110300KS_sf[[1]], huc110300KS_sf[[i]])
# for (i in 2:ncol(huc180102CA_sf)) {
#   site_name <- colnames(huc180102CA_sf[i])
#   sf_select <- data.frame(huc180102CA_sf[[1]], huc180102CA_sf[[i]])



## RETRIEVE streamflow data to get dataframe without NAs -- NA's end up being coerced by createlfobject

i = 1

# for (i in 1:nrow(huc040500MI_ws)) {
#         site_number <- huc040500MI_ws$site_no[i]
for (i in 1:nrow(huc110300KS_ws)) {
      site_number <- huc110300KS_ws$site_no[i]
# for (i in 1:nrow(huc180102CA_ws)) {
#   site_number <- huc180102CA_ws$site_no[i]
  
  ## RETRIEVE site info and streamflow data for gage station
  site_info <- dataRetrieval::readNWISsite(site_number)
  site_name <- site_info$station_nm
  raw_daily <- dataRetrieval::readNWISdv(site_number, parameter_code, start_date, end_date)
 
  ## PREPARE dataframe for calculations 
  sf_select <- subset(raw_daily, select = c(3,4))
  colnames(sf_select) <- c("Date", "Discharge")
  
  
  ## CALCULATE Q90 - 90th percentile of flow 
  Q90 <- sf_select %>% 
    summarize(Q90 = quantile(Discharge, probs = 0.9, na.rm = TRUE)) %>% 
    #https://www.tutorialspoint.com/how-to-extract-initial-last-or-middle-characters-from-a-string-in-r
    as.numeric(str_sub(Q90,-4,-2))
    
  ## CALCULATE annual mean discharge
  annual_mean <- sf_select %>%
    na.omit(sf_select) %>% 
    summarize(MeanDischarge = mean(Discharge))
  
  ## PREPARE data for use with lfstat - needs timestamp broken into columns for day, month, year, flow
  sf <- separate(sf_select, "Date", c("year", "month", "day"), "-")
  colnames(sf) <- c("year","month", "day", "flow")
  sf <- lfstat::createlfobj(sf)
  
  ## CALCULATE Annual Mean Baseflow - Average baseflow for each year
  annual_mean_baseflow <- sf %>%
    na.omit(sf) %>%  
    summarise(MeanBaseflow = mean(baseflow))
  
  ## CALCULATE MAM7 - Mean Annual Minimum over 7-day period -> 7-day low flow
  MAM7 <- lfstat::MAM(sf, n=7, year = "any", breakdays = NULL, yearly = TRUE)
  colnames(MAM7) <- c("Year","MAM7")
  MAM7 <- MAM7 %>% 
    summarise(MAM7 = mean(MAM7))
  
  ## CALCULATE Q10 - 10th percentile of flow
  Q10 <- sf_select %>% 
    summarize(Q10 = quantile(Discharge, probs = 0.1, na.rm = TRUE)) %>% 
    as.numeric(str_sub(Q10,-4,-2))
  
  ## CALCULATE Q50 - 50th percentile of flow
  Q50 <- sf_select %>% 
    summarize(Q50 = quantile(Discharge, probs = 0.5, na.rm = TRUE)) %>% 
    as.numeric(str_sub(Q50,-4,-2))
  
  ## CALCULATE Q90 - 90th percentile of flow 
  Q95 <- sf_select %>% 
    summarize(Q95= quantile(Discharge, probs = 0.95, na.rm = TRUE)) %>% 
    as.numeric(str_sub(Q95,-4,-2))
  
  ## CALCULATE BFI - Baseflow Index for each year
  # BFI <- lfstat::BFI(sf, year = "any", breakdays = NULL, yearly = TRUE)
  # BFI <- data.frame(sf_metrics$Year, BFI)
  # colnames(BFI) <- c("Year", "BFI") 
  # sf_metrics <- left_join(sf_metrics, BFI, by = "Year")
  
  
  ## APPEND metric results to WS dataframe, CHOOSE corresponding watershed before running for loop
  # huc040500MI_ws$Q95[i] <- Q95
  # huc040500MI_ws$Q90[i] <- Q90
  # huc040500MI_ws$MeanQ[i] <- annual_mean
  # huc040500MI_ws$Q50[i] <- Q50
  # huc040500MI_ws$Baseflow[i] <- annual_mean_baseflow
  # huc040500MI_ws$Q10[i] <- Q10
  # huc040500MI_ws$MAM7[i] <- MAM7
  
  huc110300KS_ws$Q95[i] <- as.numeric(Q95)
  huc110300KS_ws$Q90[i] <- as.numeric(Q90)
  huc110300KS_ws$MeanQ[i] <- as.numeric(annual_mean)
  huc110300KS_ws$Q50[i] <- as.numeric(Q50)
  huc110300KS_ws$Baseflow[i] <- as.numeric(annual_mean_baseflow)
  huc110300KS_ws$Q10[i] <- as.numeric(Q10)
  huc110300KS_ws$MAM7[i] <- as.numeric(MAM7)

  # huc180102CA_ws$Q95[i] <- Q95
  # huc180102CA_ws$Q90[i] <- Q90
  # huc180102CA_ws$MeanQ[i] <- annual_mean
  # huc180102CA_ws$Q50[i] <- Q50
  # huc180102CA_ws$Baseflow[i] <- annual_mean_baseflow
  # huc180102CA_ws$Q10[i] <- Q10
  # huc180102CA_ws$MAM7[i] <- MAM7
  
  ## SAVE CSV of streamgage metrics, CHOOSE watershed before running for loop --> in the future these should write to SQL database
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_MI/MI_", i, "_metrics.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_KS/KS_", i, "_metrics.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_CA/CA_", i, "_metrics.csv"))
  
 ## ALIGN "Year" column so that NA years are preserved for plotting, CHOOSE watershed & time frame before running for loop
  # sf_metrics <- left_join(sf_metrics_year_MI_hd, sf_metrics, by = "Year")
  # sf_metrics <- left_join(sf_metrics_year_KS_hd, sf_metrics, by = "Year")
  # sf_metrics <- left_join(sf_metrics_year_CA_hd, sf_metrics, by = "Year")
  
  # sf_metrics <- left_join(sf_metrics_year_MI_long, sf_metrics, by = "Year")
  # sf_metrics <- left_join(sf_metrics_year_KS_long, sf_metrics, by = "Year")
  # sf_metrics <- left_join(sf_metrics_year_CA_long, sf_metrics, by = "Year")

  
}



  ## PREPARE resulting metrics for plotting
# sf_metrics[sf_metrics == 0] <- 0.0001
sf_metrics_melt <- sf_metrics[ , -c(7,8)]  
sf_metrics_melt <- reshape2::melt(sf_metrics_melt, measure.vars = 2:6, variable.name = "Metric", value.name = "Discharge")
# sf_metrics_melt <- data_melt[!is.na(data_melt$Discharge), ]
  
  ## SAVE plot of streamgage metrics, CHOOSE watershed before running for loop
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_MI/MI_", i, "_metrics.png", sep = ""), width = 757, height = 464, unit = "px")
  png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_KS/KS_", i, "_metrics.png", sep = ""), width = 757, height = 464, unit = "px")
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_CA/CA_", i, "_metrics.png", sep = ""), width = 757, height = 464, unit = "px")

  ## PLOT options
  # p_metrics_melt <- ggplot2::ggplot(sf_metrics_melt, aes(x = Year, y = Discharge, color = Metric, linetype = Metric, size = Metric)) +
  #   geom_line() +
  #   scale_color_manual(values = c("royalblue4", "hotpink4", "mediumvioletred", "maroon1", "darkgoldenrod2", "deepskyblue3", "black")) +
  #   scale_linetype_manual(values = c(1, 1, 1, 1, 1, 1, 0)) +
  #   scale_size_manual(values = c(1.3, 1.05, 1.05, 1.05, 1.3, 1.5, 0)) +
  #   scale_y_log10() +
  #   ggtitle(paste(site_name, " METRICS")) +
  #   ylab(bquote("Discharge " (ft^3/s)))
    
  
  p_metrics_melt <- ggplot2::ggplot(sf_metrics_melt, aes(x = Year, y = Discharge, color = Metric, linetype = Metric, size = Metric)) +
    geom_ribbon(data = sf_metrics, mapping = aes(x = Year, ymin = Q10, ymax = Q90), fill = "grey70", inherit.aes = FALSE) +
    geom_line() +
    scale_color_manual(values = c("black", "deepskyblue3", "royalblue", "orchid", "black")) +
    scale_linetype_manual(values = c(1, 1, 1, 1, 1)) +
    scale_size_manual(values = c(0.5, 1.1, 1.1, 1.1, 0.5)) +
    scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
    scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    # scale_x_continuous(breaks = seq(1900, 2020, by = 20)) +
    # ggtitle(paste(site_name, " METRICS")) +
    labs(title = stringr::str_wrap(site_name, 30)) +
    theme(axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30), axis.text = element_text(size = 34), plot.title = element_text(size = 32), legend.position = "none") +
    ylab(bquote("Discharge " (ft^3/s)))

  ## WRITE plot to local device
  print(p_metrics_melt)
  
  # 
  # ## PLOT options
  # p_metrics <- ggplot2::ggplot(sf_metrics, aes(x = Year)) + 
  #   geom_ribbon(aes(ymin = Q10, ymax = Q90), fill = "grey70", show.legend = TRUE) +
  #   geom_line(aes(y = MeanDischarge), color = "deepskyblue2", size = 1.2) +
  #   geom_line(aes(y = MeanBaseflow), color = "royalblue", size = 1.2, show.legend = TRUE) +
  #   geom_line(aes(y = MAM7), color = "orchid", size = 1.2, show.legend = TRUE) +
  #   scale_y_log10() +
  #   scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  #   ggtitle(paste(site_name, " METRICS")) +
  #   theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text = element_text(size = 13)) +
  #   ylab(bquote("Discharge " (ft^3/s)))
  
  ## WRITE plot to local device
  # print(p_metrics)
  
  ## CLOSE PNG connection
  # Sys.sleep(5)
  dev.off()
  
  
  
}

## SAVE resulting watershed object 
huc040500MI_ws_1958_2021_sel <- huc040500MI_ws
huc110300KS_ws_1962_2021_sel <- huc110300KS_ws
huc180102CA_ws_1962_2021_sel <- huc180102CA_ws

huc040500MI_ws_long <- huc040500MI_ws
huc110300KS_ws_long <- huc110300KS_ws
huc180102CA_ws_long <- huc180102CA_ws

## SAVE CSV of watershed data, CHOOSE watershed before running for loop
write.csv(huc040500MI_ws, "~/GradSchool/DiscoverStreams/outputs/watershed_info/huc040500MI_ws_1960-2021.csv")
write.csv(huc110300KS_ws, "~/GradSchool/DiscoverStreams/outputs/watershed_info/huc110300KS_ws_1963-2021.csv")
write.csv(huc180102CA_ws,"~/GradSchool/DiscoverStreams/outputs/watershed_info/huc180102CA_ws_1962-2021.csv")

write.csv(huc040500MI_ws, "~/GradSchool/DiscoverStreams/outputs/watershed_info/huc040500MI_ws_long.csv")
write.csv(huc110300KS_ws, "~/GradSchool/DiscoverStreams/outputs/watershed_info/huc110300KS_ws_long.csv")
write.csv(huc180102CA_ws,"~/GradSchool/DiscoverStreams/outputs/watershed_info/huc180102CA_ws_long.csv")

## SAVE resulting streamflow metrics to dataframe
# huc040500MI_sfmetrics_long <- sf_metrics
# huc110300KS_sfmetrics_long <- sf_metrics
# huc180102CA_sfmetrics_long <- sf_metric


#############################################################
### PCA ###
# ISOLATE station names, metrics labels, and station IDs for plotting
pca_stations <- huc110300KS_ws$station_nm
pca_metrics <- colnames(huc110300KS_ws[ , 24:29])
pca_ids <- huc110300KS_ws$id
# SELECT isolated metrics for PCA analysis
pca_select <- huc110300KS_ws[ , 24:29]
# REFORMAT columns from lists to numeric for compatibility with princomp function
pca_select$Q90 <- as.numeric(pca_select$Q90)
pca_select$MeanQ <- as.numeric(pca_select$MeanQ)
pca_select$Baseflow <- as.numeric(pca_select$Baseflow)
pca_select$MAM7 <- as.numeric(pca_select$MAM7)
# COMPUTE PCA on metrics per station  
pca_huc110300KS_hd <- princomp(pca_select, cor = TRUE)
pca_huc110300KS_long <- princomp(pca_select, cor = TRUE)
# SUMMARY of PCA results
summary(pca_huc110300KS_hd)
summary(pca2_huc110300KS_long)
loadings(pca_huc110300KS_hd)
str(pca_huc110300KS_hd)

# SCREE PLOT of PCA components
screeplot(pca_huc110300KS_long, type = "lines", main = "Screeplot of PCA Variances for Streamflow Metrics")
screeplot(pca2_huc110300KS_long, type = "lines")
# SCREE PLOT with Variance Explained on y-axis instead of # of variances
# https://www.statology.org/principal-components-analysis-in-r/
pca_var_explained <- pca_huc110300KS_long$sdev^2 / sum(pca_huc110300KS_long$sdev^2)
qplot(x = c(1:6), pca_var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot of PCA Variances for Streamflow Metrics") +
  scale_x_continuous(breaks = 1:6) +
  ylim(0, 1)

# PLOT PCA results

# plot(pca_huc110300KS_long$scores, pch = 16, col = as.factor(pca_stations))
autoplot(pca_huc110300KS_long, data = pca_select, colour = "station_nm")

plot(pca_huc110300KS_long$loadings, pch = 16, col = as.factor(pca_metrics), main = "PCA Loadings for Streamflow Metrics in Middle Arkansas River Watershed")
text(pca_huc110300KS_long$loadings,
     labels = pca_metrics,
     cex = 0.6, pos = 3, size = 32)
legend("topright", legend = pca_metrics, col = as.factor(pca_metrics))

biplot(pca_huc110300KS_long, xlabs = pca_stations)






