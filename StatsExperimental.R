# database connection/retrieval libraries
library(readr)
library(dataRetrieval)

# data manipulation libraries
# library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(stringr)
# library(gridExtra)
library(patchwork)

# data analysis/statistical libraries
library(xts)
library(lfstat)
library(trend)
library(zoo)


## SET watershed dataframe to work with based on time period
ws_040500MI <- ws_040500MI_hd
ws_110300KS <- ws_110300KS_hd
ws_180102CA <- ws_180102CA_hd

ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_long
ws_180102CA <- ws_180102CA_long

ws_040500MI <- ws_040500MI_ep
ws_110300KS <- ws_110300KS_ep
ws_180102CA <- ws_180102CA_ep


## Run once, NEED to change parameters for NWIS streamflow data retrieval from NWIS
parameter_code <- c("00060")
parameter_names <- c("Discharge, cubic feet per second")
start_date <- "1959-10-01"  # Michigan HD data
start_date <- "1962-10-01"  # Kansas HD data
start_date <- "1961-10-01"  # California HD data
start_date <- "1951-10-01"  # All 1951 data
start_date <- ""  # Long and EP data
end_date <- "2022-09-30" # HD, long, and 1951 data
end_date <- "1940-09-30" # EP data


## LOOP 1 - metrics calculated over entire time period -> RETRIEVE streamflow data to get dataframe without NAs -- NA's end up being coerced by createlfdataframe -- SET start date and end date before running for loop
i = 1 ## KS_long - Syracuse
i = 3 ## KS_long - Dodge City
i = 6 ## KS_long - Walnut R

# for (i in 1:nrow(ws_040500MI)) {
#         site_number <- ws_040500MI$site_no[i]
# for (i in 1:nrow(ws_110300KS)) {
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
  
  ## CONVERT from ft^3/s to m^3/s
  sf_select$Discharge <- sf_select$Discharge / 35.3146667
  
  ## PREPARE data for use with lfstat - needs timestamp broken into columns for day, month, year, flow
  sf <- separate(sf_select, "Date", c("year", "month", "day"), "-")
  colnames(sf) <- c("year","month", "day", "flow")
  sf <- createlfobj(sf, hyearstart = 10)
  sf$hyear <- as.numeric(sf$hyear)
  sf$year <- as.numeric(sf$year)
  
  ## JOIN hydrologic year to sf_select to calculate sf metrics on water years instead of calendar years
  sf_select <- left_join(sf_select, sf[ ,c(3,5)], by = "year")
  
  ########### CALCULATE base stats ##############
  ## CALCULATE Q10 - probability flow will be exceeded 10% of the time (P90)
  Q10 <- Qxx(sf, Qxx = 10, yearly = TRUE)
  colnames(Q10) <- c("hyear", "Q10")
  
  sf_metrics <- Q10
  
  
  ## CALCULATE annual mean discharge
  # meanQ <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(MeanQ = mean(Discharge))
  meanQ <- meanflow(sf, yearly = TRUE)
  colnames(meanQ) <- c("hyear", "MeanQ")
  
  sf_metrics <- left_join(sf_metrics, meanQ, by = "hyear")
  
  ## CALCULATE Q50 - probability flow will be exceeded 50% of the time (P50)
  Q50 <- Qxx(sf, Qxx = 50, yearly = TRUE)
  colnames(Q50) <- c("hyear", "Q50")
  
  sf_metrics <- left_join(sf_metrics, Q50, by = "hyear")
  
  ## CALCULATE Annual Mean Baseflow  using sf from lfstat - Average baseflow for each year
  baseflow <- sf %>% 
    group_by(hyear) %>%
    na.omit(sf) %>% 
    summarize(MeanBaseflow = mean(Baseflow))
  baseflow$hyear <- as.numeric(baseflow$hyear)
  sf_metrics <- left_join(sf_metrics, baseflow, by = "hyear")
  
  ## CALCULATE MAM7 - Mean Annual Minimum over 7-day period -> 7-day low flow
  # MAM7 <- MAM(sf, n=7, yearly = TRUE)
  # colnames(MAM7) <- c("hyear","MAM7")
  # sf_metrics <- left_join(sf_metrics, MAM7, by = "hyear")
  
  ## CALCULATE Q10 - probability flow will be exceeded 90% of the time (P10)
  Q90 <- Qxx(sf, Qxx = 90, yearly = TRUE)
  colnames(Q90) <- c("hyear", "Q90")
  
  sf_metrics <- left_join(sf_metrics, Q90, by = "hyear")
  
  ## CALCULATE Q95 - probability flow will be exceeded 95% of the time (P05)
  Q95 <- Qxx(sf, Qxx = 95, yearly = TRUE)
  colnames(Q95) <- c("hyear", "Q95")
  
  sf_metrics <- left_join(sf_metrics, Q95, by = "hyear")
  
  ## CALCULATE Q90-Q10 spread
  # j = 3
  
  # sf_metrics$Q90Q10 <- NA

  # for (j in 1:nrow(sf_metrics)) {
  #   if (sf_metrics$Q10[j] != 0 & sf_metrics$Q90[j] == 0) {
  #     # If Q90 = 0 and Q10 is large (> long term average flow), then Q10-Q90 = Q10 / scaled by a very small number to represent no flow conditions
  #     sf_metrics$Q90Q10[j] <- log(sf_metrics$Q10[j]) / log(1.00001)
  #   } else if (sf_metrics$Q10[j] == 0 & sf_metrics$Q90[j] == 0) {
  #     sf_metrics$Q90Q10[j] <- 0.000001
  #   } else if (sf_metrics$Q10[j] != 0 & sf_metrics$Q90[j] != 0) {
  #     sf_metrics$Q90Q10[j] <- (log(sf_metrics$Q10[j]) - log(sf_metrics$Q90[j]))
  #   }
  # 
  # }
  
 j=1
 
  for (j in 1:nrow(sf_metrics)) {
    if (is.na(sf_metrics$Q10[j]) | is.na(sf_metrics$Q90[j])) {
      sf_metrics$Q90Q10[j] <- NA
    } else if (sf_metrics$Q10[j] == 0 & sf_metrics$Q90[j] == 0) {
      sf_metrics$Q90Q10[j] <- NA
    } else if (sf_metrics$Q10[j] != 0 & sf_metrics$Q90[j] != 0) {
      sf_metrics$Q90Q10[j] <- (log(sf_metrics$Q10[j]) - log(sf_metrics$Q90[j])) / (log(sf_metrics$Q10[i]))
    }
    
  }
  
  # replace 0's with very small non-zero -> generates -inf
  # adjust for log scale, look at moving average, still take MK 

  # diff <- sf_metrics[ , c("hyear", "Q10", "Q90")]
  # diff$Q90Q10 <- diff$Q10 - diff$Q90
  # maxQ90Q10 <- max(diff$Q90Q10, na.rm = TRUE)
  # minQ90Q10 <- min(diff$Q90Q10, na.rm = TRUE)
  # 
  # for (j in 1:nrow(sf_metrics)) {
  #   if (sf_metrics$Q10[j] == 0 & sf_metrics$Q90[j] == 0) {
  #     sf_metrics$Q90Q10[j] <- 0
  #   } else if (sf_metrics$Q10[j] != 0 & sf_metrics$Q90[j] != 0) {
  #     sf_metrics$Q90Q10[j] <- (sf_metrics$Q10[j] - sf_metrics$Q90[j]) / maxQ90Q10
  #     # sf_metrics$Q90Q10[j] <- (sf_metrics$Q10[j] - sf_metrics$Q90[j]) / sf_metrics$Q10[j]
  #     # sf_metrics$Q90Q10[j] <- (sf_metrics$Q10[j] - sf_metrics$Q90[j]) / ws_110300KS$MeanQ[i]
  #   }
  # }
  
 
  ## CALCULATE Q90Q10 spread to look for trends in how the difference between the highest and lowest flows have changed across the period of record
  # sf_metrics$Q90Q10 <- sf_metrics$Q10 - sf_metrics$Q90
  
  ## CALCULATE moving-average of Q90Q10 metric, then join with sf_metrics df
  # sf_metrics$Q90Q10ma5yr <- zoo::rollapply(sf_metrics$Q90Q10, 5, mean, align = "right", fill = NA)
  sf_metrics$Q90Q10ma3yr <- zoo::rollapply(sf_metrics$Q90Q10, 3, mean, align = "right", fill = NA)
  
  
  ## CALCULATE Linear Regression Model for change in discharge over change in time
  # lr_Baseflow <- lm(MeanBF ~ hyear, baseflow)
  # lr_MeanQ <- modifiedmk::mmky(na.omit(sf_metrics$MeanQ))
  # # mk_test_MAM7 <- trend::mk.test(na.omit(sf_metrics$MAM7))
  # lr_Q10 <- modifiedmk::mmky(as.numeric(sf_metrics$Q10))
  # lr_Q50 <- modifiedmk::mmky(as.numeric(sf_metrics$Q50))
  # lr_Q90 <- modifiedmk::mmky(as.numeric(sf_metrics$Q90))
  # lr_Q95 <- modifiedmk::mmky(as.numeric(sf_metrics$Q95))
  # lf_Diff <- modifiedmk::mmky(na.omit(as.numeric(sf_metrics$Diff)))
  
  
  
  
  ## ALIGN "Year" column so that NA years are preserved for plotting, CHOOSE watershed & time frame before running for loop
  # sf_metrics <- left_join(sf_metrics_year_MI_hd, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_KS_hd, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_CA_hd, sf_metrics, by = "hyear")
  
  # sf_metrics <- left_join(sf_metrics_year_MI_long, sf_metrics, by = "hyear")
  sf_metrics <- left_join(sf_metrics_year_KS_long, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_CA_long, sf_metrics, by = "hyear")
  
  # sf_metrics <- left_join(sf_metrics_year_MI_ep, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_KS_ep, sf_metrics, by = "hyear")
  # sf_metrics <- left_join(sf_metrics_year_CA_ep, sf_metrics, by = "hyear")
  
  
  ## PREPARE resulting metrics for plotting
  # ## EXCLUDE Q50 and Q95 from plotting to reduce clutter, then melt dataframe
  # sf_metrics_melt <- sf_metrics[ , c(1,2,3,5,6,7)] 
  ## Only plot Q10, baseflow, and Q90 to reduce clutter, then melt dataframe
  sf_metrics_melt <- sf_metrics[ , c(1,2,5,6)]
  # ## Only plot Q10, mean Q, and Q90 to reduce clutter, then melt dataframe
  # sf_metrics_melt <- sf_metrics[ , c(1,2,3,6)]
  
  sf_metrics_melt <- reshape2::melt(sf_metrics_melt, measure.vars = 2:ncol(sf_metrics_melt), variable.name = "Metric", value.name = "Discharge")
  colnames(sf_metrics_melt) <- c("Year", "Metric", "Discharge")
  # sf_metrics_melt <- data_melt[!is.na(data_melt$Discharge), ]
  
  ## Only plot Q90Q10 and Q90Q10ma, then melt dataframe
  sf_metrics_melt2 <- sf_metrics[ , c(1,8)]
  
  sf_metrics_melt2 <- reshape2::melt(sf_metrics_melt2, measure.vars = 2:ncol(sf_metrics_melt2), variable.name = "Metric", value.name = "Value")
  colnames(sf_metrics_melt2) <- c("Year", "Metric", "Diff")
 
  # ## PLOT 5 metrics  
  # p_metrics_melt <- ggplot2::ggplot(sf_metrics_melt, aes(x = Year, y = Discharge, color = Metric, linetype = Metric, size = Metric)) +
  #     geom_ribbon(data = sf_metrics, mapping = aes(x = hyear, ymin = Q90, ymax = Q10), fill = "grey80", inherit.aes = FALSE) +
  #     geom_line() +
  #     scale_color_manual(values = c("black", "deepskyblue3", "royalblue", "orchid", "black")) +
  #     scale_linetype_manual(values = c(1, 1, 1, 1, 1)) +
  #     scale_size_manual(values = c(0.5, 1.1, 1.1, 1.1, 0.5)) +
  #     scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
  #     # scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  #     scale_x_continuous(breaks = seq(1900, 2020, by = 20)) +
  #     # scale_x_continuous(breaks = seq(1900, 1940, by = 10)) +
  #     # ggtitle(paste(site_name, " METRICS")) +
  #     labs(title = stringr::str_wrap(site_name, 30)) +
  #     theme(axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30), axis.text = element_text(size = 34), plot.title = element_text(size = 32), legend.position = "none") +
  #     ylab(bquote("Discharge " (ft^3/s)))
  
  
  
  ## PLOT 3 metrics (p1) + Q90Q10 spread (p2)
  p1 <- ggplot2::ggplot(sf_metrics_melt, aes(x = Year, y = Discharge, color = Metric, linetype = Metric, size = Metric)) +
    geom_ribbon(data = sf_metrics, mapping = aes(x = hyear, ymin = Q90, ymax = Q10), fill = "grey80", inherit.aes = FALSE) +
    geom_line() +
    scale_color_manual(values = c("black", "royalblue", "black", "darkcyan")) +
    scale_linetype_manual(values = c(1, 1, 1, 1)) +
    scale_size_manual(values = c(0.5, 1, 0.5, 1)) +
    scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
    # scale_x_continuous(breaks = seq(1955, 2025, by = 10)) +
    scale_x_continuous(breaks = seq(1905, 2025, by = 20)) +
    # scale_x_continuous(breaks = seq(1905, 1945, by = 10)) +
    # ggtitle(paste(site_name, " METRICS")) +
    labs(title = stringr::str_wrap(site_name, 30)) +
    theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 24), axis.text = element_text(size = 20, hjust = 1), plot.title = element_text(size = 30)) +
    theme(axis.title.x = element_blank()) +
    ylab(bquote("Discharge " (m^3/s)))
  p2 <- ggplot2::ggplot(sf_metrics_melt2, aes(x = Year, y = Diff, color = Metric, linetype = Metric, size = Metric)) +
    geom_line() +
    scale_color_manual(values = c("darkcyan", "black", "darkred")) +
    scale_linetype_manual(values = c(1, 0, 1)) +
    scale_size_manual(values = c(1, 0.7, 0.7)) +
    # scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
    scale_x_continuous(breaks = seq(1905, 2025, by = 20)) +
    # geom_smooth(method='lm', color = "black", size = 0.7, show.legend = TRUE) +
    theme (legend.position = "right", axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24), axis.text = element_text(size = 20))
    # theme(legend.position = "right")
  
  pcombo <- p1 + p2 + plot_layout(nrow = 2, heights = c(2, 1))
  pcombo
  # p2
  # p1
  
  ## PLOT 
  # ggplot2::ggplot(sf_select, aes(x = hyear, y = Discharge)) +
    # geom_line() +
    # scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
    # # scale_x_continuous(breaks = seq(1955, 2025, by = 10)) +
    # scale_x_continuous(breaks = seq(1905, 2025, by = 20)) +
    # # scale_x_continuous(breaks = seq(1905, 1945, by = 10)) +
    # # ggtitle(paste(site_name, " METRICS")) +
    # labs(title = stringr::str_wrap(site_name, 30)) +
    # # theme(axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30), axis.text = element_text(size = 28, hjust = 1), plot.title = element_text(size = 32), legend.position = "none") +
    # ylab(bquote("Discharge " (ft^3/s)))

  