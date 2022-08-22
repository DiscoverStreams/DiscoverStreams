# data manipulation libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(stringr)

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
## Run once, CREATE separate df of consecutive years to ensure all years represented in plot when NA years are dropped
sf_metrics_year_MI_hd <- data.frame(seq(1960, 2021, by = 1))
colnames(sf_metrics_year_MI_hd) <- "hyear"
sf_metrics_year_KS_hd <- data.frame(seq(1963, 2021, by = 1))
colnames(sf_metrics_year_KS_hd) <- "hyear"
sf_metrics_year_CA_hd <- data.frame(seq(1962, 2021, by = 1))
colnames(sf_metrics_year_CA_hd) <- "hyear"

sf_metrics_year_MI_long <- data.frame(seq(1901, 2021, by = 1))
colnames(sf_metrics_year_MI_long) <- "hyear"
sf_metrics_year_KS_long <- data.frame(seq(1902, 2021, by = 1))
colnames(sf_metrics_year_KS_long) <- "hyear"
sf_metrics_year_CA_long <- data.frame(seq(1904, 2021, by = 1))
colnames(sf_metrics_year_CA_long) <- "hyear"

sf_metrics_year_MI_ep <- data.frame(seq(1901, 1940, by = 1))
colnames(sf_metrics_year_MI_ep) <- "hyear"
sf_metrics_year_KS_ep <- data.frame(seq(1902, 1940, by = 1))
colnames(sf_metrics_year_KS_ep) <- "hyear"
sf_metrics_year_CA_ep <- data.frame(seq(1904, 1940, by = 1))
colnames(sf_metrics_year_CA_ep) <- "hyear"

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
  
  ## PREPARE data for use with lfstat to calculate baseflow and water year - needs timestamp broken into columns for day, month, year, flow
  sf <- separate(sf_select, "Date", c("year", "month", "day"), "-")
  colnames(sf) <- c("year","month", "day", "flow")
  sf <- lfstat::createlfobj(sf, hyearstart = 10)
  sf$hyear <- as.numeric(sf$hyear)
  sf$year <- as.numeric(sf$year)
  
  ## JOIN hydrologic year to sf_select to calculate sf metrics on water years instead of calendar years
  sf_select <- left_join(sf_select, sf[ ,c(3,5)], by = "year")
  
  ## CALCULATE Q10 - probability flow will be exceeded 10% of the time (P90)
  # Q10 <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(Q10 = quantile(Discharge, probs = 0.9, na.rm = TRUE))
  
  Q10 <- lfstat::Qxx(sf, Qxx = 10, yearly = TRUE)
  colnames(Q10) <- c("hyear", "Q10")
  
  sf_metrics <- Q10
 
  
  ## CALCULATE annual mean discharge
  meanQ <- sf_select %>% 
    group_by(hyear) %>% 
    summarize(MeanQ = mean(Discharge))
  
  sf_metrics <- left_join(sf_metrics, meanQ, by = "hyear")
  
  ## CALCULATE Q50 - probability flow will be exceeded 50% of the time (P50)
  # Q50 <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(Q50 = quantile(Discharge, probs = 0.5, na.rm = TRUE))
 
   Q50 <- lfstat::Qxx(sf, Qxx = 50, yearly = TRUE)
   colnames(Q50) <- c("hyear", "Q50")
  
  sf_metrics <- left_join(sf_metrics, Q50, by = "hyear")
  
    ## CALCULATE Annual Mean Baseflow  using sf from lfstat - Average baseflow for each year
  baseflow <- sf %>% 
    group_by(hyear) %>%
    na.omit(sf) %>% 
    summarize(MeanBaseflow = mean(baseflow))
  baseflow$hyear <- as.numeric(baseflow$hyear)
  sf_metrics <- left_join(sf_metrics, baseflow, by = "hyear")
  
  ## CALCULATE MAM7 - Mean Annual Minimum over 7-day period -> 7-day low flow
  MAM7 <- lfstat::MAM(sf, n=7, yearly = TRUE)
  colnames(MAM7) <- c("hyear","MAM7")
  sf_metrics <- left_join(sf_metrics, MAM7, by = "hyear")
  
  ## CALCULATE Q10 - probability flow will be exceeded 90% of the time (P10)
  # Q90 <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(Q90 = quantile(Discharge, probs = 0.1, na.rm = TRUE))
  
  Q90 <- lfstat::Qxx(sf, Qxx = 90, yearly = TRUE)
  colnames(Q90) <- c("hyear", "Q90")
  
  sf_metrics <- left_join(sf_metrics, Q90, by = "hyear")
  
  ## CALCULATE Q95 - probability flow will be exceeded 95% of the time (P05)
  # Q95 <- sf_select %>% 
  #   group_by(hyear) %>% 
  #   summarize(Q95= quantile(Discharge, probs = 0.05, na.rm = TRUE))
  
  Q95 <- lfstat::Qxx(sf, Qxx = 95, yearly = TRUE)
  colnames(Q95) <- c("hyear", "Q95")
  
  sf_metrics <- left_join(sf_metrics, Q95, by = "hyear")
  
  ## CALCULATE BFI - Baseflow Index for each year
  # BFI <- lfstat::BFI(sf, year = "any", breakdays = NULL, yearly = TRUE)
  # BFI <- data.frame(sf_metrics$Year, BFI)
  # colnames(BFI) <- c("Year", "BFI") 
  # sf_metrics <- left_join(sf_metrics, BFI, by = "Year")
  
  
  ## CALCULATE Mann-Kendall Test
  mk_test_Baseflow <- trend::mk.test(na.omit(sf_metrics$MeanBaseflow))
  mk_test_MeanQ <- trend::mk.test(na.omit(sf_metrics$MeanQ))
  mk_test_MAM7 <- trend::mk.test(na.omit(sf_metrics$MAM7))
  mk_test_Q10 <- trend::mk.test(as.numeric(sf_metrics$Q10))
  mk_test_Q50 <- trend::mk.test(as.numeric(sf_metrics$Q50))
  mk_test_Q90 <- trend::mk.test(as.numeric(sf_metrics$Q90))
  mk_test_Q95 <- trend::mk.test(as.numeric(sf_metrics$Q95))
  
  
  ## APPEND MK results to WS dataframe, CHOOSE corresponding watershed before running for loop
  # ws_040500MI$p_Q10[i] <- mk_test_Q10$p.value[1]
  # if(mk_test_Q10$estimates[3] < 0)
  #   ws_040500MI$tau_Q10[i] <- "-"
  # if(mk_test_Q10$estimates[3] > 0)
  #   ws_040500MI$tau_Q10[i] <- "+"
  # ws_040500MI$p_MeanQ[i] <- mk_test_MeanQ$p.value[1]
  # if(mk_test_MeanQ$estimates[3] < 0)
  #   ws_040500MI$tau_MeanQ[i] <- "-"
  # if(mk_test_MeanQ$estimates[3] > 0)
  #   ws_040500MI$tau_MeanQ[i] <- "+"
  # ws_040500MI$p_Q50[i] <- mk_test_Q50$p.value[1]
  # if(mk_test_Q50$estimates[3] < 0)
  #   ws_040500MI$tau_Q50[i] <- "-"
  # if(mk_test_Q50$estimates[3] > 0)
  #   ws_040500MI$tau_Q50[i] <- "+"
  # ws_040500MI$p_Baseflow[i] <- mk_test_Baseflow$p.value[1]
  # if(mk_test_Baseflow$estimates[3] < 0)
  #   ws_040500MI$tau_Baseflow[i] <- "-"
  # if(mk_test_Baseflow$estimates[3] > 0)
  #   ws_040500MI$tau_Baseflow[i] <- "+"
  # ws_040500MI$p_MAM7[i] <- mk_test_MAM7$p.value[1]
  # if(mk_test_MAM7$estimates[3] < 0)
  #   ws_040500MI$tau_MAM7[i] <- "-"
  # if(mk_test_MAM7$estimates[3] > 0)
  #   ws_040500MI$tau_MAM7[i] <- "+"
  # ws_040500MI$p_Q90[i] <- mk_test_Q90$p.value[1]
  # if(mk_test_Q90$estimates[3] < 0)
  #   ws_040500MI$tau_Q90[i] <- "-"
  # if(mk_test_Q90$estimates[3] > 0)
  #   ws_040500MI$tau_Q90[i] <- "+"
  # ws_040500MI$p_Q95[i] <- mk_test_Q95$p.value[1]
  # if(mk_test_Q95$estimates[3] < 0)
  #   ws_040500MI$tau_Q95[i] <- "-"
  # if(mk_test_Q95$estimates[3] > 0)
  #   ws_040500MI$tau_Q95[i] <- "+"

  ws_110300KS$p_Q10[i] <- mk_test_Q10$p.value[1]
  if(mk_test_Q10$estimates[3] < 0)
    ws_110300KS$tau_Q10[i] <- "-"
  if(mk_test_Q10$estimates[3] > 0)
    ws_110300KS$tau_Q10[i] <- "+"
  ws_110300KS$p_MeanQ[i] <- mk_test_MeanQ$p.value[1]
  if(mk_test_MeanQ$estimates[3] < 0)
    ws_110300KS$tau_MeanQ[i] <- "-"
  if(mk_test_MeanQ$estimates[3] > 0)
    ws_110300KS$tau_MeanQ[i] <- "+"
  ws_110300KS$p_Q50[i] <- mk_test_Q50$p.value[1]
  if(mk_test_Q50$estimates[3] < 0)
    ws_110300KS$tau_Q50[i] <- "-"
  if(mk_test_Q50$estimates[3] > 0)
    ws_110300KS$tau_Q50[i] <- "+"
  ws_110300KS$p_Baseflow[i] <- mk_test_Baseflow$p.value[1]
  if(mk_test_Baseflow$estimates[3] < 0)
    ws_110300KS$tau_Baseflow[i] <- "-"
  if(mk_test_Baseflow$estimates[3] > 0)
    ws_110300KS$tau_Baseflow[i] <- "+"
  ws_110300KS$p_MAM7[i] <- mk_test_MAM7$p.value[1]
  if(mk_test_MAM7$estimates[3] == "NaN")
    ws_110300KS$tau_MAM7[i] <- "NA"
  if(!is.na(mk_test_MAM7$estimates[3]) < 0)
    ws_110300KS$tau_MAM7[i] <- "-"
  if(!is.na(mk_test_MAM7$estimates[3]) > 0)
    ws_110300KS$tau_MAM7[i] <- "+"
  ws_110300KS$p_Q90[i] <- mk_test_Q90$p.value[1]
  if(mk_test_Q90$estimates[3] < 0)
    ws_110300KS$tau_Q90[i] <- "-"
  if(mk_test_Q90$estimates[3] > 0)
    ws_110300KS$tau_Q90[i] <- "+"
  ws_110300KS$p_Q95[i] <- mk_test_Q95$p.value[1]
  if(mk_test_MAM7$estimates[3] == "NaN")
    ws_110300KS$tau_MAM7[i] <- "NA"
  if(!is.na(mk_test_Q95$estimates[3]) < 0)
    ws_110300KS$tau_Q95[i] <- "-"
  if(!is.na(mk_test_Q95$estimates[3]) > 0)
    ws_110300KS$tau_Q95[i] <- "+"
  

  # ws_180102CA$p_Q10[i] <- mk_test_Q10$p.value[1]
  # if(mk_test_Q10$estimates[3] < 0)
  #   ws_180102CA$tau_Q10[i] <- "-"
  # if(mk_test_Q10$estimates[3] > 0)
  #   ws_180102CA$tau_Q10[i] <- "+"
  # ws_180102CA$p_MeanQ[i] <- mk_test_MeanQ$p.value[1]
  # if(mk_test_MeanQ$estimates[3] < 0)
  #   ws_180102CA$tau_MeanQ[i] <- "-"
  # if(mk_test_MeanQ$estimates[3] > 0)
  #   ws_180102CA$tau_MeanQ[i] <- "+"
  # ws_180102CA$p_Q50[i] <- mk_test_Q50$p.value[1]
  # if(mk_test_Q50$estimates[3] < 0)
  #   ws_180102CA$tau_Q50[i] <- "-"
  # if(mk_test_Q50$estimates[3] > 0)
  #   ws_180102CA$tau_Q50[i] <- "+"
  # ws_180102CA$p_Baseflow[i] <- mk_test_Baseflow$p.value[1]
  # if(mk_test_Baseflow$estimates[3] < 0)
  #   ws_180102CA$tau_Baseflow[i] <- "-"
  # if(mk_test_Baseflow$estimates[3] > 0)
  #   ws_180102CA$tau_Baseflow[i] <- "+"
  # ws_180102CA$p_MAM7[i] <- mk_test_MAM7$p.value[1]
  # if(mk_test_MAM7$estimates[3] < 0)
  #   ws_180102CA$tau_MAM7[i] <- "-"
  # if(mk_test_MAM7$estimates[3] > 0)
  #   ws_180102CA$tau_MAM7[i] <- "+"
  # ws_180102CA$p_Q90[i] <- mk_test_Q90$p.value[1]
  # if(mk_test_Q90$estimates[3] < 0)
  #   ws_180102CA$tau_Q90[i] <- "-"
  # if(mk_test_Q90$estimates[3] > 0)
  #   ws_180102CA$tau_Q90[i] <- "+"
  # ws_180102CA$p_Q95[i] <- mk_test_Q95$p.value[1]
  # if(mk_test_Q95$estimates[3] < 0)
  #   ws_180102CA$tau_Q95[i] <- "-"
  # if(mk_test_Q95$estimates[3] > 0)
  #   ws_180102CA$tau_Q95[i] <- "+"
  
  
  
  ## SAVE CSV of streamgage metrics, CHOOSE watershed before running for loop --> in the future these should write to SQL database
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_MI/MI_hd/MI_", i, "_metrics_hd.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_KS/KS_hd/KS_", i, "_metrics_hd.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_CA/CA_hd/CA_", i, "_metrics_hd.csv"))
  
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_MI/MI_long/MI_", i, "_metrics_long.csv"))
  write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_KS/KS_long/KS_", i, "_metrics_long.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_CA/CA_long/CA_", i, "_metrics_long.csv"))
  
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_MI/MI_ep/MI_", i, "_metrics_ep.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_KS/KS_ep/KS_", i, "_metrics_ep.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_CA/CA_ep/CA_", i, "_metrics_ep.csv"))
  
  
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

# }
  



## PREPARE resulting metrics for plotting
## EXCLUDE Q50 and Q95 from plotting to reduce clutter, then melt dataframe
sf_metrics_melt <- sf_metrics[ , c(1,2,3,5,6,7)]  
sf_metrics_melt <- reshape2::melt(sf_metrics_melt, measure.vars = 2:6, variable.name = "Metric", value.name = "Discharge")
colnames(sf_metrics_melt) <- c("Year", "Metric", "Discharge")
# sf_metrics_melt <- data_melt[!is.na(data_melt$Discharge), ]
  

  ## SAVE plot of streamgage metrics, CHOOSE watershed before running for loop
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_MI/MI_hd/MI_", i, "_metrics_hd.png", sep = ""), width = 757, height = 464, unit = "px")
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_KS/KS_hd/KS_", i, "_metrics_hd.png", sep = ""), width = 757, height = 464, unit = "px")
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_CA/CA_hd/CA_", i, "_metrics_hd.png", sep = ""), width = 757, height = 464, unit = "px")
  
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_MI/MI_long/MI_", i, "_metrics_long.png", sep = ""), width = 757, height = 464, unit = "px")
  png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_KS/KS_long/KS_", i, "_metrics_long.png", sep = ""), width = 757, height = 464, unit = "px")
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_CA/CA_long/CA_", i, "_metrics_long.png", sep = ""), width = 757, height = 464, unit = "px")
  
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_MI/MI_ep/MI_", i, "_metrics_ep.png", sep = ""), width = 757, height = 464, unit = "px")
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_KS/KS_ep/KS_", i, "_metrics_ep.png", sep = ""), width = 757, height = 464, unit = "px")
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_CA/CA_ep/CA_", i, "_metrics_ep.png", sep = ""), width = 757, height = 464, unit = "px")
  
  
p_metrics_melt <- ggplot2::ggplot(sf_metrics_melt, aes(x = Year, y = Discharge, color = Metric, linetype = Metric, size = Metric)) +
    geom_ribbon(data = sf_metrics, mapping = aes(x = hyear, ymin = Q90, ymax = Q10), fill = "grey80", inherit.aes = FALSE) +
    geom_line() +
    scale_color_manual(values = c("black", "deepskyblue3", "royalblue", "orchid", "black")) +
    scale_linetype_manual(values = c(1, 1, 1, 1, 1)) +
    scale_size_manual(values = c(0.5, 1.1, 1.1, 1.1, 0.5)) +
    scale_y_log10(labels = scales::number_format(accuracy = 0.01)) +
    # scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    scale_x_continuous(breaks = seq(1900, 2020, by = 20)) +
    # scale_x_continuous(breaks = seq(1900, 1940, by = 10)) +
    # ggtitle(paste(site_name, " METRICS")) +
    labs(title = stringr::str_wrap(site_name, 30)) +
    theme(axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30), axis.text = element_text(size = 34), plot.title = element_text(size = 32), legend.position = "none") +
    ylab(bquote("Discharge " (ft^3/s)))

  ## WRITE plot to local device
  print(p_metrics_melt)
  
  ## CLOSE PNG connection
  dev.off()
  
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




