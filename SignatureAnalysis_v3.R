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
  sf_select$Year <- as.numeric(format(sf_select[[1]], "%Y"))
  
  ## CALCULATE Q90 - 90th percentile of flow 
  Q90 <- sf_select %>% 
    group_by(Year) %>% 
    summarize(Q90 = quantile(Discharge, probs = 0.9, na.rm = TRUE))
  
  sf_metrics <- Q90
  
  ## CALCULATE annual mean discharge
  annual_mean <- sf_select %>% 
    group_by(Year) %>% 
    summarize(MeanDischarge = mean(Discharge))
  
  sf_metrics <- left_join(sf_metrics, annual_mean, by = "Year")
  
  ## PREPARE data for use with lfstat - needs timestamp broken into columns for day, month, year, flow
  sf <- separate(sf_select, "Date", c("year", "month", "day"), "-")
  colnames(sf) <- c("year","month", "day", "flow", "water year")
  sf <- lfstat::createlfobj(sf)
  
  ## CALCULATE Annual Mean Baseflow - Average baseflow for each year
  annual_mean_baseflow <- sf %>% 
    group_by(year) %>% 
    summarize(MeanBaseflow = mean(baseflow))
  colnames(annual_mean_baseflow) <- c("Year", "MeanBaseflow")
  annual_mean_baseflow$Year <- as.double(annual_mean_baseflow$Year)
  sf_metrics <- left_join(sf_metrics, annual_mean_baseflow, by = "Year")
  
  ## CALCULATE MAM7 - Mean Annual Minimum over 7-day period -> 7-day low flow
  MAM7 <- lfstat::MAM(sf, n=7, year = "any", breakdays = NULL, yearly = TRUE)
  colnames(MAM7) <- c("Year","MAM7")
  sf_metrics <- left_join(sf_metrics, MAM7, by = "Year")
  
  ## CALCULATE Q10 - 10th percentile of flow
  Q10 <- sf_select %>% 
    group_by(Year) %>% 
    summarize(Q10 = quantile(Discharge, probs = 0.1, na.rm = TRUE))
  
  sf_metrics <- left_join(sf_metrics, Q10, by = "Year")
  
  ## CALCULATE Q50 - 50th percentile of flow
  Q50 <- sf_select %>% 
    group_by(Year) %>% 
    summarize(Q50 = quantile(Discharge, probs = 0.5, na.rm = TRUE))
  
  sf_metrics <- left_join(sf_metrics, Q50, by = "Year")
  
  
  ## CALCULATE Q90 - 90th percentile of flow 
  Q95 <- sf_select %>% 
    group_by(Year) %>% 
    summarize(Q95= quantile(Discharge, probs = 0.95, na.rm = TRUE))
  
  sf_metrics <- left_join(sf_metrics, Q95, by = "Year")
  
  ## CALCULATE BFI - Baseflow Index for each year
  # BFI <- lfstat::BFI(sf, year = "any", breakdays = NULL, yearly = TRUE)
  # BFI <- data.frame(sf_metrics$Year, BFI)
  # colnames(BFI) <- c("Year", "BFI") 
  # sf_metrics <- left_join(sf_metrics, BFI, by = "Year")
  
  ## CALCULATE Mann-Kendall Test
  mk_test_MAM7 <- trend::mk.test(sf_metrics$MAM7)
  sf_metrics_sel <- sf_metrics[!is.na(sf_metrics$MeanBaseflow), ]
  mk_test_Baseflow <- trend::mk.test(sf_metrics_sel$MeanBaseflow)
  mk_test_MeanQ <- trend::mk.test(sf_metrics_sel$MeanDischarge)
  mk_test_Q10 <- trend::mk.test(sf_metrics_sel$Q10)
  mk_test_Q50 <- trend::mk.test(sf_metrics_sel$Q50)
  mk_test_Q90 <- trend::mk.test(sf_metrics_sel$Q90)
  mk_test_Q95 <- trend::mk.test(sf_metrics_sel$Q95)
  
  ## APPEND MK results to WS dataframe, CHOOSE corresponding watershed before running for loop
  # huc040500MI_ws$p_MAM7[i] <- mk_test_MAM7$p.value[1]
  # if(mk_test_MAM7$estimates[3] < 0)
  #   huc040500MI_ws$tau_MAM7[i] <- "-"
  # if(mk_test_MAM7$estimates[3] > 0)
  #   huc040500MI_ws$tau_MAM7[i] <- "+"
  # huc040500MI_ws$p_Baseflow[i] <- mk_test_Baseflow$p.value[1]
  # if(mk_test_Baseflow$estimates[3] < 0)
  #   huc040500MI_ws$tau_Baseflow[i] <- "-"
  # if(mk_test_Baseflow$estimates[3] > 0)
  #   huc040500MI_ws$tau_Baseflow[i] <- "+"
  # huc040500MI_ws$p_MeanQ[i] <- mk_test_MeanQ$p.value[1]
  # if(mk_test_MeanQ$estimates[3] < 0)
  #   huc040500MI_ws$tau_MeanQ[i] <- "-"
  # if(mk_test_MeanQ$estimates[3] > 0)
  #   huc040500MI_ws$tau_MeanQ[i] <- "+"
  # huc040500MI_ws$p_Q10[i] <- mk_test_Q10$p.value[1]
  # if(mk_test_Q10$estimates[3] < 0)
  #   huc040500MI_ws$tau_Q10[i] <- "-"
  # if(mk_test_Q10$estimates[3] > 0)
  #   huc040500MI_ws$tau_Q10[i] <- "+"
  # huc040500MI_ws$p_Q50[i] <- mk_test_Q50$p.value[1]
  # if(mk_test_Q50$estimates[3] < 0)
  #   huc040500MI_ws$tau_Q50[i] <- "-"
  # if(mk_test_Q50$estimates[3] > 0)
  #   huc040500MI_ws$tau_Q50[i] <- "+"
  # huc040500MI_ws$p_Q90[i] <- mk_test_Q90$p.value[1]
  # if(mk_test_Q90$estimates[3] < 0)
  #   huc040500MI_ws$tau_Q90[i] <- "-"
  # if(mk_test_Q90$estimates[3] > 0)
  #   huc040500MI_ws$tau_Q90[i] <- "+"
  # huc040500MI_ws$p_Q95[i] <- mk_test_Q95$p.value[1]
  # if(mk_test_Q95$estimates[3] < 0)
  #   huc040500MI_ws$tau_Q95[i] <- "-"
  # if(mk_test_Q95$estimates[3] > 0)
  #   huc040500MI_ws$tau_Q95[i] <- "+"

  huc110300KS_ws$p_MAM7[i] <- mk_test_MAM7$p.value[1]
  if(mk_test_MAM7$estimates[3] < 0)
    huc110300KS_ws$tau_MAM7[i] <- "-"
  if(mk_test_MAM7$estimates[3] > 0)
    huc110300KS_ws$tau_MAM7[i] <- "+"
  huc110300KS_ws$p_Baseflow[i] <- mk_test_Baseflow$p.value[1]
  if(mk_test_Baseflow$estimates[3] < 0)
    huc110300KS_ws$tau_Baseflow[i] <- "-"
  if(mk_test_Baseflow$estimates[3] > 0)
    huc110300KS_ws$tau_Baseflow[i] <- "+"
  huc110300KS_ws$p_MeanQ[i] <- mk_test_MeanQ$p.value[1]
  if(mk_test_MeanQ$estimates[3] < 0)
    huc110300KS_ws$tau_MeanQ[i] <- "-"
  if(mk_test_MeanQ$estimates[3] > 0)
    huc110300KS_ws$tau_MeanQ[i] <- "+"
  huc110300KS_ws$p_Q10[i] <- mk_test_Q10$p.value[1]
  if(mk_test_Q10$estimates[3] < 0)
    huc110300KS_ws$tau_Q10[i] <- "-"
  if(mk_test_Q10$estimates[3] > 0)
    huc110300KS_ws$tau_Q10[i] <- "+"
  huc110300KS_ws$p_Q50[i] <- mk_test_Q50$p.value[1]
  if(mk_test_Q50$estimates[3] < 0)
    huc110300KS_ws$tau_Q50[i] <- "-"
  if(mk_test_Q50$estimates[3] > 0)
    huc110300KS_ws$tau_Q50[i] <- "+"
  huc110300KS_ws$p_Q90[i] <- mk_test_Q90$p.value[1]
  if(mk_test_Q90$estimates[3] < 0)
    huc110300KS_ws$tau_Q90[i] <- "-"
  if(mk_test_Q90$estimates[3] > 0)
    huc110300KS_ws$tau_Q90[i] <- "+"
  huc110300KS_ws$p_Q95[i] <- mk_test_Q95$p.value[1]
  if(mk_test_Q95$estimates[3] < 0)
    huc110300KS_ws$tau_Q95[i] <- "-"
  if(mk_test_Q95$estimates[3] > 0)
    huc110300KS_ws$tau_Q95[i] <- "+"

  # huc180102CA_ws$p_MAM7[i] <- mk_test_MAM7$p.value[1]
  # if(mk_test_MAM7$estimates[3] < 0)
  #   huc180102CA_ws$tau_MAM7[i] <- "-"
  # if(mk_test_MAM7$estimates[3] > 0)
  #   huc180102CA_ws$tau_MAM7[i] <- "+"
  # huc180102CA_ws$p_Baseflow[i] <- mk_test_Baseflow$p.value[1]
  # if(mk_test_Baseflow$estimates[3] < 0)
  #   huc180102CA_ws$tau_Baseflow[i] <- "-"
  # if(mk_test_Baseflow$estimates[3] > 0)
  #   huc180102CA_ws$tau_Baseflow[i] <- "+"
  # huc180102CA_ws$p_MeanQ[i] <- mk_test_MeanQ$p.value[1]
  # if(mk_test_MeanQ$estimates[3] < 0)
  #   huc180102CA_ws$tau_MeanQ[i] <- "-"
  # if(mk_test_MeanQ$estimates[3] > 0)
  #   huc180102CA_ws$tau_MeanQ[i] <- "+"
  # huc180102CA_ws$p_Q10[i] <- mk_test_Q10$p.value[1]
  # if(mk_test_Q10$estimates[3] < 0)
  #   huc180102CA_ws$tau_Q10[i] <- "-"
  # if(mk_test_Q10$estimates[3] > 0)
  #   huc180102CA_ws$tau_Q10[i] <- "+"
  # huc180102CA_ws$p_Q50[i] <- mk_test_Q50$p.value[1]
  # if(mk_test_Q50$estimates[3] < 0)
  #   huc180102CA_ws$tau_Q50[i] <- "-"
  # if(mk_test_Q50$estimates[3] > 0)
  #   huc180102CA_ws$tau_Q50[i] <- "+"
  # huc180102CA_ws$p_Q90[i] <- mk_test_Q90$p.value[1]
  # if(mk_test_Q90$estimates[3] < 0)
  #   huc180102CA_ws$tau_Q90[i] <- "-"
  # if(mk_test_Q90$estimates[3] > 0)
  #   huc180102CA_ws$tau_Q90[i] <- "+"
  # huc180102CA_ws$p_Q95[i] <- mk_test_Q95$p.value[1]
  # if(mk_test_Q95$estimates[3] < 0)
  #   huc180102CA_ws$tau_Q95[i] <- "-"
  # if(mk_test_Q95$estimates[3] > 0)
  #   huc180102CA_ws$tau_Q95[i] <- "+"
  
  ## SAVE CSV of streamgage metrics, CHOOSE watershed before running for loop --> in the future these should write to SQL database
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_MI/MI_", i, "_metrics.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_KS/KS_", i, "_metrics.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_CA/CA_", i, "_metrics.csv"))
  
 ## ALIGN "Year" column so that NA years are preserved for plotting, CHOOSE watershed & time frame before running for loop
  # sf_metrics <- left_join(sf_metrics_year_MI_hd, sf_metrics, by = "Year")
  sf_metrics <- left_join(sf_metrics_year_KS_hd, sf_metrics, by = "Year")
  # sf_metrics <- left_join(sf_metrics_year_CA_hd, sf_metrics, by = "Year")
  
  # sf_metrics <- left_join(sf_metrics_year_MI_long, sf_metrics, by = "Year")
  # sf_metrics <- left_join(sf_metrics_year_KS_long, sf_metrics, by = "Year")
  # sf_metrics <- left_join(sf_metrics_year_CA_long, sf_metrics, by = "Year")

  
# }
  



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
# huc180102CA_sfmetrics_long <- sf_metrics




       
###########################################################
### PLOT ###

p <- ggplot(annual_mean, aes(x = Year, y = MeanDischarge)) +
     geom_point(shape = 21, color = "black", fill = "chartreuse3", size = 4) +
     geom_line() +
     ggtitle("Scott River near Fort Jones - Annual Mean Discharge")

# + scale_y_continuous(trans='log10')

# Set axis limits c(min, max)
# min <- as.Date("1950-1-1")
# max <- as.Date("2021-12-31")
# p + scale_x_date(limits = c(min, max))
# p + scale_x_discrete(breaks = c("1960", "1965", "1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025"))

p

###########################################################
###########################################################
### DATA EXPLORER ###
# CREATE summary stats report for watersheds and streamflow sets
create_report(huc040500MI, output_file = "huc04050001MI_report.html", output_dir = oname, report_title = "04050001 - Michigan - Data Profiling Report")
create_report(huc110300KS, output_file = "huc110300KS_report.html", output_dir = oname, report_title = "110300 - Kansas - Data Profiling Report")
create_report(huc180102CA, output_file = "huc180102CA_report.html", output_dir = oname, report_title = "180102 - California - Data Profiling Report")

# DataExplorer report did not work for streamflow dataframes
create_report(huc040500MI_sf, output_file = "huc04050001MIsf_report.html", output_dir = oname, report_title = "04050001 Streamflow- Michigan - Data Profiling Report")
create_report(huc110300KS_sf, output_file = "huc110300KSsf_report.html", output_dir = oname, report_title = "110300 Streamflow - Kansas - Data Profiling Report")
create_report(huc180102CA_sf, output_file = "huc180102CAsf_report.html", output_dir = oname, report_title = "180102 Streamflow - California - Data Profiling Report")

print(summary(huc040500MI_sf))
summary_huc040500MI_sf <- summary(huc040500MI_sf)
view(summary_huc040500MI_sf)

### ZOO ###
## CALCULATE 7-day moving average then take min as 7-day low flow metric for each gage station in watershed using           Zoo package
sf <- as.zoo(raw_daily)
sf_7day_mean <- rollmean(sf, 7)
sf_7day_mean <- round(sf_7day_mean, 3)
sf_7day_mean_min <- rbind(sf_7day_mean_min, min(sf_7day_mean))
colnames(sf_7day_mean_min) <- "LF7day"


### LFSTAT ###
### PREPARE data - needs timestamp broken into columns for day, month, year, flow
sf <- separate(streamflow_SR, "Date", c("year", "month", "day"), "-")
colnames(sf) <- c("year","month", "day", "waterYear", "flow")

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
