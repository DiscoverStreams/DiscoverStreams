# data manipulation libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(reshape2)

# data analysis/statistical libraries
library(xts)
library(lfstat)
library(trend)



# huc040500MI_ws
# huc110300KS_ws 
# huc180102CA_ws 

########## STREAM DEPLETION METRICS ##########
## START loop at i = 2 because i = 1 is column "Date"
i = 2

## UN-COMMENT corresponding watershed lines and save location before running for loop
# for (i in 2:ncol(huc040500MI_sf)) {
#   site_name <- colnames(huc040500MI_sf[i])
#   sf_select <- data.frame(huc040500MI_sf[[1]], huc040500MI_sf[[i]])
# for (i in 2:ncol(huc110300KS_sf)) {
#   site_name <- colnames(huc110300KS_sf[i])
#   sf_select <- data.frame(huc110300KS_sf[[1]], huc110300KS_sf[[i]])
for (i in 2:ncol(huc180102CA_sf)) {
  site_name <- colnames(huc180102CA_sf[i])
  sf_select <- data.frame(huc180102CA_sf[[1]], huc180102CA_sf[[i]])

  ## PREPARE dataframe for calculations  
  colnames(sf_select) <- c("Date", "Discharge")
  sf_select$Year <- as.numeric(format(sf_select[[1]], "%Y"))
  
  ## CALCULATE annual mean discharge
  annual_mean <- sf_select %>% 
    group_by(Year) %>% 
    summarize(MeanDischarge = mean(Discharge))
  
  sf_metrics <- annual_mean
  
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
  Q90 <- sf_select %>% 
    group_by(Year) %>% 
    summarize(Q90 = quantile(Discharge, probs = 0.9, na.rm = TRUE))
  
  sf_metrics <- left_join(sf_metrics, Q90, by = "Year")
  
  
  ## PREPARE data for use with lfstat - needs timestamp broken into columns for day, month, year, flow
  sf <- separate(sf_select, "Date", c("year", "month", "day"), "-")
  colnames(sf) <- c("year","month", "day", "flow", "water year")
  sf <- lfstat::createlfobj(sf)
  
  ## CALCULATE MAM7 - Mean Annual Minimum over 7-day period -> 7-day low flow
  MAM7 <- lfstat::MAM(sf, n=7, year = "any", breakdays = NULL, yearly = TRUE)
  colnames(MAM7) <- c("Year","MAM7")
  sf_metrics <- left_join(sf_metrics, MAM7, by = "Year")
  
  ## CALCULATE Annual Mean Baseflow - Average baseflow for each year
  annual_mean_baseflow <- sf %>% 
    group_by(year) %>% 
    summarize(MeanBaseflow = mean(baseflow))
  colnames(annual_mean_baseflow) <- c("Year", "MeanBaseflow")
  annual_mean_baseflow$Year <- as.double(annual_mean_baseflow$Year)
  sf_metrics <- left_join(sf_metrics, annual_mean_baseflow, by = "Year")
  
  ## CALCULATE BFI - Baseflow Index for each year
  # BFI <- lfstat::BFI(sf, year = "any", breakdays = NULL, yearly = TRUE)
  # BFI <- data.frame(sf_metrics$Year, BFI)
  # colnames(BFI) <- c("Year", "BFI") 
  # sf_metrics <- left_join(sf_metrics, BFI, by = "Year")
  
  ## CALCULATE Mann-Kendall Test
  mk_test <- trend::mk.test(sf_metrics$MAM7)
  ## for MI ws fails at i = 8, i = 13, i = 16, i = 19, i = 23, i = 24, i = 27, i = 29
  ## for KS ws fails at i = 4
  ## for CA ws fails at i = 3 and i = 14
  
  
  ## APPEND data to WS dataframe, CHOOSE corresponding watershed before running for loop
  # huc040500MI_ws$p[i-1] <- mk_test$p.value[1]
  # huc040500MI_ws$tau[i-1] <- mk_test$estimates[3]
  # if(mk_test$estimates[3] < 0)
  #   huc040500MI_ws$`tau+/-`[i-1] <- "-"
  # if(mk_test$estimates[3] > 0)
  #   huc040500MI_ws$`tau+/-`[i-1] <- "+"
  # huc110300KS_ws$p[i-1] <- mk_test$p.value[1]
  # huc110300KS_ws$tau[i-1] <- mk_test$estimates[3]
  # if(mk_test$estimates[3] < 0)
  #   huc110300KS_ws$`tau+/-`[i-1] <- "-"
  # if(mk_test$estimates[3] > 0)
  #   huc110300KS_ws$`tau+/-`[i-1] <- "+"
  huc180102CA_ws$p[i-1] <- mk_test$p.value[1]
  huc180102CA_ws$tau[i-1] <- mk_test$estimates[3]
  if(mk_test$estimates[3] < 0)
    huc180102CA_ws$`tau+/-`[i-1] <- "-"
  if(mk_test$estimates[3] > 0)
    huc180102CA_ws$`tau+/-`[i-1] <- "+"
  
} 
  
  ## PREPARE resulting metrics for plotting
  data_melt <- reshape2::melt(sf_metrics, measure.vars = 2:7, variable.name = "Metric", value.name = "Discharge")
  
  ## SAVE plot of streamgage metrics, CHOOSE watershed before running for loop
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_MI/MI_", i-1, "_metrics.png", sep = ""), width = 757, height = 464, unit = "px")
  # png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_KS/KS_", i-1, "_metrics.png", sep = ""), width = 757, height = 464, unit = "px")
  png(file = paste("~/GradSchool/DiscoverStreams/plots/sfmetrics_CA/CA_", i-1, "_metrics.png", sep = ""), width = 757, height = 464, unit = "px")
  
  ## PLOT options
  p_metrics <- ggplot2::ggplot(data_melt, aes(x = Year, y = Discharge, color = Metric, linetype = Metric, size = Metric)) +
    geom_line() +
    scale_color_manual(values = c("royalblue4", "hotpink4", "mediumvioletred", "maroon1", "darkgoldenrod2", "deepskyblue3", "black")) +
    scale_linetype_manual(values = c(1, 1, 1, 1, 1, 1, 0)) +
    scale_size_manual(values = c(1.3, 1.05, 1.05, 1.05, 1.3, 1.5, 0)) +
    scale_y_log10() +
    ggtitle(paste(site_name, " METRICS")) +
    ylab(bquote("Discharge " (ft^3/s)))
    # ylab("Discharge ft^3/s") 
  
  ## WRITE plot to local device
  print(p_metrics)
  
  ## CLOSE PNG connection
  # Sys.sleep(5)
  dev.off()
  
  ## SAVE CSV of streamgage metrics, CHOOSE watershed before running for loop --> in the future these should write to SQL database
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_MI/MI_", i-1, "_metrics.csv"))
  # write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_KS/KS_", i-1, "_metrics.csv"))
  write.csv(sf_metrics, paste("~/GradSchool/DiscoverStreams/outputs/sfmetrics_CA/CA_", i-1, "_metrics.csv"))
  
}



########## MANN-KENDALL TREND TEST ##########
mk.test(sf_metrics_CA$MAM7)




       
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
