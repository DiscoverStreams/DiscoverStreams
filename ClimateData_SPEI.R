# database connection/retrieval libraries
library(readr)
library(rnoaa)
# data manipulation libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(SPEI)
library(patchwork)


## READ csv files containing climate data for all watersheds
MWBM_PET <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/Data/MWBM_PET_mm_KS_CA_MI_HUC8_climgrid.csv", header = TRUE, col.names = c("11030001", "11030002", "11030003", "11030004", "11030005", "11030006", "11030007", "11030008", "11030009", "11030010", "11030011", "11030012", "11030013", "11030014", "11030015", "11030016", "11030017", "11030018", "18010201", "18010202", "18010203", "18010204", "18010205", "18010206", "18010207", "18010208", "18010209", "18010210", "18010211", "18010212", "04050001", "04050002", "04050003", "04050004", "04050005", "04050006", "04050007", "DATE"))

MWBM_PRCP <- read.csv(file = "C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/Data/MWBM_PRCP_mm_KS_CA_MI_HUC8_climgrid.csv", header = TRUE, col.names = c("11030001", "11030002", "11030003", "11030004", "11030005", "11030006", "11030007", "11030008", "11030009", "11030010", "11030011", "11030012", "11030013", "11030014", "11030015", "11030016", "11030017", "11030018", "18010201", "18010202", "18010203", "18010204", "18010205", "18010206", "18010207", "18010208", "18010209", "18010210", "18010211", "18010212", "04050001", "04050002", "04050003", "04050004", "04050005", "04050006", "04050007", "DATE"))


## SUBSET to watersheds
cl_040500MI_PET <- MWBM_PET[ , c(38, 31:37)]
cl_040500MI_PRCP <- MWBM_PRCP[ , c(38, 31:37)]

cl_110300KS_PET <- MWBM_PET[ , c(38, 1:18)]
cl_110300KS_PRCP <- MWBM_PRCP[ , c(38, 1:18)]

cl_180102CA_PET <- MWBM_PET[ , c(38, 19:30)]
cl_180102CA_PRCP <- MWBM_PRCP[ , c(38, 19:30)]

cl_date <- MWBM_PET$DATE

## CALCULATE P-PET for use as input in SPEI calculation for each watershed
## https://stackoverflow.com/questions/28117409/how-do-you-subtract-two-data-frames-from-one-another-in-r
cl_040500MI_P_PET <- bind_rows(cl_040500MI_PET, cl_040500MI_PRCP) %>%
  group_by(DATE) %>%
  summarise_each(funs(diff(.))) %>%
  data.frame()

cl_110300KS_P_PET <- bind_rows(cl_110300KS_PET, cl_110300KS_PRCP) %>%
  group_by(DATE) %>%
  summarise_each(funs(diff(.))) %>%
  data.frame()

cl_180102CA_P_PET <- bind_rows(cl_180102CA_PET, cl_180102CA_PRCP) %>%
  group_by(DATE) %>%
  summarise_each(funs(diff(.))) %>%
  data.frame()

## SELECT watersheds specific to high-density gage stations of interest, ** MI has all watersheds represented by a gage station
cl_040500MI_P_PET_hd <- cl_040500MI_P_PET[ , c("X04050001", "X04050003", "X04050004", "X04050005", "X04050006", "X04050007")]
cl_110300KS_P_PET_hd <- cl_110300KS_P_PET[ , c("X11030001")]
                                               # , "X11030004", "X11030005", "X11030008", "X11030009", "X11030010", "X11030011", "X11030012", "X11030013", "X11030016", "X11030017", "X11030018")]
cl_180102CA_P_PET_hd <- cl_180102CA_P_PET[ , c("X18010201", "X18010202", "X18010204", "X18010206", "X18010207", "X18010208", "X18010209", "X18010210", "X18010211")]

## SELECT watersheds specific to long-term gage stations of interest
cl_040500MI_P_PET_long <- cl_040500MI_P_PET[ , c("X04050004", "X04050006")]
cl_110300KS_P_PET_wlong <- cl_110300KS_P_PET[ , c("X11030001")]
                                                 # "X11030001", "X11030003", "X11030012", "X11030013", "X11030018")]
cl_110300KS_P_PET_elong <- cl_110300KS_P_PET[ , c("X11030018")]
cl_110300KS_P_PET_clong <- cl_110300KS_P_PET[ , c("X11030009")]
cl_180102CA_P_PET_long <- cl_180102CA_P_PET[ , c("X18010201", "X18010202", "X18010206", "X18010207", "X18010209", "X18010210", "X18010211")]

## SELECT watersheds specific to early-period gage stations of interest
cl_040500MI_P_PET_ep <- cl_040500MI_P_PET[ , c("X04050004", "X04050006")]
cl_110300KS_P_PET_ep <- cl_110300KS_P_PET[ , c("X11030001", "X11030004", "X11030005", "X11030012", "X11030013", "X11030018")]
cl_180102CA_P_PET_ep <- cl_180102CA_P_PET[ , c("X18010201", "X18010202", "X18010203", "X18010204", "X18010206", "X18010207", "X18010209", "X18010210", "X18010211")]


################################################################################
### SPEI ###
## CALCULATE SPEI, exclude column 1(DATE)
# cl_040500MI_SPEI1 <- SPEI::spei(cl_040500MI_P_PET[2:8], 1)

## CALCULATE SPEI-12 for high-density data watersheds
cl_040500MI_SPEI12_hd <- SPEI::spei(cl_040500MI_P_PET_hd, 12)
    # plot(cl_040500MI_SPEI12_hd)

cl_110300KS_SPEI12_hd <- SPEI::spei(cl_110300KS_P_PET_hd, 12)
    # plot(cl_110300KS_SPEI12_hd)

cl_180102CA_SPEI12_hd <- SPEI::spei(cl_180102CA_P_PET_hd, 12)
    # plot(cl_180102CA_SPEI12_hd)

## CALCULATE SPEI-12 for long-term data watersheds
cl_040500MI_SPEI12_long <- SPEI::spei(cl_040500MI_P_PET_long, 12)
    # plot(cl_040500MI_SPEI12_long)

cl_110300KS_SPEI12_wlong <- SPEI::spei(cl_110300KS_P_PET_wlong, 12)
cl_110300KS_SPEI12_elong <- SPEI::spei(cl_110300KS_P_PET_elong, 12)
cl_110300KS_SPEI12_clong <- SPEI::spei(cl_110300KS_P_PET_clong, 12)
  # plot(cl_110300KS_SPEI12_long)

cl_180102CA_SPEI12_long <- SPEI::spei(cl_180102CA_P_PET_long, 12)
    # plot(cl_180102CA_SPEI12_long)
  
## CALCULATE SPEI-12 for early-period data watersheds
cl_040500MI_SPEI12_ep <- SPEI::spei(cl_040500MI_P_PET_ep, 12)
    # plot(cl_040500MI_SPEI12_ep)

cl_110300KS_SPEI12_ep <- SPEI::spei(cl_110300KS_P_PET_ep, 12)
    # plot(cl_110300KS_SPEI12_ep)

cl_180102CA_SPEI12_ep <- SPEI::spei(cl_180102CA_P_PET_ep, 12)
    # plot(cl_180102CA_SPEI12_ep)

######################################################################
### PLOT SPEI using ggplot ###
## COMBINE date column with SPEI results, RENAME columns, REFORMAT date column 
cl_040500MI_SPEI12_hd_df <- data.frame(cl_040500MI_P_PET$DATE, cl_040500MI_SPEI12_hd[["fitted"]])
  colnames(cl_040500MI_SPEI12_hd_df) <- c("DATE", "SPEI-04050001", "SPEI-04050003", "SPEI-04050004", "SPEI-04050005", "SPEI-04050006", "SPEI-04050007")
  cl_040500MI_SPEI12_hd_df$DATE <- as.Date(cl_040500MI_SPEI12_hd_df$DATE)
  
cl_110300KS_SPEI12_hd_df <- data.frame(cl_110300KS_P_PET$DATE, cl_110300KS_SPEI12_hd[["fitted"]])
  colnames(cl_110300KS_SPEI12_hd_df) <- c("DATE", "SPEI")
                                          # , "SPEI-11030004", "SPEI-11030005", "SPEI-11030008", "SPEI-11030009", "SPEI-11030010", "SPEI-11030011", "SPEI-11030012", "SPEI-11030013", "SPEI-11030016", "SPEI-11030017", "SPEI-11030018")
  cl_110300KS_SPEI12_hd_df$DATE <- as.Date(cl_110300KS_SPEI12_hd_df$DATE)

cl_180102CA_SPEI12_hd_df <- data.frame(cl_180102CA_P_PET$DATE, cl_180102CA_SPEI12_hd[["fitted"]])
  colnames(cl_180102CA_SPEI12_hd_df) <- c("DATE", "SPEI-18010201", "SPEI-18010202", "SPEI-18010204", "SPEI-18010206", "SPEI-18010207", "SPEI-18010208", "SPEI-18010209", "SPEI-18010210", "SPEI-18010211")
  cl_180102CA_SPEI12_hd_df$DATE <- as.Date(cl_180102CA_SPEI12_hd_df$DATE)
  

cl_040500MI_SPEI12_long_df <- data.frame(cl_040500MI_P_PET$DATE, cl_040500MI_SPEI12_long[["fitted"]])
  colnames(cl_040500MI_SPEI12_long_df) <- c("DATE", "SPEI-04050004", "SPEI-04050006")
  cl_040500MI_SPEI12_long_df$DATE <- as.Date(cl_040500MI_SPEI12_long_df$DATE)
  
cl_110300KS_SPEI12_wlong_df <- data.frame(cl_110300KS_P_PET$DATE, cl_110300KS_SPEI12_wlong[["fitted"]])
  colnames(cl_110300KS_SPEI12_wlong_df) <- c("YEAR", "SPEI-12")
                                            # "SPEI-11030001", "SPEI-11030003", "SPEI-11030012", "SPEI-11030013", "SPEI-11030018")
  cl_110300KS_SPEI12_wlong_df$YEAR <- as.Date(cl_110300KS_SPEI12_wlong_df$YEAR)

cl_110300KS_SPEI12_elong_df <- data.frame(cl_110300KS_P_PET$DATE, cl_110300KS_SPEI12_elong[["fitted"]])
  colnames(cl_110300KS_SPEI12_elong_df) <- c("YEAR", "SPEI-12")
  # "SPEI-11030001", "SPEI-11030003", "SPEI-11030012", "SPEI-11030013", "SPEI-11030018")
  cl_110300KS_SPEI12_elong_df$YEAR <- as.Date(cl_110300KS_SPEI12_elong_df$YEAR)

cl_110300KS_SPEI12_clong_df <- data.frame(cl_110300KS_P_PET$DATE, cl_110300KS_SPEI12_clong[["fitted"]])
  colnames(cl_110300KS_SPEI12_clong_df) <- c("YEAR", "SPEI-12")
  # "SPEI-11030001", "SPEI-11030003", "SPEI-11030012", "SPEI-11030013", "SPEI-11030018")
  cl_110300KS_SPEI12_clong_df$YEAR <- as.Date(cl_110300KS_SPEI12_clong_df$YEAR)
  
    
cl_180102CA_SPEI12_long_df <- data.frame(cl_180102CA_P_PET$DATE, cl_180102CA_SPEI12_long[["fitted"]])
  colnames(cl_180102CA_SPEI12_long_df) <- c("DATE", "SPEI-18010201", "SPEI-18010202", "SPEI-18010206", "SPEI-18010207", "SPEI-18010209", "SPEI-18010210", "SPEI-18010211")
  cl_180102CA_SPEI12_long_df$DATE <- as.Date(cl_180102CA_SPEI12_long_df$DATE)
  

cl_040500MI_SPEI12_ep_df <- data.frame(cl_040500MI_P_PET$DATE, cl_040500MI_SPEI12_ep[["fitted"]])
  colnames(cl_040500MI_SPEI12_ep_df) <- c("DATE", "SPEI-04050004", "SPEI-04050006")
  cl_040500MI_SPEI12_ep_df$DATE <- as.Date(cl_040500MI_SPEI12_ep_df$DATE)
  
cl_110300KS_SPEI12_ep_df <- data.frame(cl_110300KS_P_PET$DATE, cl_110300KS_SPEI12_ep[["fitted"]])
  colnames(cl_110300KS_SPEI12_ep_df) <- c("DATE", "SPEI-11030001", "SPEI-11030004", "SPEI-11030005", "SPEI-11030012", "SPEI-11030013", "SPEI-11030018")
  cl_110300KS_SPEI12_ep_df$DATE <- as.Date(cl_110300KS_SPEI12_ep_df$DATE)
 
cl_180102CA_SPEI12_ep_df <- data.frame(cl_180102CA_P_PET$DATE, cl_180102CA_SPEI12_ep[["fitted"]])
  colnames(cl_180102CA_SPEI12_ep_df) <- c("DATE", "SPEI-18010201", "SPEI-18010202", "SPEI-18010203", "SPEI-18010204", "SPEI-18010206", "SPEI-18010207", "SPEI-18010209", "SPEI-18010210", "SPEI-18010211")
  cl_180102CA_SPEI12_ep_df$DATE <- as.Date(cl_180102CA_SPEI12_ep_df$DATE)

  
## PLOT SPEI results with ggplot
# p_cl_040500MI_SPEI12 <- ggplot2::ggplot(cl_040500MI_SPEI12, aes(x = DATE)) +
#   geom_col(aes(y = `SPEI-04050006`, fill = `SPEI-04050006`)) +
#   scale_fill_steps2(low = "red", mid = "grey50", high = "blue") +
#   geom_hline(yintercept = mean(!is.na(as.double(cl_040500MI_SPEI12$`SPEI-04050006`))), colour = "green") +
#   geom_vline(xintercept = 1958, color = "black") +
#   geom_vline(xintercept = 1901, color = "black") 
# p_cl_huc040500MI_SPEI12 
# 

  
  cl_110300KS_SPEI12_wlong_df$MA2yr <- zoo::rollapply(cl_110300KS_SPEI12_wlong_df$`SPEI-12`, 2, mean, align = "right", fill = NA)
  cl_110300KS_SPEI12_elong_df$MA2yr <- zoo::rollapply(cl_110300KS_SPEI12_elong_df$`SPEI-12`, 2, mean, align = "right", fill = NA)
  cl_110300KS_SPEI12_clong_df$MA2yr <- zoo::rollapply(cl_110300KS_SPEI12_clong_df$`SPEI-12`, 2, mean, align = "right", fill = NA)
  
png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/SPEI_KS_long/SPEI_ecw_long.png", sep = ""), width = 1586, height = 770, unit = "px")

## PLOT SPEI of westernmost HUC08 waterhsed  
# png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/SPEI_KS_long/SPEI_11030001_long.png", sep = ""), width = 1586, height = 269, unit = "px")
  
p_cl_110300KS_SPEI12_wlong <- ggplot2::ggplot(cl_110300KS_SPEI12_wlong_df, aes(x = YEAR)) +
  geom_col(aes(y = `SPEI-12`, fill = `SPEI-12`)) +
  geom_line(aes(y = MA2yr)) +
  scale_fill_steps2(low = "red", mid = "grey50", high = "blue") +
  # scale_x_date(limits = as.Date(c("1962-10-01", "2021-09-30"))) +
  scale_x_date(limits = as.Date(c("1901-10-01", "2022-10-01")), date_breaks = "10 years", date_labels = "%Y") +
  ylim(min = -2, max = 3) +
  theme(plot.title = element_text(size = 28), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.position = "none") +
  # geom_hline(yintercept = mean(!is.na(cl_110300KS_SPEI12$`SPEI-11030001`)), colour = "green") +
  geom_vline(xintercept = as.Date("1962-10-01"), color = "black", linetype = 2) +
  geom_line(aes(y = MA2yr)) +
  labs(title = "Western Middle Arkansas River Watershed (HUC 11030001)")
# print(p_cl_110300KS_SPEI12_long11030001)

## PLOT SPEI of easternmost HUC08 watershed
# png(file = paste("C:/Users/e781p134/OneDrive - University of Kansas/Documents/Powell/DiscoverStreams/plots/SPEI_KS_long/SPEI_11030018_long.png", sep = ""), width = 1586, height = 269, unit = "px")

p_cl_110300KS_SPEI12_elong <- ggplot2::ggplot(cl_110300KS_SPEI12_elong_df, aes(x = YEAR)) +
  geom_col(aes(y = `SPEI-12`, fill = `SPEI-12`)) +
  geom_line(aes(y = MA2yr)) +
  scale_fill_steps2(low = "red", mid = "grey50", high = "blue") +
  # scale_x_date(limits = as.Date(c("1962-10-01", "2021-09-30"))) +
  scale_x_date(limits = as.Date(c("1901-10-01", "2022-10-01")), date_breaks = "10 years", date_labels = "%Y") +
  ylim(min = -2, max = 3) +
  theme(plot.title = element_text(size = 28), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.position = "none") +
  # geom_hline(yintercept = mean(!is.na(cl_110300KS_SPEI12$`SPEI-11030001`)), colour = "green") +
  geom_vline(xintercept = as.Date("1962-10-01"), color = "black", linetype = 2) +
  labs(title = "Eastern Middle Arkansas River Watershed (HUC 11030018)")
# print(p_cl_110300KS_SPEI12_long11030018)

p_cl_110300KS_SPEI12_clong <- ggplot2::ggplot(cl_110300KS_SPEI12_clong_df, aes(x = YEAR)) +
  geom_col(aes(y = `SPEI-12`, fill = `SPEI-12`)) +
  geom_line(aes(y = MA2yr)) +
  scale_fill_steps2(low = "red", mid = "grey50", high = "blue") +
  # scale_x_date(limits = as.Date(c("1962-10-01", "2021-09-30"))) +
  scale_x_date(limits = as.Date(c("1901-10-01", "2022-10-01")), date_breaks = "10 years", date_labels = "%Y") +
  ylim(min = -2, max = 3) +
  theme(plot.title = element_text(size = 28), axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 26), legend.title = element_text(size = 26), legend.key.height = unit(1.35, "cm"), legend.key.width = unit(1, "cm")) +
  # geom_hline(yintercept = mean(!is.na(cl_110300KS_SPEI12$`SPEI-11030001`)), colour = "green") +
  geom_vline(xintercept = as.Date("1962-10-01"), color = "black", linetype = 2) +
  labs(title = "Central Middle Arkansas River Watershed (HUC 11030009)")
# print(p_cl_110300KS_SPEI12_long11030018)

## COMBINE west and east plots
p_cl_combo <- p_cl_110300KS_SPEI12_wlong + p_cl_110300KS_SPEI12_clong + p_cl_110300KS_SPEI12_elong + plot_layout(nrow = 3, guides = "collect")
print(p_cl_combo)

## CLOSE PNG connection
dev.off()

# 
# p_cl_180102CA_SPEI12 <- ggplot2::ggplot(cl_180102CA_SPEI12, aes(x = DATE)) +
#   geom_col(aes(y = `SPEI-18010211`, fill = `SPEI-18010211`)) +
#   scale_fill_steps2(low = "red", mid = "grey50", high = "blue") +
#   geom_hline(yintercept = mean(!is.na(cl_180102CA_SPEI12$`SPEI-18010211`)), colour = "green") +
#   geom_vline(xintercept = 1964, color = "black") +
#   geom_vline(xintercept = 1902, color = "black") 
# p_cl_180102CA_SPEI12 



### PLOT high-density watersheds ###
## MELT SPEI-12 results for hd watersheds in MI then PLOT
cl_040500MI_SPEI12_hd_melt <- reshape2::melt(cl_040500MI_SPEI12_hd_df, measure.vars = 2:ncol(cl_040500MI_SPEI12_hd_df), variable.name = "Watershed", value.name = "SPEI")
  cl_040500MI_SPEI12_hd_melt$DATE <- as.Date(cl_040500MI_SPEI12_hd_melt$DATE)
    
p_cl_040500MI_SPEI12_hd_melt <- ggplot2::ggplot(cl_040500MI_SPEI12_hd_melt, aes(x = DATE, y = SPEI, group = Watershed, color = Watershed)) +
  geom_line() +
  geom_vline(xintercept = as.Date("1959-10-01"), color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_color_manual(values = c("darkorchid", "deepskyblue2", "chartreuse3", "goldenrod1", "darkorange1", "red3")) +
  scale_x_date(limits = as.Date(c("1955-10-01", "1965-10-01"))) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_cl_040500MI_SPEI12_hd_melt 


## MELT SPEI-12 results for hd watersheds in KS then PLOT
cl_110300KS_SPEI12_hd_melt <- reshape2::melt(cl_110300KS_SPEI12_hd_df, measure.vars = 2:ncol(cl_110300KS_SPEI12_hd_df), variable.name = "Watershed", value.name = "SPEI")
cl_110300KS_SPEI12_hd_melt$DATE <- as.Date(cl_110300KS_SPEI12_hd_melt$DATE)

p_cl_110300KS_SPEI12_hd_melt <- ggplot2::ggplot(cl_110300KS_SPEI12_hd_melt, aes(x = DATE, y = SPEI, group = Watershed, color = Watershed)) +
  geom_line() +
  # geom_vline(xintercept = as.Date("1962-10-01"), color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_color_manual(values = c("darkorchid", "violet", "blue", "deepskyblue2", "turquoise3", "springgreen4", "chartreuse2", "yellow", "goldenrod1", "orange", "darkorange1", "red3")) +
  # scale_x_date(limits = as.Date(c("1955-10-01", "1965-10-01"))) +
  scale_x_date(limits = as.Date(c("1962-10-01", "2021-09-30"))) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_cl_110300KS_SPEI12_hd_melt


## MELT SPEI-12 results for hd watersheds in CA then PLOT
cl_180102CA_SPEI12_hd_melt <- reshape2::melt(cl_180102CA_SPEI12_hd_df, measure.vars = 2:ncol(cl_180102CA_SPEI12_hd_df), variable.name = "Watershed", value.name = "SPEI")
cl_180102CA_SPEI12_hd_melt$DATE <- as.Date(cl_180102CA_SPEI12_hd_melt$DATE)

p_cl_180102CA_SPEI12_hd_melt <- ggplot2::ggplot(cl_180102CA_SPEI12_hd_melt, aes(x = DATE, y = SPEI, group = Watershed, color = Watershed)) +
  geom_line() +
  geom_vline(xintercept = as.Date("1961-10-01"), color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_color_manual(values = c("darkorchid", "violet", "blue", "deepskyblue2", "turquoise3",  "chartreuse2", "goldenrod1", "orange", "red3")) +
  scale_x_date(limits = as.Date(c("1955-10-01", "1965-10-01"))) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_cl_180102CA_SPEI12_hd_melt


### PLOT long-term watersheds ###
## MELT SPEI-12 results for long watersheds in MI then PLOT
cl_040500MI_SPEI12_long_melt <- reshape2::melt(cl_040500MI_SPEI12_long_df, measure.vars = 2:ncol(cl_040500MI_SPEI12_long_df), variable.name = "Watershed", value.name = "SPEI")
cl_040500MI_SPEI12_long_melt$DATE <- as.Date(cl_040500MI_SPEI12_long_melt$DATE)

p_cl_040500MI_SPEI12_long_melt <- ggplot2::ggplot(cl_040500MI_SPEI12_long_melt, aes(x = DATE, y = SPEI, group = Watershed, fill = Watershed)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = c("darkorchid", "chartreuse3")) +
  # scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_cl_040500MI_SPEI12_long_melt 


 ## MELT SPEI-12 results for long watersheds in KS then PLOT
cl_110300KS_SPEI12_long_melt <- reshape2::melt(cl_110300KS_SPEI12_long_df, measure.vars = 2:ncol(cl_110300KS_SPEI12_long_df), variable.name = "SPEI-12", value.name = "SPEI-12")
cl_110300KS_SPEI12_long_melt$DATE <- as.Date(cl_110300KS_SPEI12_long_melt$DATE)

p_cl_110300KS_SPEI12_long_melt <- ggplot2::ggplot(cl_110300KS_SPEI12_long_melt, aes(x = DATE, y = SPEI, group = Watershed, fill = Watershed)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = c("darkorchid", "deepskyblue2", "chartreuse2", "goldenrod1", "red3")) +
  # scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
print(p_cl_110300KS_SPEI12_long_melt)


## MELT SPEI-12 results for long watersheds in CA then PLOT
cl_180102CA_SPEI12_long_melt <- reshape2::melt(cl_180102CA_SPEI12_long_df, measure.vars = 2:ncol(cl_180102CA_SPEI12_long_df), variable.name = "Watershed", value.name = "SPEI")
cl_180102CA_SPEI12_long_melt$DATE <- as.Date(cl_180102CA_SPEI12_long_melt$DATE)

p_cl_180102CA_SPEI12_long_melt <- ggplot2::ggplot(cl_180102CA_SPEI12_long_melt, aes(x = DATE, y = SPEI, group = Watershed, fill = Watershed)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = c("darkorchid", "blue", "deepskyblue2", "chartreuse2", "yellow", "orange", "red3")) +
  # scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_cl_180102CA_SPEI12_long_melt  


### PLOT early-period watersheds ###
## MELT SPEI-12 results for ep watersheds in MI then PLOT
cl_040500MI_SPEI12_ep_melt <- reshape2::melt(cl_040500MI_SPEI12_ep_df, measure.vars = 2:ncol(cl_040500MI_SPEI12_ep_df), variable.name = "Watershed", value.name = "SPEI")
cl_040500MI_SPEI12_ep_melt$DATE <- as.Date(cl_040500MI_SPEI12_ep_melt$DATE)

p_cl_040500MI_SPEI12_ep_melt <- ggplot2::ggplot(cl_040500MI_SPEI12_ep_melt, aes(x = DATE, y = SPEI, group = Watershed, fill = Watershed)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = c("darkorchid", "chartreuse3")) +
  # scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  scale_x_date(limits = as.Date(c("1900-10-01", "1940-09-30"))) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_cl_040500MI_SPEI12_ep_melt 


## MELT SPEI-12 results for ep watersheds in KS then PLOT
cl_110300KS_SPEI12_ep_melt <- reshape2::melt(cl_110300KS_SPEI12_ep_df, measure.vars = 2:ncol(cl_110300KS_SPEI12_ep_df), variable.name = "Watershed", value.name = "SPEI")
cl_110300KS_SPEI12_ep_melt$DATE <- as.Date(cl_110300KS_SPEI12_ep_melt$DATE)

p_cl_110300KS_SPEI12_ep_melt <- ggplot2::ggplot(cl_110300KS_SPEI12_ep_melt, aes(x = DATE, y = SPEI, group = Watershed, fill = Watershed)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = c("darkorchid", "deepskyblue2", "chartreuse2", "yellow", "orange", "red3")) +
  # scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  scale_x_date(limits = as.Date(c("1900-10-01", "1940-09-30"))) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_cl_110300KS_SPEI12_ep_melt


## MELT SPEI-12 results for ep watersheds in CA then PLOT
cl_180102CA_SPEI12_ep_melt <- reshape2::melt(cl_180102CA_SPEI12_ep_df, measure.vars = 2:ncol(cl_180102CA_SPEI12_ep_df), variable.name = "Watershed", value.name = "SPEI")
cl_180102CA_SPEI12_ep_melt$DATE <- as.Date(cl_180102CA_SPEI12_ep_melt$DATE)

p_cl_180102CA_SPEI12_ep_melt <- ggplot2::ggplot(cl_180102CA_SPEI12_ep_melt, aes(x = DATE, y = SPEI, group = Watershed, fill = Watershed)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = c("violet", "darkorchid", "blue", "deepskyblue2", "springgreen4", "chartreuse2", "yellow", "orange", "red3")) +
  # scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  scale_x_date(limits = as.Date(c("1900-10-01", "1940-09-30"))) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_cl_180102CA_SPEI12_ep_melt  



######################################################################################
### VERIFY CLIMATE NEUTRAL YEARS ###
## SELECT years where SPEI is neutral (+/- 0.25)
cl_040500MI_SPEI12_hd_neutral <- cl_040500MI_SPEI12_hd_melt[cl_040500MI_SPEI12_hd_melt$SPEI <= 0.25 & cl_040500MI_SPEI12_hd_melt$SPEI >= -0.25, ]







#########################################################################################  
### CLIMATE (NOAA) ###
## RUN corresponding streamflow dataframe for each HUC06, also set HUC06 in dataframe setup and in for loop
cl_040500MI <- data.frame()
cl_110300KS <- data.frame()
cl_180102CA <- data.frame()

MI_ncdc_datasets <- rnoaa::ncdc_datasets(locationid = "HUC:040500", datasetid = "NORMAL_DLY")
MI_ncdc <- rnoaa::ncdc(datasetid = "GHCND", datatypeid = "PRCP", locationid = "HUC:040500", startdate = "2021-01-01", enddate = "2022-01-01", add_units = TRUE)
ncdc_plot(MI_ncdc, breaks = "1 month", dateformat = "%d/%m")

station_data <- ghcnd_stations()
MI_8 <- data.frame(name = huc040500MI_ws[8,2], lat = huc040500MI_ws[8,3], lon = huc040500MI_ws[8,4], station_data = station_data, radius = 10)

MI_meteo_stations <- rnoaa::meteo_nearby_stations(MI_8, lat_colname = "lat", lon_colname = "lon", station_data = ghcnd_stations())
