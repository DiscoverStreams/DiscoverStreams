# database connection/retrieval libraries
library(readr)
# data manipulation libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(SPEI)

## READ csv files containing climate data for all watersheds
MWBM_PET <- read.csv(file = "C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/MWBM_PET_mm_KS_CA_MI_HUC8_climgrid.csv", header = TRUE, col.names = c("11030001", "11030002", "11030003", "11030004", "11030005", "11030006", "11030007", "11030008", "11030009", "11030010", "11030011", "11030012", "11030013", "11030014", "11030015", "11030016", "11030017", "11030018", "18010201", "18010202", "18010203", "18010204", "18010205", "18010206", "18010207", "18010208", "18010209", "18010210", "18010211", "18010212", "04050001", "04050002", "04050003", "04050004", "04050005", "04050006", "04050007", "DATE"))

MWBM_PRCP <- read.csv(file = "C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/MWBM_PRCP_mm_KS_CA_MI_HUC8_climgrid.csv", header = TRUE, col.names = c("11030001", "11030002", "11030003", "11030004", "11030005", "11030006", "11030007", "11030008", "11030009", "11030010", "11030011", "11030012", "11030013", "11030014", "11030015", "11030016", "11030017", "11030018", "18010201", "18010202", "18010203", "18010204", "18010205", "18010206", "18010207", "18010208", "18010209", "18010210", "18010211", "18010212", "04050001", "04050002", "04050003", "04050004", "04050005", "04050006", "04050007", "DATE"))


## SUBSET to watersheds
huc040500MI_cl_PET <- MWBM_PET[ , c(38, 31:37)]
huc040500MI_cl_PRCP <- MWBM_PRCP[ , c(38, 31:37)]

huc110300KS_cl_PET <- MWBM_PET[ , c(38, 1:18)]
huc110300KS_cl_PRCP <- MWBM_PRCP[ , c(38, 1:18)]

huc180102CA_cl_PET <- MWBM_PET[ , c(38, 19:30)]
huc180102CA_cl_PRCP <- MWBM_PRCP[ , c(38, 19:30)]

cl_date <- MWBM_PET$DATE

## CALCULATE P-PET for use as input in SPEI calculation for each watershed
## https://stackoverflow.com/questions/28117409/how-do-you-subtract-two-data-frames-from-one-another-in-r
huc040500MI_cl_P_PET <- bind_rows(huc040500MI_cl_PET, huc040500MI_cl_PRCP) %>%
  group_by(DATE) %>%
  summarise_each(funs(diff(.))) %>%
  data.frame()

huc110300KS_cl_P_PET <- bind_rows(huc110300KS_cl_PET, huc110300KS_cl_PRCP) %>%
  group_by(DATE) %>%
  summarise_each(funs(diff(.))) %>%
  data.frame()

huc180102CA_cl_P_PET <- bind_rows(huc180102CA_cl_PET, huc180102CA_cl_PRCP) %>%
  group_by(DATE) %>%
  summarise_each(funs(diff(.))) %>%
  data.frame()

## SELECT watersheds specific to high-density gage stations of interest, ** MI has all watersheds represented by a gage station
huc040500MI_cl_P_PET_hd <- huc040500MI_cl_P_PET
huc110300KS_cl_P_PET_hd <- huc110300KS_cl_P_PET[ , c("X11030001", "X11030004", "X11030005", "X11030008", "X11030009", "X11030010", "X11030011", "X11030012", "X11030013", "X11030016", "X11030017", "X11030018")]
huc180102CA_cl_P_PET_hd <- huc180102CA_cl_P_PET[ , c("X18010201", "X18010202", "X18010204", "X18010206", "X18010207", "X18010208", "X18010209", "X18010210", "X18010211")]

## SELECT watersheds specific to long-term gage stations of interest
huc040500MI_cl_P_PET_long <- huc040500MI_cl_P_PET[ , c("X04050004", "X04050006")]
huc110300KS_cl_P_PET_long <- huc110300KS_cl_P_PET[ , c("X11030001", "X11030003", "X11030012", "X11030013", "X11030018")]
huc180102CA_cl_P_PET_long <- huc180102CA_cl_P_PET[ , c("X18010201", "X18010202", "X18010206", "X18010210", "X18010211", "X18010212")]

## CALCULATE SPEI, exclude column 1(DATE)
huc040500MI_cl_SPEI1 <- SPEI::spei(huc040500MI_cl_P_PET[2:8], 1)

huc040500MI_cl_SPEI12_long <- SPEI::spei(huc040500MI_cl_P_PET_long, 12)
  plot(huc040500MI_cl_SPEI12_long)

huc110300KS_cl_SPEI12_long <- SPEI::spei(huc110300KS_cl_P_PET_long, 12)
  plot(huc110300KS_cl_SPEI12_long)

huc180102CA_cl_SPEI12_long <- SPEI::spei(huc180102CA_cl_P_PET_long, 12)
  plot(huc180102CA_cl_SPEI12_long)

## COMBINE date column with SPEI results, RENAME columns, REFORMAT date column 
huc040500MI_SPEI12 <- data.frame(huc040500MI_cl_P_PET$DATE, huc040500MI_cl_SPEI12[["fitted"]])
  colnames(huc040500MI_SPEI12) <- c("DATE", "SPEI-04050001", "SPEI-04050002", "SPEI-04050003", "SPEI-04050004", "SPEI-04050005", "SPEI-04050006", "SPEI-04050007")
  huc040500MI_SPEI12$DATE <- as.Date(huc040500MI_SPEI12$DATE)
  
huc110300KS_SPEI12 <- data.frame(huc110300KS_cl_P_PET$DATE, huc110300KS_cl_SPEI12[["fitted"]])
  colnames(huc110300KS_SPEI12) <- c("DATE", "SPEI-11030001", "SPEI-11030002", "SPEI-11030003", "SPEI-11030004", "SPEI-11030005", "SPEI-11030006", "SPEI-11030007", "SPEI-11030008", "SPEI-11030009", "SPEI-11030010", "SPEI-11030011", "SPEI-11030012", "SPEI-11030013", "SPEI-11030014", "SPEI-11030015", "SPEI-11030016", "SPEI-11030017", "SPEI-11030018")
  huc110300KS_SPEI12$DATE <- as.Date(huc110300KS_SPEI12$DATE)
  
huc180102CA_SPEI12 <- data.frame(huc180102CA_cl_P_PET$DATE, huc180102CA_cl_SPEI12[["fitted"]])
  colnames(huc180102CA_SPEI12) <- c("DATE", "SPEI-18010201", "SPEI-18010202", "SPEI-18010203", "SPEI-18010204", "SPEI-18010205", "SPEI-18010206", "SPEI-18010207", "SPEI-18010208", "SPEI-18010209", "SPEI-18010210", "SPEI-18010211", "SPEI-18010212")
  huc180102CA_SPEI12$DATE <- as.Date(huc180102CA_SPEI12$DATE)

  
## PLOT SPEI results with ggplot
p_huc040500MI_SPEI12 <- ggplot2::ggplot(huc040500MI_SPEI12, aes(x = DATE)) +
  geom_col(aes(y = `SPEI-04050006`, fill = `SPEI-04050006`)) +
  scale_fill_steps2(low = "red", mid = "grey50", high = "blue") +
  geom_hline(yintercept = mean(!is.na(as.double(huc040500MI_SPEI12$`SPEI-04050006`))), colour = "green") +
  geom_vline(xintercept = 1958, color = "black") +
  geom_vline(xintercept = 1901, color = "black") 
p_huc040500MI_SPEI12 

p_huc110300KS_SPEI12 <- ggplot2::ggplot(huc110300KS_SPEI12, aes(x = DATE)) +
  geom_col(aes(y = `SPEI-11030018`, fill = `SPEI-11030018`)) +
  scale_fill_steps2(low = "red", mid = "grey50", high = "blue") +
  geom_hline(yintercept = mean(!is.na(huc110300KS_SPEI12$`SPEI-11030018`)), colour = "green") +
  geom_vline(xintercept = 1962, color = "black") +
  geom_vline(xintercept = 1902, color = "black") 
p_huc110300KS_SPEI12 

p_huc180102CA_SPEI12 <- ggplot2::ggplot(huc180102CA_SPEI12, aes(x = DATE)) +
  geom_col(aes(y = `SPEI-18010211`, fill = `SPEI-18010211`)) +
  scale_fill_steps2(low = "red", mid = "grey50", high = "blue") +
  geom_hline(yintercept = mean(!is.na(huc180102CA_SPEI12$`SPEI-18010211`)), colour = "green") +
  geom_vline(xintercept = 1964, color = "black") +
  geom_vline(xintercept = 1902, color = "black") 
p_huc180102CA_SPEI12 




## MELT data for plotting all HUC08 watersheds together
huc040500MI_SPEI12_melt <- reshape2::melt(huc040500MI_SPEI12, measure.vars = 2:ncol(huc040500MI_SPEI12), variable.name = "Watershed", value.name = "SPEI")
  huc040500MI_SPEI12_melt$DATE <- as.Date(huc040500MI_SPEI12_melt$DATE)
    
p_huc040500MI_SPEI12_melt <- ggplot2::ggplot(huc040500MI_SPEI12_melt, aes(x = DATE, y = SPEI, group = Watershed, color = Watershed)) +
  geom_line() +
  geom_vline(xintercept = as.Date("1960-01-01"), color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_color_manual(values = c("darkorchid", "deepskyblue2", "turquoise3", "chartreuse3", "goldenrod1", "darkorange1", "red3")) +
  scale_x_date(limits = as.Date(c("1955-10-01", "1965-10-01"))) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_huc040500MI_SPEI12_melt 

## SELECT watersheds specific to high-density gage stations of interest, ** MI has all watersheds represented by a gage station
huc110300KS_SPEI12_hd <- huc110300KS_SPEI12[ , c("DATE", "SPEI-11030001", "SPEI-11030004", "SPEI-11030005", "SPEI-11030008", "SPEI-11030009", "SPEI-11030010", "SPEI-11030011", "SPEI-11030012", "SPEI-11030013", "SPEI-11030016", "SPEI-11030017", "SPEI-11030018")]

huc110300KS_SPEI12_hd_melt <- reshape2::melt(huc110300KS_SPEI12_hd, measure.vars = 2:ncol(huc110300KS_SPEI12_hd), variable.name = "Watershed", value.name = "SPEI")
huc110300KS_SPEI12_hd_melt$DATE <- as.Date(huc110300KS_SPEI12_hd_melt$DATE)

p_huc110300KS_SPEI12_melt <- ggplot2::ggplot(huc110300KS_SPEI12_hd_melt, aes(x = DATE, y = SPEI, group = Watershed, color = Watershed)) +
  geom_line() +
  geom_vline(xintercept = as.Date("1963-01-01"), color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_color_manual(values = c("darkorchid", "violet", "blue", "deepskyblue2", "turquoise3", "springgreen4", "chartreuse2", "yellow", "goldenrod1", "orange", "darkorange1", "red3")) +
  scale_x_date(limits = as.Date(c("1955-10-01", "1965-10-01"))) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_huc110300KS_SPEI12_melt

## SELECT watersheds specific to high-density gage stations of interest, ** MI has all watersheds represented by a gage station
huc180102CA_SPEI12_hd <- huc180102CA_SPEI12[ , c("DATE", "SPEI-18010201", "SPEI-18010202", "SPEI-18010204", "SPEI-18010206", "SPEI-18010207", "SPEI-18010208", "SPEI-18010209", "SPEI-18010210", "SPEI-18010211")]
huc180102CA_SPEI12_hd_melt <- reshape2::melt(huc180102CA_SPEI12_hd, measure.vars = 2:ncol(huc180102CA_SPEI12_hd), variable.name = "Watershed", value.name = "SPEI")
huc180102CA_SPEI12_hd_melt$DATE <- as.Date(huc180102CA_SPEI12_hd_melt$DATE)

p_huc180102CA_SPEI12_melt <- ggplot2::ggplot(huc180102CA_SPEI12_hd_melt, aes(x = DATE, y = SPEI, group = Watershed, color = Watershed)) +
  geom_line() +
  geom_vline(xintercept = as.Date("1962-01-01"), color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_color_manual(values = c("darkorchid", "violet", "blue", "deepskyblue2", "turquoise3",  "chartreuse2", "goldenrod1", "orange", "red3")) +
  scale_x_date(limits = as.Date(c("1955-10-01", "1965-10-01"))) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c(-2, -1, 0, 1, 2)) +
  theme(axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26), axis.text = element_text(size = 28), legend.text = element_text(size = 18), legend.title = element_text(size = 18))
p_huc180102CA_SPEI12_melt

  
## SELECT years where SPEI is neutral (+/- 0.25)
huc040500MI_SPEI12_melt <- huc040500MI_SPEI12_melt[huc040500MI_SPEI12_melt$SPEI <= 0.25 & huc040500MI_SPEI12_melt$SPEI >= -0.25, ]

huc110300KS_SPEI12
huc180102CA_SPEI12