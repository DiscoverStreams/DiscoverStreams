# data manipulation libraries
library(tidyverse)
library(ggplot2)
# data retrieval libraries
library(readr)

## RETRIEVE CSV files of PDSI data
CA_PDSI_JAN <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/January/CA-pdsi-JAN-1895-2022.csv", skip = 3)
CA_PDSI_JAN$Date <- gsub('.{2}$', '', CA_PDSI_JAN$Date)
CA_PDSI_JAN$Date <- as.numeric(CA_PDSI_JAN$Date)
CA_PDSI_JAN <- CA_PDSI_JAN[ , 1:2]
colnames(CA_PDSI_JAN) <- c("Date", "PDSI")
CA_PDSI_JAN$Watershed <- "California"

CA_PDSI_OCT <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/October/CA-pdsi-OCT-1895-2022.csv", skip = 3)
CA_PDSI_OCT$Date <- gsub('.{2}$', '', CA_PDSI_OCT$Date)
CA_PDSI_OCT$Date <- as.numeric(CA_PDSI_OCT$Date)
CA_PDSI_OCT <- CA_PDSI_OCT[ , 1:2]
colnames(CA_PDSI_OCT) <- c("Date", "PDSI")
CA_PDSI_OCT$Watershed <- "California"

IN_PDSI_JAN <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/January/IN-pdsi-JAN-1895-2022.csv", skip = 3)
IN_PDSI_JAN$Date <- gsub('.{2}$', '', IN_PDSI_JAN$Date)
IN_PDSI_JAN$Date <- as.numeric(IN_PDSI_JAN$Date)
IN_PDSI_JAN <- IN_PDSI_JAN[ , 1:2]
colnames(IN_PDSI_JAN) <- c("Date", "PDSI")
IN_PDSI_JAN$Watershed <- "Indiana"

IN_PDSI_OCT <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/October/IN-pdsi-OCT-1895-2022.csv", skip = 3)
IN_PDSI_OCT$Date <- gsub('.{2}$', '', IN_PDSI_OCT$Date)
IN_PDSI_OCT$Date <- as.numeric(IN_PDSI_OCT$Date)
IN_PDSI_OCT <- IN_PDSI_OCT[ , 1:2]
colnames(IN_PDSI_OCT) <- c("Date", "PDSI")
IN_PDSI_OCT$Watershed <- "Indiana"

KS_PDSI_JAN <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/January/KS-pdsi-JAN-1895-2022.csv", skip = 3)
KS_PDSI_JAN$Date <- gsub('.{2}$', '', KS_PDSI_JAN$Date)
KS_PDSI_JAN$Date <- as.numeric(KS_PDSI_JAN$Date)
KS_PDSI_JAN <- KS_PDSI_JAN[ , 1:2]
colnames(KS_PDSI_JAN) <- c("Date", "PDSI")
KS_PDSI_JAN$Watershed <- "Kansas"

KS_PDSI_OCT <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/October/KS-pdsi-OCT-1895-2022.csv", skip = 3)
KS_PDSI_OCT$Date <- gsub('.{2}$', '', KS_PDSI_OCT$Date)
KS_PDSI_OCT$Date <- as.numeric(KS_PDSI_OCT$Date)
KS_PDSI_OCT <- KS_PDSI_OCT[ , 1:2]
colnames(KS_PDSI_OCT) <- c("Date", "PDSI")
KS_PDSI_OCT$Watershed <- "Kansas"

MI_PDSI_JAN <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/January/MI-pdsi-JAN-1895-2022.csv", skip = 3)
MI_PDSI_JAN$Date <- gsub('.{2}$', '', MI_PDSI_JAN$Date)
MI_PDSI_JAN$Date <- as.numeric(MI_PDSI_JAN$Date)
MI_PDSI_JAN <- MI_PDSI_JAN[ , 1:2]
colnames(MI_PDSI_JAN) <- c("Date", "PDSI")
MI_PDSI_JAN$Watershed <- "Michigan"

MI_PDSI_OCT <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/October/MI-pdsi-OCT-1895-2022.csv", skip = 3)
MI_PDSI_OCT$Date <- gsub('.{2}$', '', MI_PDSI_OCT$Date)
MI_PDSI_OCT$Date <- as.numeric(MI_PDSI_OCT$Date)
MI_PDSI_OCT <- MI_PDSI_OCT[ , 1:2]
colnames(MI_PDSI_OCT) <- c("Date", "PDSI")
MI_PDSI_OCT$Watershed <- "Michigan"

OR_PDSI_JAN <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/January/OR-pdsi-JAN-1895-2022.csv", skip = 3)
OR_PDSI_JAN$Date <- gsub('.{2}$', '', OR_PDSI_JAN$Date)
OR_PDSI_JAN$Date <- as.numeric(OR_PDSI_JAN$Date)
OR_PDSI_JAN <- OR_PDSI_JAN[ , 1:2]
colnames(OR_PDSI_JAN) <- c("Date", "PDSI")
OR_PDSI_JAN$Watershed <- "Oregon"

OR_PDSI_OCT <- readr::read_csv("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Data/PDSI/October/OR-pdsi-OCT-1895-2022.csv", skip = 3)
OR_PDSI_OCT$Date <- gsub('.{2}$', '', OR_PDSI_OCT$Date)
OR_PDSI_OCT$Date <- as.numeric(OR_PDSI_OCT$Date)
OR_PDSI_OCT <- OR_PDSI_OCT[ , 1:2]
colnames(OR_PDSI_OCT) <- c("Date", "PDSI")
OR_PDSI_OCT$Watershed <- "Oregon"


## CREATE weights for watershed areas
CA_weight <- 6.7767e+06 / 1.08016e+07
OR_weight <- 4.0249e+06 / 1.08016e+07
MI_weight <- 7.36628e+06 / 8.676440e+06
IN_weight <- 1.31016e+06 / 8.676440e+06

## CALCULATE weighted average PDSI for watershed areas in multiple states
Klamath_PDSI_Jan <- data.frame(CA_PDSI_JAN$Date, round(((CA_PDSI_JAN$PDSI * CA_weight) + (OR_PDSI_JAN$PDSI * OR_weight)), 2))
colnames(Klamath_PDSI_Jan) <- c("Date", "PDSI")
Klamath_PDSI_Jan$Watershed <- "Klamath Jan"

Klamath_PDSI_Oct <- data.frame(CA_PDSI_OCT$Date, round(((CA_PDSI_OCT$PDSI * CA_weight) + (OR_PDSI_OCT$PDSI * OR_weight)), 2))
colnames(Klamath_PDSI_Oct) <- c("Date", "PDSI")
Klamath_PDSI_Oct$Watershed <- "Klamath Oct"

SELakeMI_PDSI_Jan <- data.frame(MI_PDSI_JAN$Date, round(((MI_PDSI_JAN$PDSI * MI_weight) + (IN_PDSI_JAN$PDSI * IN_weight)), 2))
colnames(SELakeMI_PDSI_Jan) <- c("Date", "PDSI")
SELakeMI_PDSI_Jan$Watershed <- "SE Lake Michigan Jan"

SELakeMI_PDSI_Oct <- data.frame(MI_PDSI_OCT$Date, round(((MI_PDSI_OCT$PDSI * MI_weight) + (IN_PDSI_OCT$PDSI * IN_weight)), 2))
colnames(SELakeMI_PDSI_Oct) <- c("Date", "PDSI")
SELakeMI_PDSI_Oct$Watershed <- "SE Lake Michigan Oct"

MidArkansas_PDSI_Jan <- KS_PDSI_JAN[ , 1:2]
colnames(MidArkansas_PDSI_Jan) <- c("Date", "PDSI")
MidArkansas_PDSI_Jan$Watershed <- "Middle Arkansas Jan"

MidArkansas_PDSI_Oct <- KS_PDSI_OCT[ , 1:2]
colnames(MidArkansas_PDSI_Oct) <- c("Date", "PDSI")
MidArkansas_PDSI_Oct$Watershed <- "Middle Arkansas Oct"

## COMBINE watershed PDSI for plotting
PDSI_Jan <- data.frame()
PDSI_Jan <- rbind.data.frame(Klamath_PDSI_Jan, SELakeMI_PDSI_Jan, MidArkansas_PDSI_Jan)

PDSI_Oct <- data.frame()
PDSI_Oct <- rbind.data.frame(Klamath_PDSI_Oct, SELakeMI_PDSI_Oct, MidArkansas_PDSI_Oct)

PDSI_Jan_Klamath <- data.frame()
PDSI_Jan_Klamath <- rbind.data.frame(Klamath_PDSI_Jan, CA_PDSI_JAN, OR_PDSI_JAN)
  levels(PDSI_Jan_Klamath$Watershed) <- c("California", "Klamath", "Oregon")

PDSI_Oct_Klamath <- data.frame()  
PDSI_Oct_Klamath <- rbind.data.frame(Klamath_PDSI_Oct, CA_PDSI_OCT, OR_PDSI_OCT)
  levels(PDSI_Jan_Klamath$Watershed) <- c("California", "Klamath WS", "Oregon")

PDSI_Jan_SELakeMI <- data.frame()
PDSI_Jan_SELakeMI <- rbind.data.frame(MI_PDSI_JAN, SELakeMI_PDSI_Jan, IN_PDSI_JAN)
  levels(PDSI_Jan_SELakeMI$Watershed) <- c("Michigan", "SE Lake Michigan", "Indiana")
  
PDSI_Oct_SELakeMI <- data.frame()
PDSI_Oct_SELakeMI <- rbind.data.frame(MI_PDSI_OCT, SELakeMI_PDSI_Oct, IN_PDSI_OCT)
  levels(PDSI_Oct_SELakeMI$Watershed) <- c("Michigan", "SE Lake Michigan", "Indiana")
  

## PLOT PDSI for all watersheds
ggplot2::ggplot(PDSI_Jan, aes(x = Date, y = PDSI, fill = Watershed)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("plum4", "tan3", "springgreen4")) +
  facet_grid(Watershed ~ .) +
  geom_vline(xintercept = 1950, color = "black") +
  geom_vline(xintercept = 1960, color = "black") +
  geom_vline(xintercept = 1970, color = "black") +
  scale_x_continuous(limits = c(1945, 1975), breaks = seq(1945, 1975, 5), minor_breaks = seq(1945, 1975, 1)) +
  theme(panel.grid.major.x = element_line(size = 0.75), panel.grid.minor = element_line(size = 0.1)) +
  ylim(min = -8, max = 8)

  

## PLOT averaged PDSI for Klamath and St. Joseph watersheds with contributing states PDSI
ggplot2::ggplot(PDSI_Jan_Klamath, aes(x = Date, y = PDSI, fill = Watershed)) +
  geom_col(width = 0.75, position = "dodge") +
  scale_fill_manual(values = c("blue", "plum3", "black")) +
  # facet_grid(Watershed ~ .) +
  scale_x_continuous(limits = c(1945, 1975), breaks = seq(1945, 1975, 5), minor_breaks = seq(1945, 1975, 1)) +
  scale_y_continuous(limits = c(-8, 8), breaks = seq(-8, 8, 2)) +
  theme(panel.grid.major.x = element_line(size = 0.75), panel.grid.minor = element_line(size = 0.1)) 
  


ggplot2::ggplot(PDSI_Oct_SELakeMI, aes(x = Date, y = PDSI, fill = Watershed)) +
  geom_col(width = 0.75, position = "dodge") +
  scale_fill_manual(values = c("black", "blue", "springgreen3")) +
  # facet_grid(Watershed ~ .) +
  scale_x_continuous(limits = c(1945, 1975), breaks = seq(1945, 1975, 5), minor_breaks = seq(1945, 1975, 1)) +
  scale_y_continuous(limits = c(-8, 8), breaks = seq(-8, 8, 2)) +
  theme(panel.grid.major.x = element_line(size = 0.75), panel.grid.minor = element_line(size = 0.1)) +
  theme(panel.grid.major.x = element_line(size = 0.75), panel.grid.minor = element_line(size = 0.1)) 

