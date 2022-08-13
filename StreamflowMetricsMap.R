# load libraries
library(raster)
library(rgdal)
library(ggplot2)
library(broom)
library(RColorBrewer)
library(rgeos)
library(dplyr)
library(ggrepel)
# note that you don't need to call maptools to run the code below but it needs to be installed.
library(maptools)
# to add a north arrow and a scale bar to the map
library(ggsn)


# set factors to false
options(stringsAsFactors = FALSE)


### IMPORT SHAPEFILES ###
## IMPORT US state boundaries
us_states <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/cb_2018_us_state_contiguous.shp")
    ## convert spatial object to a ggplot ready data frame
    us_states_df <- tidy(us_states)
    ## make sure the shapefile attribute table has an id column
    us_states$id <- rownames(us_states@data)
    ## join the attribute table from the spatial object to the new data frame
    us_states_df <- left_join(us_states_df, us_states@data, by = "id")

## IMPORT MI state boundaries
    MI_stateBoundary <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/MI/MI_StateBoundaries.geojson")
    ## convert spatial object to a ggplot ready data frame
    MI_stateBoundary_df <- tidy(MI_stateBoundary)
    ## make sure the shapefile attribute table has an id column
    MI_stateBoundary$id <- rownames(MI_stateBoundary@data)
    ## join the attribute table from the spatial object to the new data frame
    MI_stateBoundary_df <- left_join(MI_stateBoundary_df, MI_stateBoundary@data, by = "id")
    
## IMPORT KS state boundaries
    KS_stateBoundary <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/KS/KS_StateBoundaries.geojson")
    ## convert spatial object to a ggplot ready data frame
    KS_stateBoundary_df <- tidy(KS_stateBoundary)
    ## make sure the shapefile attribute table has an id column
    KS_stateBoundary$id <- rownames(KS_stateBoundary@data)
    ## join the attribute table from the spatial object to the new data frame
    KS_stateBoundary_df <- left_join(KS_stateBoundary_df, KS_stateBoundary@data, by = "id")

## IMPORT CA state boundaries
    CA_stateBoundary <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/CA/CA_StateBoundaries.geojson")
    ## convert spatial object to a ggplot ready data frame
    CA_stateBoundary_df <- tidy(CA_stateBoundary)
    ## make sure the shapefile attribute table has an id column
    CA_stateBoundary$id <- rownames(CA_stateBoundary@data)
    ## join the attribute table from the spatial object to the new data frame
    CA_stateBoundary_df <- left_join(CA_stateBoundary_df, CA_stateBoundary@data, by = "id")
    
## IMPORT US rivers
us_rivers <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/Lakes_and_Rivers_Shapefile/NA_Lakes_and_Rivers/data/hydrography_l_rivers_v2_contiguous.shp")
    ## convert spatial object to a ggplot ready data frame
    us_rivers_df <- tidy(us_rivers)
    ## make sure the shapefile attribute table has an id column
    us_rivers$id <- rownames(us_rivers@data)
    ## join the attribute table from the spatial object to the new data frame
    us_rivers_df <- left_join(us_rivers_df, us_rivers@data, by = "id")
    
## IMPORT US rivers clipped to MI view - ArcGIS source
    MI_rivers <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/MI/MI_Rivers_and_Streams.geojson")
    ## convert spatial object to a ggplot ready data frame
    MI_rivers_df <- tidy(MI_rivers)
    ## make sure the shapefile attribute table has an id column
    MI_rivers$id <- rownames(MI_rivers@data)
    ## join the attribute table from the spatial object to the new data frame
    MI_rivers_df <- left_join(MI_rivers_df, MI_rivers@data, by = "id")
    
## IMPORT US rivers clipped to KS view - ArcGIS source
    KS_rivers <- readOGR("C:/Users/Misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/KS/KS_Rivers_and_Streams.geojson")
    ## convert spatial object to a ggplot ready data frame
    KS_rivers_df <- tidy(KS_rivers)
    ## make sure the shapefile attribute table has an id column
    KS_rivers$id <- rownames(KS_rivers@data)
    ## join the attribute table from the spatial object to the new data frame
    KS_rivers_df <- left_join(KS_rivers_df, KS_rivers@data, by = "id")
    
## IMPORT US rivers clipped to CA view - ArcGIS source
    CA_rivers <- readOGR("C:/Users/Misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/CA/CA_Rivers_and_Streams.geojson")
    ## convert spatial object to a ggplot ready data frame
    CA_rivers_df <- tidy(CA_rivers)
    ## make sure the shapefile attribute table has an id column
    CA_rivers$id <- rownames(CA_rivers@data)
    ## join the attribute table from the spatial object to the new data frame
    CA_rivers_df <- left_join(CA_rivers_df, CA_rivers@data, by = "id")

## IMPORT St. Joseph watershed boundary
SELakeMI_WS_boundary <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/MI/040500MI_WSboundary.geojson")
    ## convert spatial object to a ggplot ready data frame
    SELakeMI_WS_boundary_df <- tidy(SELakeMI_WS_boundary)
    ## make sure the shapefile attribute table has an id column
    SELakeMI_WS_boundary$id <- rownames(SELakeMI_WS_boundary@data)
    ## join the attribute table from the spatial object to the new data frame
    SELakeMI_WS_boundary_df <- left_join(SELakeMI_WS_boundary_df, SELakeMI_WS_boundary@data, by = "id")


## IMPORT Middle Arkansas River watershed boundary
MidArkRiver_WS_boundary <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/KS/110300KS_WSboundary.geojson")
    ## convert spatial object to a ggplot ready data frame
    MidArkRiver_WS_boundary_df <- tidy(MidArkRiver_WS_boundary)
    ## make sure the shapefile attribute table has an id column
    MidArkRiver_WS_boundary$id <- rownames(MidArkRiver_WS_boundary@data)
    ## join the attribute table from the spatial object to the new data frame
    MidArkRiver_WS_boundary_df <- left_join(MidArkRiver_WS_boundary_df, MidArkRiver_WS_boundary@data, by = "id")

## IMPORT Klamath watershed boundary
Klamath_WS_boundary <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/CA/180102CA_WSboundary.geojson")
    ## convert spatial object to a ggplot ready data frame
    Klamath_WS_boundary_df <- tidy(Klamath_WS_boundary)
    ## make sure the shapefile attribute table has an id column
    Klamath_WS_boundary$id <- rownames(Klamath_WS_boundary@data)
    ## join the attribute table from the spatial object to the new data frame
    Klamath_WS_boundary_df <- left_join(Klamath_WS_boundary_df, Klamath_WS_boundary@data, by = "id")

### FUNCTIONS FOR PLOTTING ###   
    id.number <- function(n){
      # if(n == 1){return(ws_040500MI[n, ])}
      if(n == 1){return(ws_110300KS[n, ])}
      # if(n == 1){return(ws_180102CA[n, ])}
    }



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
  
  
    
### WATERSHED SITE MAP ###  
ggplot() +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) +
  geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  # geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5 ) +
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), color = "black", fill = "white", alpha = 0) +
  # geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5 ) +
  # geom_point(data = ws_040500MI, aes(x = dec_long_va, y = dec_lat_va), fill = "plum4") +
  geom_point(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va), color = "black", fill = "white", size = 4) 
  # geom_point(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va), fill = "springgreen4")


### ISOLATE GAGE STATION MAP ###

ggplot() +
  geom_path(data = MI_stateBoundary_df, aes(x = long, y = lat, group = group), size = 1) +
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_path(data = MI_rivers_df, aes(x = long, y = lat, group = group, color = ), color = "royalblue2") +
  geom_point(data = ws_040500MI, aes(x = dec_long_va, y = dec_lat_va), color = "white",  size = 4) +
  geom_point(data = sapply(ws_040500MI$id, id.number)[[1]], aes(x = dec_long_va, y = dec_lat_va), color = "black", fill = "black", size = 7) 
  

ggplot() +
  geom_path(data = KS_stateBoundary_df, aes(x = long, y = lat, group = group), size = 1) +
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "white", color = "black", alpha = 0, size = 1.5) +
  geom_path(data = KS_rivers_df, aes(x = long, y = lat, group = group, alpha = Feature), color = "royalblue2") +
  scale_alpha_manual(values = c(0, 0, 0, 1, 0)) +
  geom_point(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va), fill = "white", color = "black", shape = 21 , size = 4) +
  geom_point(data = sapply(ws_110300KS$id, id.number)[[1]], aes(x = dec_long_va, y = dec_lat_va), color = "black", fill = "black", size = 7) +
  theme(legend.position = "none")


ggplot() +
  geom_path(data = CA_stateBoundary_df, aes(x = long, y = lat, group = group), size = 1) +
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_path(data = CA_rivers_df, aes(x = long, y = lat, group = group, color = ), color = "royalblue2") +
  geom_point(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va), color = "white",  size = 4) +
  geom_point(data = sapply(ws_180102CA$id, id.number)[[1]], aes(x = dec_long_va, y = dec_lat_va), color = "black", fill = "black", size = 7)
  
  
### FUNCTIONS FOR DATA-DRIVEN FORMATTING ###
color.picker <- function(t){
  if(t == "+"){return("darkblue")}
  else {return("darkred")}
}

color.picker2 <- function(t, p){
  if(t == "+" && p <= 0.05){return("darkblue")}
  else if (t == "+" && p > 0.05){return("steelblue1")}
  else if (t == "-" && p <= 0.05){return("darkred")}
  else {return("lightcoral")}
}

shape.picker <- function(z){
  if(z == "+"){return(24)}
  else {return(25)}
}

alpha.picker <- function(z){
  if(z <= 0.05){return(0.1)}
  else {return(1)}
}


### PLOT MK test for MAM7 ###
MI_mk_MAM7_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = ws_040500MI,aes(x = dec_long_va, y = dec_lat_va, shape = tau_MAM7, fill = tau_MAM7, alpha = p_MAM7), size = 3) +
  geom_text_repel(data = ws_040500MI, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_040500MI$tau_MAM7, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_040500MI$tau_MAM7, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_040500MI$tau_MAM7, ws_040500MI$p_MAM7)) +
  # + xlim(-87, -83.75) + ylim(41.25, 43.5)
  ggtitle("M-K Test on Annual MAM7 for SE Lake Michigan Watershed 1901 - 2021") + 
  theme(legend.position="none")
MI_mk_MAM7_map

KS_mk_MAM7_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, shape = tau_MAM7, fill = tau_MAM7, alpha = p_MAM7), size = 3) +
  geom_text_repel(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_110300KS$tau_MAM7, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_110300KS$tau_MAM7, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_110300KS$tau_MAM7, ws_110300KS$p_MAM7)) +
  xlim(-103, -96.5) + ylim(36, 39.5) +
  ggtitle("M-K Test on Annual MAM7 for Middle Arkansas Watershed 1902 - 2021") +
  theme(legend.position="none")
KS_mk_MAM7_map

CA_mk_MAM7_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # (data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, shape = tau_MAM7, fill = tau_MAM7, alpha = p_MAM7), size = 3) +
  geom_text_repel(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_180102CA$tau_MAM7, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_180102CA$tau_MAM7, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_180102CA$tau_MAM7, ws_180102CA$p_MAM7)) +
  # + xlim(-103, -96.5) + ylim(36, 39.5)
  ggtitle("M-K Test on Annual MAM7 for Klamath Watershed 1904 - 2021") +
  theme(legend.position="none")
CA_mk_MAM7_map


##### PLOT MK test for Mean Annual Baseflow #####
MI_mk_Baseflow_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = ws_040500MI,aes(x = dec_long_va, y = dec_lat_va, shape = tau_Baseflow, fill = tau_Baseflow, alpha = p_Baseflow), size = 3) +
  geom_text_repel(data = ws_040500MI, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_040500MI$tau_Baseflow, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_040500MI$tau_Baseflow, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_040500MI$tau_Baseflow, ws_040500MI$p_Baseflow)) +
  # + xlim(-87, -83.75) + ylim(41.25, 43.5)
  ggtitle("M-K Test on Annual Mean Baseflow for SE Lake Michigan Watershed 1901 - 2021") + 
  theme(legend.position="none")
MI_mk_Baseflow_map

KS_mk_Baseflow_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Baseflow, fill = tau_Baseflow, alpha = p_Baseflow), size = 3) +
  geom_text_repel(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_110300KS$tau_Baseflow, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_110300KS$tau_Baseflow, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_110300KS$tau_Baseflow, ws_110300KS$p_Baseflow)) +
  xlim(-103, -96.5) + ylim(36, 39.5) +
  ggtitle("M-K Test on Annual Mean Baseflow for Middle Arkansas Watershed 1902 - 2021") +
  theme(legend.position="none")
KS_mk_Baseflow_map

CA_mk_Baseflow_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # (data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Baseflow, fill = tau_Baseflow, alpha = p_Baseflow), size = 3) +
  geom_text_repel(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_180102CA$tau_Baseflow, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0)) +
  scale_fill_manual(values = sapply(ws_180102CA$tau_Baseflow, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_180102CA$tau_Baseflow, ws_180102CA$p_Baseflow)) +
  # + xlim(-103, -96.5) + ylim(36, 39.5)
  ggtitle("M-K Test on Annual Mean Baseflow for Klamath Watershed 1904 - 2021") +
  theme(legend.position="none")
CA_mk_Baseflow_map


##### PLOT MK test for Mean Annual Discharge (MeanQ) #####
MI_mk_MeanQ_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = ws_040500MI,aes(x = dec_long_va, y = dec_lat_va, shape = tau_MeanQ, fill = tau_MeanQ, alpha = p_MeanQ), size = 3) +
  geom_text_repel(data = ws_040500MI, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_040500MI$tau_MeanQ, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_040500MI$tau_MeanQ, color.picker)) +
# scale_fill_manual(values = mapply(FUN = color.picker2, ws_040500MI$tau_MeanQ, ws_040500MI$p_MeanQ)) +
  # + xlim(-87, -83.75) + ylim(41.25, 43.5)
  ggtitle("M-K Test on Annual Mean Discharge for SE Lake Michigan Watershed 1901 - 2021") + 
  theme(legend.position="none")
MI_mk_MeanQ_map

KS_mk_MeanQ_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, shape = tau_MeanQ, fill = tau_MeanQ, alpha = p_MeanQ), size = 3) +
  geom_text_repel(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_110300KS$tau_MeanQ, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_110300KS$tau_MeanQ, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_110300KS$tau_MeanQ, ws_110300KS$p_MeanQ)) +
  xlim(-103, -96.5) + ylim(36, 39.5) +
  ggtitle("M-K Test on Annual Mean Discharge for Middle Arkansas Watershed 1902 - 2021") +
  theme(legend.position="none")
KS_mk_MeanQ_map

CA_mk_MeanQ_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # (data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, shape = tau_MeanQ, fill = tau_MeanQ, alpha = p_MeanQ), size = 3) +
  geom_text_repel(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_180102CA$tau_MeanQ, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_180102CA$tau_MeanQ, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_180102CA$tau_MeanQ, ws_180102CA$p_MeanQ)) +
  # + xlim(-103, -96.5) + ylim(36, 39.5)
  ggtitle("M-K Test on Annual Mean Discharge for Klamath Watershed 1904 - 2021") +
  theme(legend.position="none")
CA_mk_MeanQ_map


##### PLOT MK test for Q10 #####
MI_mk_Q10_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = ws_040500MI,aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q10, fill = tau_Q10, alpha = p_Q10), size = 3) +
  geom_text_repel(data = ws_040500MI, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_040500MI$tau_Q10, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_040500MI$tau_Q10, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_180102CA$tau_Q10, ws_180102CA$p_Q10)) +
  # + xlim(-87, -83.75) + ylim(41.25, 43.5)
  ggtitle("M-K Test on Annual Q10 for SE Lake Michigan Watershed 1901 - 2021") + 
  theme(legend.position="none")
MI_mk_Q10_map

KS_mk_Q10_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q10, fill = tau_Q10, alpha = p_Q10), size = 3) +
  geom_text_repel(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_110300KS$tau_Q10, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_110300KS$tau_Q10, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_110300KS$tau_Q10, ws_110300KS$p_Q10)) +
  xlim(-103, -96.5) + ylim(36, 39.5) +
  ggtitle("M-K Test on Annual Q10 for Middle Arkansas Watershed 1902 - 2021") +
  theme(legend.position="none")
KS_mk_Q10_map

CA_mk_Q10_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # (data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q10, fill = tau_Q10, alpha = p_Q10), size = 3) +
  geom_text_repel(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_180102CA$tau_Q10, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_180102CA$tau_Q10, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_180102CA$tau_Q10, ws_180102CA$p_Q10)) +
  # + xlim(-103, -96.5) + ylim(36, 39.5)
  ggtitle("M-K Test on Annual Q10 for Klamath Watershed 1904 - 2021") +
  theme(legend.position="none")
CA_mk_Q10_map


##### PLOT MK test for Q50 #####
MI_mk_Q50_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = ws_040500MI,aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q50, fill = tau_Q50, alpha = p_Q50), size = 3) +
  geom_text_repel(data = ws_040500MI, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_040500MI$tau_Q50, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_040500MI$tau_Q50, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_040500MI$tau_Q50, ws_040500MI$p_Q50)) +
  # + xlim(-87, -83.75) + ylim(41.25, 43.5)
  ggtitle("M-K Test on Annual Q50 for SE Lake Michigan Watershed 1901 - 2021") + 
  theme(legend.position="none")
MI_mk_Q50_map

KS_mk_Q50_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q50, fill = tau_Q50, alpha = p_Q50), size = 3) +
  geom_text_repel(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_110300KS$tau_Q50, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_110300KS$tau_Q50, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_110300KS$tau_Q50, ws_110300KS$p_Q50)) +
  xlim(-103, -96.5) + ylim(36, 39.5) +
  ggtitle("M-K Test on Annual Q50 for Middle Arkansas Watershed 1902 - 2021") +
  theme(legend.position="none")
KS_mk_Q50_map

CA_mk_Q50_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # (data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q50, fill = tau_Q50, alpha = p_Q50), size = 3) +
  geom_text_repel(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_180102CA$tau_Q50, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_180102CA$tau_Q50, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_180102CA$tau_Q50, ws_180102CA$p_Q50)) +
  # + xlim(-103, -96.5) + ylim(36, 39.5)
  ggtitle("M-K Test on Annual Q50 for Klamath Watershed 1904 - 2021") +
  theme(legend.position="none")
CA_mk_Q50_map


##### PLOT MK test for Q90 #####
MI_mk_Q90_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = ws_040500MI,aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q90, fill = tau_Q90, alpha = p_Q90), size = 3) +
  geom_text_repel(data = ws_040500MI, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_040500MI$tau_Q90, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_040500MI$tau_Q90, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_040500MI$tau_Q90, ws_040500MI$Q90)) +
  # + xlim(-87, -83.75) + ylim(41.25, 43.5)
  ggtitle("M-K Test on Annual Q90 for SE Lake Michigan Watershed 1901 - 2021") + 
  theme(legend.position="none")
MI_mk_Q90_map

KS_mk_Q90_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q90, fill = tau_Q90, alpha = p_Q90), size = 3) +
  geom_text_repel(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_110300KS$tau_Q90, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_110300KS$tau_Q90, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_110300KS$tau_Q90, ws_110300KS$p_Q90)) +
  xlim(-103, -96.5) + ylim(36, 39.5) +
  ggtitle("M-K Test on Annual Q90 for Middle Arkansas Watershed 1902 - 2021") +
  theme(legend.position="none")
KS_mk_Q90_map

CA_mk_Q90_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # (data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q90, fill = tau_Q90, alpha = p_Q90), size = 3) +
  geom_text_repel(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 3) +
  scale_shape_manual(values = sapply(ws_180102CA$tau_Q90, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_180102CA$tau_Q90, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_180102CA$tau_Q90, ws_180102CA$p_Q90)) +
  # + xlim(-103, -96.5) + ylim(36, 39.5)
  ggtitle("M-K Test on Annual Q90 for Klamath Watershed 1904 - 2021") +
  theme(legend.position="none")
CA_mk_Q90_map


##### PLOT MK test for Q95 #####
MI_mk_Q95_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = ws_040500MI,aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q95, fill = tau_Q95, alpha = p_Q95), size = 3) +
  geom_text_repel(data = ws_040500MI, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size =4) +
  scale_shape_manual(values = sapply(ws_040500MI$tau_Q95, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_040500MI$tau_Q95, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_040500MI$tau_Q95, ws_040500MI$p_Q95)) +
  # + xlim(-87, -83.75) + ylim(41.25, 43.5)
  ggtitle("M-K Test on Annual Q95 for SE Lake Michigan Watershed 1901 - 2021") + 
  theme(legend.position="none")
MI_mk_Q95_map

KS_mk_Q95_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q95, fill = tau_Q95, alpha = p_Q95), size = 3) +
  geom_text_repel(data = ws_110300KS, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 4) +
  scale_shape_manual(values = sapply(ws_110300KS$tau_Q95, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_110300KS$tau_Q95, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_110300KS$tau_Q95, ws_110300KS$p_Q95)) +
  xlim(-103, -96.5) + ylim(36, 39.5) +
  ggtitle("M-K Test on Annual Q95 for Middle Arkansas Watershed 1902 - 2021") +
  theme(legend.position="none")
KS_mk_Q95_map

CA_mk_Q95_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # (data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Q95, fill = tau_Q95, alpha = p_Q95), size = 3) +
  geom_text_repel(data = ws_180102CA, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 4) +
  scale_shape_manual(values = sapply(ws_180102CA$tau_Q95, shape.picker), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(1, 0.1)) +
  scale_fill_manual(values = sapply(ws_180102CA$tau_Q95, color.picker)) +
  # scale_fill_manual(values = mapply(FUN = color.picker2, ws_180102CA$tau_Q95, ws_180102CA$p_Q95)) +
  # + xlim(-103, -96.5) + ylim(36, 39.5)
  ggtitle("M-K Test on Annual Q95 for Klamath Watershed 1904 - 2021") +
  theme(legend.position="none")
CA_mk_Q95_map




