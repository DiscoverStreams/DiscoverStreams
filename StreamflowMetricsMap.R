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

## IMPORT US rivers
us_rivers <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/Lakes_and_Rivers_Shapefile/NA_Lakes_and_Rivers/data/hydrography_l_rivers_v2_contiguous.shp")
    ## convert spatial object to a ggplot ready data frame
    us_rivers_df <- tidy(us_rivers)
    ## make sure the shapefile attribute table has an id column
    us_rivers$id <- rownames(us_rivers@data)
    ## join the attribute table from the spatial object to the new data frame
    us_rivers_df <- left_join(us_rivers_df, us_rivers@data, by = "id")

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

    
# # quick plot using base plot
# plot(us_states,
#      main = "United States (contiguous)")


### PLOT MAP ###

ggplot() +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) +
  geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") 
  # geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  # geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5 ) +
  # geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5 ) +
  # geom_point(data = huc040500MI_ws, aes(x = dec_long_va, y = dec_lat_va), fill = "plum4") +
  # geom_point(data = huc110300KS_ws, aes(x = dec_long_va, y = dec_lat_va), fill = "tan4") +
  # geom_point(data = huc180102CA_ws, aes(x = dec_long_va, y = dec_lat_va), fill = "springgreen4")


##### PLOT MK test for MAM7
MI_mk_MAM7_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = huc040500MI_ws,aes(x = dec_long_va, y = dec_lat_va, shape = tau_MAM7, fill = tau_MAM7, alpha = p_MAM7), size = 3) +
  geom_text_repel(data = huc040500MI_ws, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size =4) +
  scale_shape_manual(values = c(25, 24), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(0.1, 1)) +
  scale_fill_manual(values = c("darkred", "darkblue"), na.translate= FALSE) +
  # + xlim(-87, -83.75) + ylim(41.25, 43.5)
  ggtitle("M-K Test on Annual MAM7 for SE Lake Michigan Watershed 1961 - 2021") + 
  theme(legend.position="none")
MI_mk_MAM7_map

KS_mk_MAM7_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = huc110300KS_ws, aes(x = dec_long_va, y = dec_lat_va, shape = tau_MAM7, fill = tau_MAM7, alpha = p_MAM7), size = 3) +
  geom_text_repel(data = huc110300KS_ws, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 4) +
  scale_shape_manual(values = c(25, 24), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(0.1, 1)) +
  scale_fill_manual(values = c("darkred", "darkblue"), na.translate= FALSE) +
  xlim(-103, -96.5) + ylim(36, 39.5) +
  ggtitle("M-K Test on Annual MAM7 for Middle Arkansas Watershed 1962 - 2021") +
  theme(legend.position="none")
KS_mk_MAM7_map

CA_mk_MAM7_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # (data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = huc180102CA_ws, aes(x = dec_long_va, y = dec_lat_va, shape = tau_MAM7, fill = tau_MAM7, alpha = p_MAM7), size = 3) +
  geom_text_repel(data = huc180102CA_ws, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 4) +
  scale_shape_manual(values = c(25, 24), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(0.1, 1)) +
  scale_fill_manual(values = c("darkred", "darkblue"), na.translate= FALSE) +
# + xlim(-103, -96.5) + ylim(36, 39.5)
  ggtitle("M-K Test on Annual MAM7 for Klamath Watershed 1964 - 2021") +
  theme(legend.position="none")
CA_mk_MAM7_map


##### PLOT MK test for baseflow #####
MI_mk_Baseflow_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = SELakeMI_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = huc040500MI_ws,aes(x = dec_long_va, y = dec_lat_va, shape = tau_Baseflow, fill = tau_Baseflow, alpha = p_Baseflow), size = 3) +
  geom_text_repel(data = huc040500MI_ws, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size =4) +
  scale_shape_manual(values = c(25, 24), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(0.1, 1)) +
  scale_fill_manual(values = c("darkred", "darkblue"), na.translate= FALSE) +
  # + xlim(-87, -83.75) + ylim(41.25, 43.5)
  ggtitle("M-K Test on Annual Mean Baseflow for SE Lake Michigan Watershed 1961 - 2021") + 
  theme(legend.position="none")
MI_mk_Baseflow_map

KS_mk_Baseflow_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = huc110300KS_ws, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Baseflow, fill = tau_Baseflow, alpha = p_Baseflow), size = 3) +
  geom_text_repel(data = huc110300KS_ws, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 4) +
  scale_shape_manual(values = c(25, 24), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(0.1, 1)) +
  scale_fill_manual(values = c("darkred", "darkblue"), na.translate= FALSE) +
  xlim(-103, -96.5) + ylim(36, 39.5) +
  ggtitle("M-K Test on Annual Mean Baseflow for Middle Arkansas Watershed 1962 - 2021") +
  theme(legend.position="none")
KS_mk_Baseflow_map

CA_mk_Baseflow_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # (data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = huc180102CA_ws, aes(x = dec_long_va, y = dec_lat_va, shape = tau_Baseflow, fill = tau_Baseflow, alpha = p_Baseflow), size = 3) +
  geom_text_repel(data = huc180102CA_ws, aes(x = dec_long_va, y = dec_lat_va, label = station_nm), size = 4) +
  scale_shape_manual(values = c(25, 24), na.translate = TRUE, na.value = 1) +
  scale_alpha_binned(breaks = 0.05, range = c(0.1, 1)) +
  scale_fill_manual(values = c("darkred", "darkblue"), na.translate= FALSE) +
  # + xlim(-103, -96.5) + ylim(36, 39.5)
  ggtitle("M-K Test on Annual Mean Baseflow for Klamath Watershed 1964 - 2021") +
  theme(legend.position="none")
CA_mk_Baseflow_map
