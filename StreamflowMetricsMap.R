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
StJoseph_WS_boundary <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/MI/040500MI.geojson")
    ## convert spatial object to a ggplot ready data frame
    StJoseph_WS_boundary_df <- tidy(StJoseph_WS_boundary)
    ## make sure the shapefile attribute table has an id column
    StJoseph_WS_boundary$id <- rownames(StJoseph_WS_boundary@data)
    ## join the attribute table from the spatial object to the new data frame
    StJoseph_WS_boundary_df <- left_join(StJoseph_WS_boundary_df, StJoseph_WS_boundary@data, by = "id")


## IMPORT Middle Arkansas River watershed boundary
MidArkRiver_WS_boundary <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/KS/110300KS.geojson")
    ## convert spatial object to a ggplot ready data frame
    MidArkRiver_WS_boundary_df <- tidy(MidArkRiver_WS_boundary)
    ## make sure the shapefile attribute table has an id column
    MidArkRiver_WS_boundary$id <- rownames(MidArkRiver_WS_boundary@data)
    ## join the attribute table from the spatial object to the new data frame
    MidArkRiver_WS_boundary_df <- left_join(MidArkRiver_WS_boundary_df, MidArkRiver_WS_boundary@data, by = "id")

## IMPORT Klamath watershed boundary
Klamath_WS_boundary <- readOGR("C:/Users/misty/OneDrive - The University of Kansas/Documents/Powell/Shapefiles/CA/180102CA.geojson")
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
  # geom_polygon(data = StJoseph_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  # geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5 ) +
  # geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5 ) +
  # geom_point(data = huc040500MI_ws, aes(x = dec_long_va, y = dec_lat_va), fill = "plum4") +
  # geom_point(data = huc110300KS_ws, aes(x = dec_long_va, y = dec_lat_va), fill = "tan4") +
  # geom_point(data = huc180102CA_ws, aes(x = dec_long_va, y = dec_lat_va), fill = "springgreen4")

MI_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = StJoseph_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "plum2", alpha = 0.5) +
  geom_point(data = huc040500MI_ws, aes(x = dec_long_va, y = dec_lat_va), color = "plum4", size = 3) +
  geom_text_repel(data = huc040500MI_ws, aes(x = dec_long_va, y = dec_lat_va, label = row.names(huc040500MI_ws)), size = 4) +
  xlim(-87, -83.75) + ylim(41.25, 43.5)
MI_map

KS_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = MidArkRiver_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "tan2", alpha = 0.5) +
  geom_point(data = huc110300KS_ws, aes(x = dec_long_va, y = dec_lat_va), color = "tan4", size = 3) +
  geom_text_repel(data = huc110300KS_ws, aes(x = dec_long_va, y = dec_lat_va, label = row.names(huc110300KS_ws)), size = 4) 
  # + xlim(-103, -96.5) + ylim(36, 39.5)
KS_map

CA_map <- ggplot() +
  # geom_path(data = us_rivers_df, aes(x = long, y = lat, group = group), color = "royalblue2") +
  # geom_path(data = us_states_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = Klamath_WS_boundary_df, aes(x = long, y = lat, group = group), fill = "springgreen2", alpha = 0.5) +
  geom_point(data = huc180102CA_ws, aes(x = dec_long_va, y = dec_lat_va), color = "springgreen4", size = 3) +
  geom_text_repel(data = huc180102CA_ws, aes(x = dec_long_va, y = dec_lat_va, label = row.names(huc180102CA_ws)), size = 4) 
# + xlim(-103, -96.5) + ylim(36, 39.5)

CA_map

