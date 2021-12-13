
library(tidyverse)
library(sf)
library(tmap)
library(stringr)
library(sp)
library(rgdal)
library(readxl)
library(shinyjs)
library(RColorBrewer)
library(rFIA)
library(daymetr)
library(lubridate)
library(purrr)

# Read in FIA data by state
ne <- read_excel("NE tree spp of interest.xlsx")
wi <- read_excel("WI tree spp of interest.xlsx")
mo <- read_excel("MO tree spp of interest.xlsx")
sd <- read_excel("SD tree spp of interest.xlsx")
oh <- read_excel("OH tree spp of interest.xlsx")
nd <- read_excel("ND tree spp of interest.xlsx")
mi <- read_excel("MI tree spp of interest.xlsx")
ks <- read_excel("KS tree spp of interest.xlsx")
ind <- read_excel("IN tree spp of interest.xlsx")
il <- read_excel("IL tree spp of interest.xlsx")
ia <- read_excel("IA tree spp of interest.xlsx")
mn <- read_excel("MN tree spp of interest.xlsx")

# Shapefile for county boundaries
counties <- sf::read_sf("./County Data/midwestCB.shp") %>% st_make_valid()

######################### Extract Plot-level Lat/Longs (Averaged for plot) for obtaining weather data

##### Nebraska
# Average Lat/Longs for each plot for climate data
ne_ll <- ne %>%
         group_by(PLOT) %>%
         dplyr::summarise(., plot_lat = mean(LAT),
                             plot_lon = mean(LON))
colnames(ne_ll) <- c("site", "lat", "lon")
#write.csv(ne_ll, "ne_latlong.csv", row.names=FALSE)

##### Wisconsin
# Average Lat/Longs for each plot for climate data
wi_ll <- wi %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(wi_ll) <- c("site", "lat", "lon")
#write.csv(wi_ll, "wi_latlong.csv", row.names=FALSE)

##### Missouri
# Average Lat/Longs for each plot for climate data
mo_ll <- mo %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(mo_ll) <- c("site", "lat", "lon")
#write.csv(mo_ll, "mo_latlong.csv", row.names=FALSE)

##### South Dakota
# Average Lat/Longs for each plot for climate data
sd_ll <- sd %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(sd_ll) <- c("site", "lat", "lon")
#write.csv(sd_ll, "sd_latlong.csv", row.names=FALSE)

##### Ohio
# Average Lat/Longs for each plot for climate data
oh_ll <- oh %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(oh_ll) <- c("site", "lat", "lon")
#write.csv(oh_ll, "oh_latlong.csv", row.names=FALSE)

##### North Dakota
# Average Lat/Longs for each plot for climate data
nd_ll <- nd %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(nd_ll) <- c("site", "lat", "lon")
#write.csv(nd_ll, "nd_latlong.csv", row.names=FALSE)

##### Michigan
# Average Lat/Longs for each plot for climate data
mi_ll <- mi %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(mi_ll) <- c("site", "lat", "lon")
#write.csv(mi_ll, "mi_latlong.csv", row.names=FALSE)

##### Kansas
# Average Lat/Longs for each plot for climate data
ks_ll <- ks %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(ks_ll) <- c("site", "lat", "lon")
#write.csv(ks_ll, "ks_latlong.csv", row.names=FALSE)

##### Indiana
# Average Lat/Longs for each plot for climate data
ind_ll <- ind %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(ind_ll) <- c("site", "lat", "lon")
#write.csv(ind_ll, "ind_latlong.csv", row.names=FALSE)

##### Illinois
# Average Lat/Longs for each plot for climate data
il_ll <- il %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(il_ll) <- c("site", "lat", "lon")
#write.csv(il_ll, "il_latlong.csv", row.names=FALSE)

##### Iowa
# Average Lat/Longs for each plot for climate data
ia_ll <- ia %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(ia_ll) <- c("site", "lat", "lon")
#write.csv(ia_ll, "ia_latlong.csv", row.names=FALSE)

##### Minnesota
# Average Lat/Longs for each plot for climate data
mn_ll <- mn %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(mn_ll) <- c("site", "lat", "lon")
#write.csv(mn_ll, "mn_latlong.csv", row.names=FALSE)

###########################################################
# Download Climate Data for each state from 1980-2020

### General function to extract climate data
get_daymet <- function(site, lat, lon){
  # Run download_daymet on each location
  temp_daymet <- download_daymet(site,
                                 lat,
                                 lon,
                                 start = 1980,
                                 end = 2020) %>% 
    # Extract df with environmental vars from list
    .$data %>% 
    # Convert to tibble
    as_tibble() %>% 
    # Only include the variables you want
    dplyr::select(., -dayl..s., -srad..W.m.2., -swe..kg.m.2.) %>% 
    # Give each its plot_id
    mutate(site_id = site) %>% 
    # Get date from day of the year using lubridate
    mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>% 
    # Add column with month
    mutate(month = month(date, label = TRUE, abbr = FALSE))
  return(temp_daymet)
}  


######### Obtain Midwestern State FIA Plot Climate Data from DayMet
## Variables: Total daily precipitation (mm), max daily temperature (C), min daily temperature (C), daily average water vapor pressure (Pa)

##### Nebraska
# ne_clim <- ne_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### Wisconsin
# wi_clim <- wi_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### Missouri
# mo_clim <- mo_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### South Dakota
# sd_clim <- sd_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### Ohio
# oh_clim <- oh_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### North Dakota
# nd_clim <- nd_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### Michigan
# mi_clim <- mi_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### Kansas
# ks_clim <- ks_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### Indiana
# in_clim <- ind_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### Illinois
# il_clim <- il_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### Iowa
# ia_clim <- ia_ll %>% purrr::pmap_dfr(.,get_daymet) 
# 
# ##### Minnesota
# mn_clim <- mn_ll %>% purrr::pmap_dfr(.,get_daymet) 

######### Calculate monthly average climate variables for Midwestern State FIA Plots from 1980-2020

##### Nebraska (no month variable)
ne_month <- ne_clim  %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### Wisconsin (no month variable)
wi_month <- wi_clim  %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### Missouri
mo_month <- mo_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### South Dakota
sd_month <- sd_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### Ohio
oh_month <- oh_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### North Dakota
nd_month <- nd_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### Michigan
mi_month <- mi_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### Kansas
ks_month <- ks_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### Indiana
in_month <- in_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### Illinois
il_month <- il_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### Iowa
ia_month <- ia_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

##### Minnesota
mn_month <- mn_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanVP = mean(meanVPY)) 

########## Merge Plot Lat/Longs with Monthly Climate Data
ne_cLL <- merge(ne_month, ne_ll, by.x = "site_id", by.y = "site")
wi_cLL <- merge(wi_month, wi_ll, by.x = "site_id", by.y = "site")
mo_cLL <- merge(mo_month, mo_ll, by.x = "site_id", by.y = "site")
sd_cLL <- merge(sd_month, sd_ll, by.x = "site_id", by.y = "site")
oh_cLL <- merge(oh_month, oh_ll, by.x = "site_id", by.y = "site")
nd_cLL <- merge(nd_month, nd_ll, by.x = "site_id", by.y = "site")
mi_cLL <- merge(mi_month, mi_ll, by.x = "site_id", by.y = "site")
ks_cLL <- merge(ks_month, ks_ll, by.x = "site_id", by.y = "site")
in_cLL <- merge(in_month, ind_ll, by.x = "site_id", by.y = "site")
il_cLL <- merge(il_month, il_ll, by.x = "site_id", by.y = "site")
ia_cLL <- merge(ia_month, ia_ll, by.x = "site_id", by.y = "site")
mn_cLL <- merge(mn_month, mn_ll, by.x = "site_id", by.y = "site")

######################################################################

########## Filter trees in each state to only include live trees

##### Nebraska
ne_tree <- filter(ne, STATUSCD==1)
##### Wisconsin
wi_tree <- filter(wi, STATUSCD==1)
##### Missouri
mo_tree <- filter(mo, STATUSCD==1)
##### South Dakota
sd_tree <- filter(sd, STATUSCD==1)
##### Ohio
oh_tree <- filter(oh, STATUSCD==1)
##### North Dakota
nd_tree <- filter(nd, STATUSCD==1)
##### Michigan
mi_tree <- filter(mi, STATUSCD==1)
##### Kansas
ks_tree <- filter(ks, STATUSCD==1)
##### Indiana
in_tree <- filter(ind, STATUSCD==1)
##### Illinois
il_tree <- filter(il, STATUSCD==1)
##### Iowa
ia_tree <- filter(ia, STATUSCD==1)
##### Minnesota
mn_tree <- filter(mn, STATUSCD==1)

########## Merge tree data with plot-level climate data

##### Nebraska
ne_tc <- merge(ne_tree, ne_cLL, by.x = "PLOT", by.y = "site_id")
##### Wisconsin
wi_tc <- merge(wi_tree, wi_cLL, by.x = "PLOT", by.y = "site_id")
##### Missouri
mo_tc <- merge(mo_tree, mo_cLL, by.x = "PLOT", by.y = "site_id")
##### South Dakota
sd_tc <- merge(sd_tree, sd_cLL, by.x = "PLOT", by.y = "site_id")
##### Ohio
oh_tc <- merge(oh_tree, oh_cLL, by.x = "PLOT", by.y = "site_id")
##### North Dakota
nd_tc <- merge(nd_tree, nd_cLL, by.x = "PLOT", by.y = "site_id")
##### Michigan
mi_tc <- merge(mi_tree, mi_cLL, by.x = "PLOT", by.y = "site_id")
##### Kansas
ks_tc <- merge(ks_tree, ks_cLL, by.x = "PLOT", by.y = "site_id")
##### Indiana
in_tc <- merge(in_tree, in_cLL, by.x = "PLOT", by.y = "site_id")
##### Illinois
il_tc <- merge(il_tree, il_cLL, by.x = "PLOT", by.y = "site_id")
##### Iowa
ia_tc <- merge(ia_tree, ia_cLL, by.x = "PLOT", by.y = "site_id")
##### Minnesota
mn_tc <- merge(mn_tree, mn_cLL, by.x = "PLOT", by.y = "site_id")

###### Species counts by state
table(ia_tc$SPCD)
table(il_tc$SPCD)
table(in_tc$SPCD)
table(ks_tc$SPCD)
table(mi_tc$SPCD)
table(mn_tc$SPCD)
table(mo_tc$SPCD)
table(nd_tc$SPCD)
table(ne_tc$SPCD)
table(oh_tc$SPCD)
table(sd_tc$SPCD)
table(wi_tc$SPCD)

######################################################################

########## Calculating min, max, and variation in climatic variables, by species

##### Nebraska

## Make species code a factor
ne_tc$SPCD <- as.factor(ne_tc$SPCD)

head(ne_tc)

## Does not retain the Site, Lat, and Long
ne_range <- ne_tc %>%
  group_by(SPCD) %>%
  mutate(., minTC = min(minTemp),
            maxTC = max(maxTemp),
                      minRain = min(cumPrecip),
                      maxRain = max(cumPrecip),
                      minAVP = min(meanVP),
                      maxAVP = max(meanVP))  %>%
    pivot_longer(., cols = minTC:maxAVP,
                    names_to = "envVarName",
                    values_to = "envVar") %>%
  group_by(SPCD, envVarName) %>%
      dplyr::summarise(., v = max(envVar))
  
glimpse(ne_range)  


d <- ne_tc %>% dplyr::select(SPCD, PLOT, lat, lon) %>% unique()
  
#   cols = new_sp_m014:newrel_f65,
# names_to = c("diagnosis", "gender", "age"),
# names_pattern = "new_?(.*)_(.)(.*)",
# values_to = "count"
  

View(ne_range)

glimpse(ne_range)


# Two step process to extract the min/max values
ne_r2 <- ne_tc %>%
        group_by(SPCD,PLOT,lat,lon) %>%
        dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP))
# Min temp
ne_minTC <- ne_r2 %>%
          group_by(SPCD) %>%
          filter(., minTC == min(minTC)) %>%
          dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP)

# Max temp
ne_maxTC <- ne_r2 %>%
          group_by(SPCD) %>%
          filter(.,maxTC == max(maxTC)) %>%
          dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP)
# Min rainfall
ne_minR <- ne_r2 %>%
        group_by(SPCD) %>%
        filter(.,minRain == min(minRain)) %>%
        dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP)
# Max rainfall
ne_maxR <- ne_r2 %>%
        group_by(SPCD) %>%
        filter(.,maxRain == max(maxRain)) %>%
        dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP)
# Min VP
ne_minV <- ne_r2 %>%
        group_by(SPCD) %>%
        filter(.,minAVP == min(minAVP)) %>%
        dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP)
# Min VP
ne_maxV <- ne_r2 %>%
        group_by(SPCD) %>%
        filter(.,maxAVP == max(maxAVP)) %>%
        dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP)

##### Wisconsin
wi_tc
##### Missouri
mo_tc
##### South Dakota
sd_tc
##### Ohio
oh_tc 
##### North Dakota
nd_tc 
##### Michigan
mi_tc
##### Kansas
ks_tc
##### Indiana
in_tc 
##### Illinois
il_tc 
##### Iowa
ia_tc 
##### Minnesota
mn_tc 


######################################################################

####### Mapping Midwestern FIA Plot Locations

## Add State to each dataset
ne_ll$state <- rep("NE",times = length(nrow(ne_ll)))
wi_ll$state <- rep("WI",times = length(nrow(wi_ll)))
mo_ll$state <- rep("MO",times = length(nrow(mo_ll)))
sd_ll$state <- rep("SD",times = length(nrow(sd_ll)))
oh_ll$state <- rep("OH",times = length(nrow(oh_ll)))
nd_ll$state <- rep("ND",times = length(nrow(nd_ll)))
mi_ll$state <- rep("MI",times = length(nrow(mi_ll)))
ks_ll$state <- rep("KS",times = length(nrow(ks_ll)))
ind_ll$state <- rep("IN",times = length(nrow(ind_ll)))
il_ll$state <- rep("IL",times = length(nrow(il_ll)))
ia_ll$state <- rep("IA",times = length(nrow(ia_ll)))
mn_ll$state <- rep("MN",times = length(nrow(mn_ll)))

## Midwest FIA Plot Locations
midwestFIA <- rbind(ne_ll, wi_ll, mo_ll, sd_ll,
                    oh_ll, nd_ll, mi_ll, ks_ll,
                    ind_ll, il_ll, ia_ll, mn_ll)
# Convert Lat/Longs to spatial features - NAD83 to match FIA procedure
dat_sf = st_as_sf(midwestFIA, coords = c("lon", "lat"), 
                  crs = 4259)

# Create a map of the plot locations by state, Counties
tm_shape(counties) +
  tm_polygons(col = "white") +
  tm_borders(col = "#919191") +
  tm_shape(dat_sf) + 
  tm_dots(col = "state")

# Resample soils/climate to be same resolution
# ?Resample - bilinear may be the best resolution

# Put all points on a 2 by 2 km resolution

# Create new columns for lats/longs
# Average by plot for lats/longs
# Use the lats/longs to extract 
# 2x2 km = 500 acres

# Day Met R
