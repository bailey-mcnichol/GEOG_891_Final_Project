
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

# Read in Common/Scientific Names for Each Spp
spp <- read_excel("sppNiobraraFIAMidwest.xlsx")

# Shapefile for county boundaries
counties <- sf::read_sf("./County Data/midwestCB.shp") %>% st_make_valid()

# Shapefile for state boundaries
state <- sf::read_sf("./County Data/midwestSB.shp") 

# Read in CSVs with Min/Max Environmental Variables by State, for each spp
maxSRstate <- read.csv("sppMaxSRstate.csv")
minSRstate <- read.csv("sppMinSRstate.csv")
maxVstate <- read.csv("sppMaxVstate.csv")
minVstate <- read.csv("sppMinVstate.csv")
maxRstate <- read.csv("sppMaxRstate.csv")
minRstate <- read.csv("sppMinRstate.csv")
maxTCstate <- read.csv("sppMaxTCstate.csv")
minTCstate <- read.csv("sppMinTCstate.csv")

###### Extract Plot-level Lat/Longs (Averaged for plot) for obtaining weather data

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
    dplyr::select(., -dayl..s., -swe..kg.m.2.) %>% 
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

#### Nebraska
#ne_clim <- ne_ll %>% purrr::pmap_dfr(.,get_daymet)

##### Wisconsin
#wi_clim <- wi_ll %>% purrr::pmap_dfr(.,get_daymet)

##### Missouri
#mo_clim <- mo_ll %>% purrr::pmap_dfr(.,get_daymet)

##### South Dakota
#sd_clim <- sd_ll %>% purrr::pmap_dfr(.,get_daymet)

##### Ohio
#oh_clim <- oh_ll %>% purrr::pmap_dfr(.,get_daymet)

##### North Dakota
#nd_clim <- nd_ll %>% purrr::pmap_dfr(.,get_daymet)

##### Michigan
#mi_clim <- mi_ll %>% purrr::pmap_dfr(.,get_daymet)

##### Kansas
#ks_clim <- ks_ll %>% purrr::pmap_dfr(.,get_daymet)

##### Indiana
#in_clim <- ind_ll %>% purrr::pmap_dfr(.,get_daymet)

##### Illinois
#il_clim <- il_ll %>% purrr::pmap_dfr(.,get_daymet)

##### Iowa
#ia_clim <- ia_ll %>% purrr::pmap_dfr(.,get_daymet)

##### Minnesota
#mn_clim <- mn_ll %>% purrr::pmap_dfr(.,get_daymet)

######### Calculate monthly average climate variables for Midwestern State FIA Plots from 1980-2020

##### Nebraska
ne_month <- ne_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(ne_month, "neMonthlyClim.csv")

##### Wisconsin (no month variable)
wi_month <- wi_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(wi_month, "wiMonthlyClim.csv")

##### Missouri
mo_month <- mo_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(mo_month, "moMonthlyClim.csv")

##### South Dakota
sd_month <- sd_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(sd_month, "sdMonthlyClim.csv") 

##### Ohio
oh_month <- oh_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(oh_month, "ohMonthlyClim.csv")

##### North Dakota
nd_month <- nd_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(nd_month, "ndMonthlyClim.csv") 

##### Michigan
mi_month <- mi_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(mi_month, "miMonthlyClim.csv")

##### Kansas
ks_month <- ks_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(ks_month, "ksMonthlyClim.csv")

##### Indiana
in_month <- in_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(in_month, "inMonthlyClim.csv")

##### Illinois
il_month <- il_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(il_month, "ilMonthlyClim.csv")

##### Iowa
ia_month <- ia_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(ia_month, "iaMonthlyClim.csv")

##### Minnesota
mn_month <- mn_clim  %>%
  group_by(site_id, year, month) %>%
  dplyr::summarise(., cumPrecipY = sum(prcp..mm.day.),
                   minTempY = mean(tmin..deg.c.),
                   maxTempY = mean(tmax..deg.c.),
                   meanSRadY = mean(srad..W.m.2.),
                   meanVPY = mean(vp..Pa.))  %>%
  group_by(site_id, month) %>%
  dplyr::summarise(., cumPrecip = mean(cumPrecipY),
                   minTemp = mean(minTempY),
                   maxTemp = mean(maxTempY),
                   meanSR = mean(meanSRadY),
                   meanVP = mean(meanVPY))
#write.csv(mn_month, "mnMonthlyClim.csv")

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
########## Calculating min, max, and var in climatic variables, by species

##### Nebraska
## Make species code a factor
ne_tc$SPCD <- as.factor(ne_tc$SPCD)
# Add state
ne_tc$State <- rep("NE",times = length(nrow(ne_tc)))
# Group by species/location and summarize to obtain extreme values
ne_r2 <- ne_tc %>%
        group_by(SPCD,State,PLOT,lat,lon) %>%
        dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
ne_minSR <- ne_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
ne_maxSR <- ne_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
ne_minTC <- ne_r2 %>%
    group_by(SPCD) %>%
    filter(., minTC == min(minTC)) %>%
    dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
ne_maxTC <- ne_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
ne_minR <- ne_r2 %>%
        group_by(SPCD) %>%
        filter(.,minRain == min(minRain)) %>%
        dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
ne_maxR <- ne_r2 %>%
        group_by(SPCD) %>%
        filter(.,maxRain == max(maxRain)) %>%
        dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
ne_minV <- ne_r2 %>%
        group_by(SPCD) %>%
        filter(.,minAVP == min(minAVP)) %>%
        dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
ne_maxV <- ne_r2 %>%
        group_by(SPCD) %>%
        filter(.,maxAVP == max(maxAVP)) %>%
        dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### Wisconsin
## Make species code a factor
wi_tc$SPCD <- as.factor(wi_tc$SPCD)
# Add state
wi_tc$State <- rep("WI",times = length(nrow(wi_tc)))
# Group by species/location and summarize to obtain extreme values
wi_r2 <- wi_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
wi_minSR <- wi_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
wi_maxSR <- wi_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
wi_minTC <- wi_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
wi_maxTC <- wi_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
wi_minR <- wi_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
wi_maxR <- wi_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
wi_minV <- wi_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
wi_maxV <- wi_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### Missouri
## Make species code a factor
mo_tc$SPCD <- as.factor(mo_tc$SPCD)
# Add state
mo_tc$State <- rep("MO",times = length(nrow(mo_tc)))
# Group by species/location and summarize to obtain extreme values
mo_r2 <- mo_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
mo_minSR <- mo_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
mo_maxSR <- mo_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
mo_minTC <- mo_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
mo_maxTC <- mo_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
mo_minR <- mo_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
mo_maxR <- mo_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
mo_minV <- mo_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
mo_maxV <- mo_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### South Dakota
## Make species code a factor
sd_tc$SPCD <- as.factor(sd_tc$SPCD)
# Add state
sd_tc$State <- rep("SD",times = length(nrow(sd_tc)))
# Group by species/location and summarize to obtain extreme values
sd_r2 <- sd_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
sd_minSR <- sd_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
sd_maxSR <- sd_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
sd_minTC <- sd_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
sd_maxTC <- sd_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
sd_minR <- sd_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
sd_maxR <- sd_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
sd_minV <- sd_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
sd_maxV <- sd_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### Ohio
## Make species code a factor
oh_tc$SPCD <- as.factor(oh_tc$SPCD)
# Add state
oh_tc$State <- rep("OH",times = length(nrow(oh_tc)))
# Group by species/location and summarize to obtain extreme values
oh_r2 <- oh_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
oh_minSR <- oh_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
oh_maxSR <- oh_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
oh_minTC <- oh_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
oh_maxTC <- oh_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
oh_minR <- oh_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
oh_maxR <- oh_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
oh_minV <- oh_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
oh_maxV <- oh_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### North Dakota
## Make species code a factor
nd_tc$SPCD <- as.factor(nd_tc$SPCD)
# Add state
nd_tc$State <- rep("ND",times = length(nrow(nd_tc)))
# Group by species/location and summarize to obtain extreme values
nd_r2 <- nd_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
nd_minSR <- nd_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
nd_maxSR <- nd_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
nd_minTC <- nd_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
nd_maxTC <- nd_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
nd_minR <- nd_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
nd_maxR <- nd_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
nd_minV <- nd_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
nd_maxV <- nd_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### Michigan

## Make species code a factor
mi_tc$SPCD <- as.factor(mi_tc$SPCD)
# Add state
mi_tc$State <- rep("MI",times = length(nrow(mi_tc)))
# Group by species/location and summarize to obtain extreme values
mi_r2 <- mi_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
mi_minSR <- mi_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
mi_maxSR <- mi_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
mi_minTC <- mi_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
mi_maxTC <- mi_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
mi_minR <- mi_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
mi_maxR <- mi_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
mi_minV <- mi_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
mi_maxV <- mi_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### Kansas

## Make species code a factor
ks_tc$SPCD <- as.factor(ks_tc$SPCD)
# Add state
ks_tc$State <- rep("KS",times = length(nrow(ks_tc)))
# Group by species/location and summarize to obtain extreme values
ks_r2 <- ks_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
ks_minSR <- ks_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
ks_maxSR <- ks_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
ks_minTC <- ks_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
ks_maxTC <- ks_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
ks_minR <- ks_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
ks_maxR <- ks_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
ks_minV <- ks_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
ks_maxV <- ks_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### Indiana
## Make species code a factor
in_tc$SPCD <- as.factor(in_tc$SPCD)
# Group by species/location and summarize to obtain extreme values
# Add state
in_tc$State <- rep("IN",times = length(nrow(in_tc)))
in_r2 <- in_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
in_minSR <- in_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
in_maxSR <- in_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
in_minTC <- in_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
in_maxTC <- in_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
in_minR <- in_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
in_maxR <- in_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
in_minV <- in_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
in_maxV <- in_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### Illinois
## Make species code a factor
il_tc$SPCD <- as.factor(il_tc$SPCD)
# Add state
il_tc$State <- rep("IL",times = length(nrow(il_tc)))
# Group by species/location and summarize to obtain extreme values
il_r2 <- il_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
il_minSR <- il_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
il_maxSR <- il_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
il_minTC <- il_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
il_maxTC <- il_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
il_minR <- il_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
il_maxR <- il_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
il_minV <- il_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
il_maxV <- il_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### Iowa
## Make species code a factor
ia_tc$SPCD <- as.factor(ia_tc$SPCD)
# Add state
ia_tc$State <- rep("IA",times = length(nrow(ia_tc)))
# Group by species/location and summarize to obtain extreme values
ia_r2 <- ia_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
ia_minSR <- ia_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
ia_maxSR <- ia_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
ia_minTC <- ia_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
ia_maxTC <- ia_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
ia_minR <- ia_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
ia_maxR <- ia_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
ia_minV <- ia_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
ia_maxV <- ia_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

##### Minnesota
## Make species code a factor
mn_tc$SPCD <- as.factor(mn_tc$SPCD)
# Add state
mn_tc$State <- rep("MN",times = length(nrow(mn_tc)))
# Group by species/location and summarize to obtain extreme values
mn_r2 <- mn_tc %>%
  group_by(SPCD,State,PLOT,lat,lon) %>%
  dplyr::summarise(., minTC = min(minTemp),
                   maxTC = max(maxTemp),
                   minRain = min(cumPrecip),
                   maxRain = max(cumPrecip),
                   minAVP = min(meanVP),
                   maxAVP = max(meanVP),
                   minASR = min(meanSR),
                   maxASR = max(meanSR))
# Min SR
mn_minSR <- mn_r2 %>%
  group_by(SPCD) %>%
  filter(., minASR == min(minASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -maxASR)
# Max SR
mn_maxSR <- mn_r2 %>%
  group_by(SPCD) %>%
  filter(., maxASR == max(maxASR)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR)
# Min temp
mn_minTC <- mn_r2 %>%
  group_by(SPCD) %>%
  filter(., minTC == min(minTC)) %>%
  dplyr::select(., -maxTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max temp
mn_maxTC <- mn_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxTC == max(maxTC)) %>%
  dplyr::select(., -minTC, -minRain, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min rainfall
mn_minR <- mn_r2 %>%
  group_by(SPCD) %>%
  filter(.,minRain == min(minRain)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Max rainfall
mn_maxR <- mn_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxRain == max(maxRain)) %>%
  dplyr::select(., -minTC, -maxTC, -minRain, -minAVP, -maxAVP, -minASR, -maxASR)
# Min VP
mn_minV <- mn_r2 %>%
  group_by(SPCD) %>%
  filter(.,minAVP == min(minAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -maxAVP, -minASR, -maxASR)
# Max VP
mn_maxV <- mn_r2 %>%
  group_by(SPCD) %>%
  filter(.,maxAVP == max(maxAVP)) %>%
  dplyr::select(., -minTC, -maxTC, -maxRain, -minRain, -minAVP, -minASR, -maxASR)

######################################################################
########## Aggregate Min/Max Values Across States and make lat/long spatial features
# Min Monthly Temperatures
midwMinTC <- rbind(ne_minTC, wi_minTC, mo_minTC, sd_minTC,
                    oh_minTC, nd_minTC, mi_minTC, ks_minTC,
                    in_minTC, il_minTC, ia_minTC, mn_minTC)
# Convert lat/long to sf
MinTC_sf = st_as_sf(midwMinTC, coords = c("lon", "lat"), 
                  crs = 4269)
# Add Species/Common Names to Each Row based on species' code (SPCD)
MinTC_sf$Common <- spp$common[match(MinTC_sf$SPCD, spp$SPCD)]
MinTC_sf$Species <- spp$scientific[match(MinTC_sf$SPCD, spp$SPCD)]
# Write file
write.csv(MinTC_sf, "sppMinTCstate.csv")

# Max Monthly Temperatures
midwMaxTC <- rbind(ne_maxTC, wi_maxTC, mo_maxTC, sd_maxTC,
                   oh_maxTC, nd_maxTC, mi_maxTC, ks_maxTC,
                   in_maxTC, il_maxTC, ia_maxTC, mn_maxTC)
# Convert lat/long to sf
MaxTC_sf = st_as_sf(midwMaxTC, coords = c("lon", "lat"), 
                    crs = 4269)
# Add Species/Common Names to Each Row based on species' code (SPCD)
MaxTC_sf$Common <- spp$common[match(MaxTC_sf$SPCD, spp$SPCD)]
MaxTC_sf$Species <- spp$scientific[match(MaxTC_sf$SPCD, spp$SPCD)]
# Write file
write.csv(MaxTC_sf, "sppMaxTCstate.csv")

# Minimum Monthly Cumulative Rainfall
midwMinR <- rbind(ne_minR, wi_minR, mo_minR, sd_minR,
                   oh_minR, nd_minR, mi_minR, ks_minR,
                   in_minR, il_minR, ia_minR, mn_minR)
# Convert lat/long to sf
MinR_sf = st_as_sf(midwMinR, coords = c("lon", "lat"), 
                    crs = 4269)
# Add Species/Common Names to Each Row based on species' code (SPCD)
MinR_sf$Common <- spp$common[match(MinR_sf$SPCD, spp$SPCD)]
MinR_sf$Species <- spp$scientific[match(MinR_sf$SPCD, spp$SPCD)]
# Write file
write.csv(MinR_sf, "sppMinRstate.csv")

# Max Monthly Cumulative Rainfall
midwMaxR <- rbind(ne_maxR, wi_maxR, mo_maxR, sd_maxR,
                   oh_maxR, nd_maxR, mi_maxR, ks_maxR,
                   in_maxR, il_maxR, ia_maxR, mn_maxR)
# Convert lat/long to sf
MaxR_sf = st_as_sf(midwMaxR, coords = c("lon", "lat"), 
                   crs = 4269)
# Add Species/Common Names to Each Row based on species' code (SPCD)
MaxR_sf$Common <- spp$common[match(MaxR_sf$SPCD, spp$SPCD)]
MaxR_sf$Species <- spp$scientific[match(MaxR_sf$SPCD, spp$SPCD)]
# Write file
write.csv(MaxR_sf, "sppMaxRstate.csv")

# Minimum Avg Vapor Pressure
midwMinV <- rbind(ne_minV, wi_minV, mo_minV, sd_minV,
                  oh_minV, nd_minV, mi_minV, ks_minV,
                  in_minV, il_minV, ia_minV, mn_minV)
# Convert lat/long to sf
MinV_sf = st_as_sf(midwMinV, coords = c("lon", "lat"), 
                   crs = 4269)
# Add Species/Common Names to Each Row based on species' code (SPCD)
MinV_sf$Common <- spp$common[match(MinV_sf$SPCD, spp$SPCD)]
MinV_sf$Species <- spp$scientific[match(MinV_sf$SPCD, spp$SPCD)]
# Write file
write.csv(MinV_sf, "sppMinVstate.csv")

# Maximum Avg Vapor Pressure
midwMaxV <- rbind(ne_maxV, wi_maxV, mo_maxV, sd_maxV,
                  oh_maxV, nd_maxV, mi_maxV, ks_maxV,
                  in_maxV, il_maxV, ia_maxV, mn_maxV)
# Convert lat/long to sf
MaxV_sf = st_as_sf(midwMaxV, coords = c("lon", "lat"), 
                   crs = 4269)
# Add Species/Common Names to Each Row based on species' code (SPCD)
MaxV_sf$Common <- spp$common[match(MaxV_sf$SPCD, spp$SPCD)]
MaxV_sf$Species <- spp$scientific[match(MaxV_sf$SPCD, spp$SPCD)]
# Write file
write.csv(MaxV_sf, "sppMaxVstate.csv")

# Minimum Shortwave Radiation
midwMinSR <- rbind(ne_minSR, wi_minSR, mo_minSR, sd_minSR,
                  oh_minSR, nd_minSR, mi_minSR, ks_minSR,
                  in_minSR, il_minSR, ia_minSR, mn_minSR)
# Convert lat/long to sf
MinSR_sf = st_as_sf(midwMinSR, coords = c("lon", "lat"), 
                   crs = 4269)
# Add Species/Common Names to Each Row based on species' code (SPCD)
MinSR_sf$Common <- spp$common[match(MinSR_sf$SPCD, spp$SPCD)]
MinSR_sf$Species <- spp$scientific[match(MinSR_sf$SPCD, spp$SPCD)]
# Write file
write.csv(MinSR_sf, "sppMinSRstate.csv")

# Maximum Shortwave Radiation
midwMaxSR <- rbind(ne_maxSR, wi_maxSR, mo_maxSR, sd_maxSR,
                  oh_maxSR, nd_maxSR, mi_maxSR, ks_maxSR,
                  in_maxSR, il_maxSR, ia_maxSR, mn_maxSR)
# Convert lat/long to sf
MaxSR_sf = st_as_sf(midwMaxSR, coords = c("lon", "lat"), 
                   crs = 4269)
# Add Species/Common Names to Each Row based on species' code (SPCD)
MaxSR_sf$Common <- spp$common[match(MaxSR_sf$SPCD, spp$SPCD)]
MaxSR_sf$Species <- spp$scientific[match(MaxSR_sf$SPCD, spp$SPCD)]
# Write file
write.csv(MaxSR_sf, "sppMaxSRstate.csv")

######################################################################
####### Mapping Min Monthly Temps by Species Across States

# Subset to only include minimum for each species
minTCspp <- MinTC_sf %>%
            group_by(SPCD) %>%
            filter(., minTC == min(minTC)) 
# Map!
tm_shape(state) +
     tm_polygons(col = "white") +
tm_shape(state) +
     tm_borders(col = "black", lwd = 2) +
#tm_shape(counties) +
  #   tm_polygons(col = "white") +
tm_shape(minTCspp) + 
     tm_dots(col = "Common")



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
                  crs = 4269)

dat_sf

# Create a map of the plot locations by state, Counties
tm_shape(counties) +
    tm_polygons(col = "white") +
tm_shape(counties) +
    tm_borders(col = "#919191") +
tm_shape(dat_sf) + 
    tm_dots(col = "state")


# Map!
tm_shape(counties) +
  tm_polygons(col = "white") +
  tm_shape(counties) +
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
