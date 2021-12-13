
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

##### Nebraska
# Average Lat/Longs for each plot for climate data
ne_ll <- ne %>%
         group_by(PLOT) %>%
         dplyr::summarise(., plot_lat = mean(LAT),
                             plot_lon = mean(LON))
colnames(ne_ll) <- c("site", "lat", "lon")
#write.csv(ne_ll, "ne_latlong.csv", row.names=FALSE)
ne_ll$state <- rep("NE",times = length(nrow(ne_ll)))

##### Wisconsin
# Average Lat/Longs for each plot for climate data
wi_ll <- wi %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(wi_ll) <- c("site", "lat", "lon")
#write.csv(wi_ll, "wi_latlong.csv", row.names=FALSE)
wi_ll$state <- rep("WI",times = length(nrow(wi_ll)))

##### Missouri
# Average Lat/Longs for each plot for climate data
mo_ll <- mo %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(mo_ll) <- c("site", "lat", "lon")
#write.csv(mo_ll, "mo_latlong.csv", row.names=FALSE)
mo_ll$state <- rep("MO",times = length(nrow(mo_ll)))

##### South Dakota
# Average Lat/Longs for each plot for climate data
sd_ll <- sd %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(sd_ll) <- c("site", "lat", "lon")
#write.csv(sd_ll, "sd_latlong.csv", row.names=FALSE)
sd_ll$state <- rep("SD",times = length(nrow(sd_ll)))

##### Ohio
# Average Lat/Longs for each plot for climate data
oh_ll <- oh %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(oh_ll) <- c("site", "lat", "lon")
#write.csv(oh_ll, "oh_latlong.csv", row.names=FALSE)
oh_ll$state <- rep("OH",times = length(nrow(oh_ll)))

##### North Dakota
# Average Lat/Longs for each plot for climate data
nd_ll <- nd %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(nd_ll) <- c("site", "lat", "lon")
#write.csv(nd_ll, "nd_latlong.csv", row.names=FALSE)
nd_ll$state <- rep("ND",times = length(nrow(nd_ll)))

##### Michigan
# Average Lat/Longs for each plot for climate data
mi_ll <- mi %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(mi_ll) <- c("site", "lat", "lon")
#write.csv(mi_ll, "mi_latlong.csv", row.names=FALSE)
mi_ll$state <- rep("MI",times = length(nrow(mi_ll)))

##### Kansas
# Average Lat/Longs for each plot for climate data
ks_ll <- ks %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(ks_ll) <- c("site", "lat", "lon")
#write.csv(ks_ll, "ks_latlong.csv", row.names=FALSE)
ks_ll$state <- rep("KS",times = length(nrow(ks_ll)))

##### Indiana
# Average Lat/Longs for each plot for climate data
ind_ll <- ind %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(ind_ll) <- c("site", "lat", "lon")
#write.csv(ind_ll, "ind_latlong.csv", row.names=FALSE)
ind_ll$state <- rep("IN",times = length(nrow(ind_ll)))

##### Illinois
# Average Lat/Longs for each plot for climate data
il_ll <- il %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(il_ll) <- c("site", "lat", "lon")
#write.csv(il_ll, "il_latlong.csv", row.names=FALSE)
il_ll$state <- rep("IL",times = length(nrow(il_ll)))

##### Iowa
# Average Lat/Longs for each plot for climate data
ia_ll <- ia %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(ia_ll) <- c("site", "lat", "lon")
#write.csv(ia_ll, "ia_latlong.csv", row.names=FALSE)
ia_ll$state <- rep("IA",times = length(nrow(ia_ll)))

##### Minnesota
# Average Lat/Longs for each plot for climate data
mn_ll <- mn %>%
  group_by(PLOT) %>%
  dplyr::summarise(., plot_lat = mean(LAT),
                   plot_lon = mean(LON))
colnames(mn_ll) <- c("site", "lat", "lon")
#write.csv(mn_ll, "mn_latlong.csv", row.names=FALSE)
mn_ll$state <- rep("MN",times = length(nrow(mn_ll)))

## Midwest FIA Plot Locations
midwestFIA <- rbind(ne_ll, wi_ll, mo_ll, sd_ll,
                    oh_ll, nd_ll, mi_ll, ks_ll,
                    ind_ll, il_ll, ia_ll, mn_ll)
# Convert Lat/Longs to spatial features - NAD83 to match FIA procedure
dat_sf = st_as_sf(midwestFIA, coords = c("lon", "lat"), 
                  crs = 4259)
# Plot
tm_shape(dat_sf) + 
  tm_dots(col = "state")

# Counties
tm_shape(counties) +
  tm_polygons() +
  tm_shape(dat_sf) + 
  tm_dots(col = "state")

###########################################################
# Download Climate Data from 1980-2020
# daymet.test2.ne <- download_daymet_batch(file_location = "ne_latlong.csv",
#                                          start = 1980,
#                                          end = 2020,
#                                          internal = TRUE,
#                                          simplify = FALSE)

# Procedure with single row of data

ne <- read.csv("ne_latlong.csv")
head(ne)

first.row <- ne[1,]

day.test <- download_daymet(site = first.row$site,
                          lat = first.row$lat,
                          lon = first.row$lon,
                          start = 2018,
                          end = 2019,
                          internal = TRUE,
                          simplify = F) # returns tidy data!

dat.test <- day.test$data

# 2018
first.row$janMaxTemp <- mean(day.test$data$tmax..deg.c.[1:31]) #2018
first.row$janMinTemp <- mean(day.test$data$tmin..deg.c.[1:31]) #2018
first.row$janMeanVP <- mean(day.test$data$vp..Pa.[1:31]) #2018
first.row$julMaxTemp <- mean(day.test$data$tmax..deg.c.[182:212]) #2018
first.row$julMinTemp <- mean(day.test$data$tmin..deg.c.[182:212]) #2018
first.row$julMeanVP <- mean(day.test$data$vp..Pa.[182:212]) #2018
first.row$annPrecip <- sum(day.test$data$prcp..mm.day.[1:365]) #2018
# 2019
first.row$janMaxTemp2 <- mean(day.test$data$tmax..deg.c.[366:396]) #2019
first.row$janMinTemp2 <- mean(day.test$data$tmin..deg.c.[366:396]) #2019
first.row$janMeanVP2 <- mean(day.test$data$vp..Pa.[366:396]) #2019
first.row$julMaxTemp2 <- mean(day.test$data$tmax..deg.c.[547:577]) #2019
first.row$julMinTemp2 <- mean(day.test$data$tmin..deg.c.[547:577]) #2019
first.row$julMeanVP2 <- mean(day.test$data$vp..Pa.[547:577]) #2019
first.row$annPrecip2 <- sum(day.test$data$prcp..mm.day.[366:730]) #2019

View(first.row)

first.row <- ne[1,]
 
# Function to extract climate data
get_daymet <- function(...){
  print(...)
  my.row <- as.tibble(...)
  #my.row <- first.row
  # Extract plot, lat, and long for each by row
  temp_site <- my.row[,1]
  temp_lat <- my.row[,2]
  temp_lon <- my.row[,3]
  print(temp_site)
  # Run download_daymet on each location
  temp_daymet <- download_daymet(site = temp_site,
                           lat = temp_lat,
                           lon = temp_lon,
                           start = 2018,
                           end = 2019) %>% 
    # Extract df with environmental vars from list
    .$data %>% 
    # Convert to tibble
    as_tibble() %>% 
    # Only include the variables you want
    dplyr::select(., -dayl..s., -srad..W.m.2., -swe..kg.m.2.) %>% 
    # Give each its plot_id
    mutate(site_id = temp_site) %>% 
    # Get date from day of the year using lubridate
    mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))
  return(temp_daymet)
}  


returnmap_dfr(ne, get_daymet)
 
test <- ne %>% purrr::map_dfr(get_daymet)

test2 <- purrr::map_dfr(ne, .f= get_daymet)

test <- ne %>% purrr::pmap_dfr(get_daymet)

unique(test$site_id)

# # Function to extract climate data
# get_daymet <- function(my.row){
#   # Extract plot, lat, and long for each by row
#   temp_site <- dat[i, ] %>% pull(site)
#   temp_lat <- dat[i, ] %>% pull(lon)
#   temp_lon <- dat[i, ] %>% pull(lat)
#   # Run download_daymet on each location
#   temp_daymet <- download_daymet(site = temp_site,
#     lat = temp_lat,
#     lon = temp_lon,
#     start = 2000,
#     end = 2020) %>% 
#     # Extract df with environmental vars from list
#     .$data %>% 
#     # Convert to tibble
#     as_tibble() %>% 
#     # Give each its plot_id
#     mutate(site_id = temp_site) %>% 
#     # Get date from day of the year using lubridate
#     mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))
#   return(temp_daymet)
# }  


# Loop over all rows to get data for every plot (every lat/long pair)
(
  daymet_all_points <- lapply(dat, get_daymet) %>% 
  #--- need to combine the list of data.frames into a single data.frame ---#
    bind_rows()
)



daymet.test2.ne$data

# Resample soils/climate to be same resolution
# ?Resample - bilinear may be the best resolution

# Put all points on a 2 by 2 km resolution

# Create new columns for lats/longs
# Average by plot for lats/longs
# Use the lats/longs to extract 
# 2x2 km = 500 acres

# Day Met R
