
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
library(raster)
library(spdep)


NEsoc <- raster::raster("./State Soil Tifs/NEsoc_150cm_500m.tif") 
plot(NEsoc)
crs(NEsoc)

d.all <- sf::read_sf("./County Data/County_2010Census_DP1.shp")


mdwUS <- d.all %>% 
      dplyr::filter(stringr::str_starts(GEOID10, "17") |
                    stringr::str_starts(GEOID10, "18") |
                    stringr::str_starts(GEOID10, "19") |
                    stringr::str_starts(GEOID10, "20") |
                    stringr::str_starts(GEOID10, "26") |
                    stringr::str_starts(GEOID10, "27") |
                    stringr::str_starts(GEOID10, "29") |
                    stringr::str_starts(GEOID10, "31") |
                    stringr::str_starts(GEOID10, "38") |
                    stringr::str_starts(GEOID10, "39") |
                    stringr::str_starts(GEOID10, "46") |
                    stringr::str_starts(GEOID10, "55"))

# Remove the ALAND10 and AWATER10 variables to reduce shapefile size
mdwUS2 <- dplyr::select(mdwUS, -ALAND10, -AWATER10)

sf::write_sf(mdwUS2, "./County Data/midwestCB.shp")


# Shapefile for state boundaries
states <- sf::read_sf("./tl_2012_us_state/tl_2012_us_state.shp") %>% st_make_valid()
# Transform CRS
state <- st_transform(states, "NAD83")

midwestST <- state %>% 
  dplyr::filter(STATEFP == "17" |
                STATEFP == "18" |
                STATEFP == "19" |
                STATEFP == "20" |
                STATEFP == "26" |
                STATEFP == "27" |
                STATEFP == "29" |
                STATEFP == "31" |
                STATEFP == "38" |
                STATEFP == "39" |
                STATEFP == "46" |
                STATEFP == "55")

glimpse(midwestST)

tm_shape(midwestST) +
  tm_polygons(col = "white") +
  tm_shape(midwestST) +
  tm_borders(col = "black", lwd = 2)

mwS2 <- dplyr::select(midwestST, -ALAND, -AWATER)

sf::write_sf(mwS2, "./County Data/midwestSB.shp")

