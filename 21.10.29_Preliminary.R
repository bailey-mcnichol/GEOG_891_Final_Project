
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

## Read in shapefile with Nebraska Counties
counties <- sf::read_sf("./County_Boundaries-_Census.shp")

## Load in FIA data
neFIA = read.csv("Niobrara FIA Data_20211026.csv")

## Load in 30-year average climate data (1980-2010) - 800-m resolution
climNorm = read.csv("neFIA_30yr1980-2010_800m_monthly_normals.csv")

## Load in species codes/names for woody species of interest
spp = read_excel("Species for inventory data across range in North America.xlsx")

## To look at color palettes!
tmaptools::palette_explorer()

## Extract unique lat-long pairs to obtain NE FIA Plot Locations
neFIAlatlong <- neFIA %>% 
                    distinct(LAT, LON)
## Create a dataset of these for use extracting environmental variables/soils data
write.csv(neFIAlatlong, file = "neFIAlatlong.csv")


## Use the st_as_sf function to convert lat/long into a spatial object
## CRS (Coordinate Reference System) 4326 is for WGS84
# CRS used by organizations that provide GIS data for the entire globe or many countries
neFIA$Longitude = neFIA$LON
neFIA$Latitude = neFIA$LAT
dat_sf = st_as_sf(neFIA, coords = c("LON", "LAT"), 
                 crs = 4326)

## To make sure that longitude/latitude are showing up as we would expect
plot(dat_sf)

## Examine CRS - correctly assigned
dat_sf %>% sf::st_crs()

## Determine which counties the NE FIA plots fall within
table(dat_sf$COUNTYCD)

## Plot the FIA plot locations and color code by county
map <- tm_shape(dat_sf) + 
          tm_dots(col = "COUNTYNAME")
map +  tm_layout(legend.outside = TRUE)


## Use the st_as_sf function to convert lat/long for climate variables into spatial object with the WGS84 CRS (to match the FIA plot CRS)
clim_sf = st_as_sf(climNorm, coords = c("Longitude", "Latitude"), 
                  crs = 4326)

### Create a 1600-m buffer around each lat/long with climate data
clim_buff <- sf::st_buffer(clim_sf, dist = 800)

## Create spatial intersection between FIA plots and climate variables
plot_clim <- sf::st_intersection(dat_sf, clim_buff)

## Add Species/Common Names to Each Row based on species' code (SPCD)
plot_clim$Common <- spp$'COMMON NAME'[match(plot_clim$SPCD_2, spp$SPCD)]
plot_clim$Species <- spp$'SCIENTIFIC NAME'[match(plot_clim$SPCD_2, spp$SPCD)]

## Filter data to only include plots with Niobrara species
niob <- dplyr::filter(plot_clim, Common=="American basswood"| 
                Common=="black walnut" | Common=="American elm" |
                Common=="boxelder maple" | Common=="bur oak" |
                Common=="chokecherry" | Common=="eastern cottonwood" |
                Common=="eastern hophornbeam" | Common=="eastern redcedar" |
                Common=="green ash" | Common=="hackberry" | 
                Common=="ponderosa pine" | Common=="slippery elm")
dim(niob)

## Count number of plots - 77 with the Niobrara species represented
plyr::count(niob$PLOT)

#### Determine the extremes that each species can experience/the locations
niob <- filter(niob, Month != "Annual")

## Can't get this to retain latitude and longitude for each!!
# niobLim <- niob %>%
#            group_by(Common) %>%
#                dplyr::summarise(., 
#                     minPrecip = min(ppt_mm, na.rm = TRUE),
#                     maxPrecip = max(ppt_mm, na.rm = TRUE),
#                     minTemp = min(tmin_C, na.rm = TRUE),
#                     maxTemp = max(tmax_C, na.rm = TRUE),
#                     minVPD = min(vpdmin_hPa, na.rm = TRUE),
#                     maxVPD = max(vpdmax_hPa, na.rm = TRUE)) %>%
#             select(., Latitude, Longitude)

## Extract color codes for the 13 species

colourCount = 13
getPalette = colorRampPalette(brewer.pal(8, "Accent"))

## Minimum Monthly Precipitation
minRain <- niob %>%
        group_by(Common) %>%
        mutate(minPrecip = min(ppt_mm)) %>%
        arrange(Common) %>%
        filter(minPrecip == ppt_mm) %>%
        select(., Common, Latitude, Longitude, Month, minPrecip) %>%
        slice(1)
# PLOT
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(minRain) +
  tm_dots(col = "Common", size = 0.4, title = "Species", jitter = 0.2,
          palette=getPalette(colourCount)) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "bottom",
            legend.stack = "vertical")

  
## Maximum Monthly Precipitation
maxRain <- niob %>%
        group_by(Common) %>%
        mutate(maxPrecip = max(ppt_mm)) %>%
        arrange(Common) %>%
        filter(maxPrecip == ppt_mm) %>%
        select(., Common, Latitude, Longitude, Month, maxPrecip)  %>%
        slice(1)
# PLOT
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(maxRain) +
  tm_dots(col = "Common", size = 0.4, title = "Species", jitter = 0.2,
          palette=getPalette(colourCount)) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "bottom",
            legend.stack = "vertical")

## Minimum Monthly Temp
minTemp <- niob %>%
      group_by(Common) %>%
      mutate(minTC = min(tmin_C)) %>%
      arrange(Common) %>%
      filter(minTC == tmin_C) %>%
      select(., Common, Latitude, Longitude, Month, minTC) %>%
      slice(1)
# PLOT
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(minTemp) +
  tm_dots(col = "Common", size = 0.4, title = "Species", jitter = 0.2,
          palette=getPalette(colourCount)) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "bottom",
            legend.stack = "vertical")

## Maximum Monthly Temp
maxTemp <- niob %>%
      group_by(Common) %>%
      mutate(maxTC = max(tmax_C)) %>%
      arrange(Common) %>%
      filter(maxTC == tmax_C) %>%
      select(., Common, Latitude, Longitude, Month, maxTC)  %>%
      slice(1)
# PLOT
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(maxTemp) +
  tm_dots(col = "Common", size = 0.4, title = "Species", jitter = 0.2,
          palette=getPalette(colourCount)) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "bottom",
            legend.stack = "vertical")

## Minimum Monthly VPD
minVPD <- niob %>%
      group_by(Common) %>%
      mutate(minV = min(vpdmin_hPa)) %>%
      arrange(Common) %>%
      filter(minV == vpdmin_hPa) %>%
      select(., Common, Latitude, Longitude, Month, minV)  %>%
      slice(1)
# PLOT
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(minVPD) +
  tm_dots(col = "Common", size = 0.4, title = "Species", jitter = 0.2,
          palette=getPalette(colourCount)) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "bottom",
            legend.stack = "vertical")

## Maximum Monthly VPD
maxVPD <- niob %>%
  group_by(Common) %>%
  mutate(maxV = max(vpdmax_hPa)) %>%
  arrange(Common) %>%
  filter(maxV == vpdmax_hPa) %>%
  select(., Common, Latitude, Longitude, Month, maxV)  %>%
  slice(1)
# PLOT
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(maxVPD) +
  tm_dots(col = "Common", size = 0.4, title = "Species", jitter = 0.2,
          palette=getPalette(colourCount)) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "bottom",
            legend.stack = "vertical")


#### Filter data to only include July climate data
july <- filter(niob, Month == "July")

## Plot mean monthly July precipitation in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
tm_shape(july) +
      tm_dots(col = "ppt_mm", size = 0.5, title = "Rainfall (mm)",
              breaks = c(40, 55, 70, 85, 100, 115, 120)) 

## Plot mean monthly July temp in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
tm_shape(july) +
      tm_dots(col = "tmean_C", size = 0.5,
              breaks = c(21, 22, 23, 24, 25, 26),
              title = "Mean Temp. (°C)")

## Plot min monthly July temp in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(july) +
  tm_dots(col = "tmin_C", size = 0.5, 
          title = "Minimum Temp. (°C)")

## Plot max monthly July temp in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(july) +
  tm_dots(col = "tmax_C", size = 0.5, 
          title = "Maximum Temp. (°C)")

## Plot the FIA plot elevations
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
tm_shape(july) +
    tm_dots(col = "Elevation..m.", size = 0.5, title = "Elevation (m)",
            breaks = c(200, 400, 600, 800, 1000, 1200, 1400))

## Plot min monthly July VPD in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
tm_shape(july) +
  tm_dots(col = "vpdmin_hPa", size = 0.5,
          title = "Minimum VPD (hPa)")

## Plot max monthly July VPD in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
tm_shape(july) +
  tm_dots(col = "vpdmax_hPa", size = 0.5, 
          breaks = c(18, 21, 24, 27, 30, 34),
          title = "Maximum VPD (hPa)")


#### Filter data to only include December climate data
december <- filter(niob, Month == "December")

## Plot mean monthly December precipitation in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(december) +
  tm_dots(col = "ppt_mm", size = 0.5, title = "Rainfall (mm)",
          palette = "cividis")
          #breaks = c(40, 55, 70, 85, 100, 115, 120)) 

## Plot mean monthly December temp in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(december) +
  tm_dots(col = "tmean_C", size = 0.5,
          breaks = c(-6, -5, -4, -3, -2, -1),
          title = "Mean Temp. (°C)",
          palette = "cividis")

## Plot min monthly December temp in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(december) +
  tm_dots(col = "tmin_C", size = 0.5, 
          title = "Minimum Temp. (°C)",
          palette = "cividis")

## Plot max monthly December temp in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(december) +
  tm_dots(col = "tmax_C", size = 0.5, 
          title = "Maximum Temp. (°C)",
          palette = "cividis")

## Plot min monthly December VPD in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(december) +
  tm_dots(col = "vpdmin_hPa", size = 0.5,
          title = "Minimum VPD (hPa)",
          palette = "cividis")

## Plot max monthly December VPD in each plot 
tm_shape(counties) +
  tm_polygons(col = "#FFFFFF") +
  tm_shape(december) +
  tm_dots(col = "vpdmax_hPa", size = 0.5, 
          title = "Maximum VPD (hPa)",
         palette = "cividis")


###############################################
## Reading in the Soils data from the gSSURGO GDB
# The input file geodatabase
soils_gdb <- "gSSURGO_NE.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(soils_gdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="some_featureclass")

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)


##

parks <- sf::read_sf("./State_Park_Locations.shp")
