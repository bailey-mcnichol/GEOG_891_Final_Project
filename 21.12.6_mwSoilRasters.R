
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
NEcarbon <- raster::aggregate(NEsoc, fact = 2, fun = mean)

# List all Tif Raster Files in Folder
tifName <- list.files(path = "./State Soil Tifs", 
                      pattern = '.tif')
# Extract length of list
tifFile <- length(tifName)

# # Resample reclass1 image based on reclass2 image
# for (i in 1:tifFile) {
#   r1<-raster::raster(tifName[[i]])    
#   rs<-raster::aggregate(r1, fact = 2, fun = mean) 
#   # Save the ouput
#   writeRaster(rs, paste0("./Resampled Rasters/rs_",i), overwrite=T)
# }
# 
# 
# x<- tifName[1]

## Function to aggregate resolution of soil rasters from 500 x 500 m to the 1000 x 1000 m scale
myfun <- function(x){
  # Call files from this folder
  r1 <- raster::raster(paste0("./State Soil Tifs/", x))
  # Use aggregate() with a factor of two and the mean function to decrease resolution to 1000 x 1000 m 
  rs<-raster::aggregate(r1, fact = 2, fun = mean) 
  # Write the new files into this folder
  writeRaster(rs, paste0("./Resampled Rasters/rs_",x),overwrite=T)
}

## Run each file through the map function to resample each raster tif
tifName %>% map(., myfun)








