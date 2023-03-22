library(tidyverse)
library(readr)
library(sf)
library(units)
library(terra)
library(mapview)
library(rosm)
library(ggspatial)
library(writexl)
library(tibble)
attach(count)
attach(campusP)



countP <- st_as_sf(count, coords = c("GPSE", "GPSS"), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

countP %>% mapview()

campus = campus_mila_1_


campusP = st_as_sf(campus, coords = c("east" ,"south"), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

campusP %>% mapview((zcol = 'fgcm')) + countP %>% mapview(col.regions = c("black", "red", "White"), layer.name = "Campus Location", cex = 10)


      
           
