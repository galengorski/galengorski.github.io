#===================================================================================#
# NOTES:
# #this is working off a tutorial from this site:
# http://matthewrvross.com/active.html
# https://rspatial.org/raster/rosu/Chapter11.htmls
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# ggorski@ucsc.edu
# 05-27-2020
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
install.packages('devtools')
library(devtools)
install.packages('sf')
library(sf)
install.packages('mapview')
library(mapview)
install.packages('mapedit')
library(mapedit)
install.packages('rayshader')
library(rayshader)
install.packages('tidyverse')
library(tidyverse)
install.packages('elevatr')
library(elevatr)
install.packages('raster')
library(raster)
devtools::install_github("giswqs/whiteboxR")
library(whitebox)
install.packages('stars')
library(stars)
install.packages('rgl')
library(rgl)
install.packages('rgdal')
library(rgdal)
knitr::knit_hooks$set(webgl = hook_webgl)
install.packages('tibble')
library(tibble)
install.packages('magrittr')
library(magrittr)
install.packages('dplyr')
library(dplyr)
#####
#===================================================================================#

library(sf)
library(mapview)
library(elevatr)
library(raster)

setwd("~/Google Drive File Stream/My Drive/UCSC Google Drive/Projects/Extra/Website/Soil_Example/")

vm <- st_read("vanmeter.shp")

mapview(vm)

vm_dem <- get_elev_raster(vm,z=10)

mapview(vm_dem)+
  mapview(vm, col.regions = 'transparent', alpha.regions = 0, lwd = 3)

vm_clipped <- mask(vm_dem, vm)

mapview(vm_clipped, na.color = 'transparent')+
  mapview(vm, col.regions = 'transparent', alpha.regions = 0, lwd = 3)


vmsites <- tibble(site=c('Sac City','Panora','Jefferson','Redfield','Van Meter'),
                lat=c(42.35475,41.687209,41.98804,41.589432, 41.53388),
                long=c(-94.99033,-94.371077, -94.37691, -94.151346,-93.94995)) %>%
  #Convert to spatial object
  st_as_sf(coords=c('long','lat'),crs=4326) %>%
  #transform to NAD83 northern Colorado projection %>%
  st_transform(26953)

## Use elevatr::get_elev_raster to download data. Z sets the resolution 
# 14 is highest resolution, 1 is lowest
rac_dem <- elevatr::get_elev_raster(sites,z=12)

#generate a box and check topo basemap for full watershed capture
rac_dem <- st_bbox(rac_Dem) %>% st_as_sfc()
mapview(rac_dem) + 
  #  mapview(kank_box) +
  mapview(sites) 
