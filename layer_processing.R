#########################################################
#   FUNCTIONS TO PROCESS ECOLOGICAL LAND UNITS LAYERS
# - land cover
# - temperature
# - aridity index
#########################################################


# library(raster)
# library(rgdal)
# library(tidyverse)
# library(here)
# library(sf) 


#########################################################
#                 RASTER PREPARATION
#########################################################

crop_shape <- function(ref_raster , shape){
  new_shape <- spTransform(shape, crs(ref_raster))
  return(mask(crop(x= ref_raster, y=st_bbox(new_shape)),new_shape))
}


###################################################################################################
#                       LANDCOVER
# functions originally coded at landcover_create_layer.Rmd

# Land cover raw data is obtained from Multi-Resolution Land Characteristics Consortium
# https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover

###################################################################################################

# Aggregate land cover classifications
# original classifications here:
# https://www.mrlc.gov/data/legends/national-land-cover-database-2019-nlcd2019-legend

# 11 : open water          (10)
# 12 : perennial ice/snow  (1)
# 21-24 : developed        (2)
# 31 : barren              (3)
# 41-43 : forest           (4)
# 51,52 : shrubland        (5)
# 71-74 : grassland        (7)
# 81,82: cropland          (8)
# 90,95: wetland           (9)

landcover_regions <- function(lc_raster){
  thresh<- cbind( c(1:9)*10, c(2:10)*10, c(1:9)) 
  thresh[1,1] <-12
  
  new_raster <- lc_raster
  new_raster[new_raster == 11] <- 10
  
  return(reclassify(new_raster,thresh, right=FALSE))
}

#########################################################
#                     BIOMASS
#########################################################

# reclassify a raster pre-processed by landcover_regions 
# to include only classes deemed relevant for biomass analysis
biomass_regions <- function(lc_raster){
  
  # category (initial code)  -> biomass code
  # forest (4) -> 4 
  # shrub  (5) -> 3 
  # grass  (7) -> 2
  # barren (3) -> 1
  # developed (2) -> 0
  
  thresh <- rbind(c(5,3), c(7,2), c(3,1), c(2,0))
  new_raster <- reclassify(lc_raster,thresh, right=FALSE)
  new_raster[ !(new_raster %in% 0:4) == TRUE] <- NA 
  
  return(new_raster)
}


