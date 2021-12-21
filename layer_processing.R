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
#                 RASTER CROPPING
#########################################################

# shape needs to be opened via readOGR() in library(rgdal) 
crop_shape <- function(ref_raster , shape){
  new_shape <- spTransform(shape, crs(ref_raster))
  return(mask(crop(x= ref_raster, y=st_bbox(new_shape)),new_shape))
}


#########################################################
#           OUTLIER DETECTION & REPLACEMENT
#########################################################

# returns a raster with same extent as rast,
# with all values outside (low_end, high_end) converted to some out_value
# and all other values to NA
outliers_rast <- function(rast, low_end, high_end){
  
  out_value = as.integer(summary(rast)[5]*10)  
  # create value outside raster range
  # summary(rast)[5] is the max value on the raster
  
  bad_data <- rast
  bad_data[bad_data<low_end] <- out_value
  bad_data[bad_data>high_end] <- out_value  
  bad_data[bad_data!= out_value] <- NA
  
  return(bad_data)
}
# ----------------------------------------------------

# returns a list of indices of the pixels in rast 
# with values outside (low_end, high_end) 
outliers <- function(rast, low_end, high_end){
  
  bad_indices <- which((rast[] > high_end) | (rast[] < low_end))
  
  return(bad_indices)
}

# ----------------------------------------------------

# returns a copy of the rast raster 
# with the outliers replaced by the average of buffer=100000 cells around
replace_outliers <- function(rast,low_end, high_end){
  
  bad_indices <- outliers(rast,low_end, high_end)
  
  # remove outliers
  new_rast <- rast
  new_rast[new_rast<low_end] <- NA
  new_rast[new_rast>high_end] <- NA
  
  result<- extract(new_rast, 
                   xyFromCell(new_rast, bad_indices), 
                   buffer=100000, 
                   fun=mean,
                   na.rm=TRUE)
  new_rast[bad_indices] <- result
  return(new_rast)
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

##################################################################################
#                       ARIDITY INDEX
# functions originally coded at aridity_replacing_outliers.Rmd
# 

#################################################################################

# reclassify values into three groups: 
# 1 = desert => AI < 0.05
# 2 = dry => 0.05 <= AI <= 0.65
# 3 = moist => 0.65 < AI
# https://www.rdocumentation.org/packages/raster/versions/3.5-2/topics/reclassify
# min, max, new_value
aridity_regions <- function(arid_raster){
  m <- c(0, 500 , 1,  
         500, 6500, 2,  
         6500, Inf, 3)
  thresh <- matrix(m, ncol=3, byrow=TRUE)
  
  # double check open/closed intervals
  return(reclassify(arid_raster*10000,thresh))
}
