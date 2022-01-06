#########################################################
#   FUNCTIONS TO PROCESS ECOLOGICAL LAND UNITS LAYERS
# - land cover
# - biomass
# - aridity index
# - temperature (climate)
# - landforms

#########################################################

# libraries needed:

# library(raster)
# library(rgdal)
# library(tidyverse)
# library(here)
# library(sf) 


#########################################################
#                 RASTER CROPPING
#########################################################

# shape passed as parameter needs to be opened via readOGR() in library(rgdal) 
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


########################################################################
#                       CLIMATE
# functions originally coded at climate_regions.Rmd
#
#######################################################################
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#  FUNCTIONS FOR NAVIGATING WorldClim2 DOWNLOADED FILES

# ---- NOTES ABOUT FILES ----
# 2010 - 2019 decade folders only has files until 2018
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#----- generate file name for WorldClim2 downloaded data
# tminmax (string): "tmin" or "tmax"
# year (int): 1960 through 2018
# month (int): 1 = January, ... , 12 = December
file_name <- function(tminmax,year,month){
  file <- paste("wc2.1_2.5m_",tminmax,"_",year,"-", sep="")
  if(month<10){
    file <- paste(file,'0',sep="")
  }
  file <- paste(file,month,".tif",sep="")
  return(file)
}

#----- generate folder name for WorldClim2 downloaded data
# tminmax (string): "tmin" or "tmax"
# year (int): 1960 through 2018
folder_name <- function(tminmax,year){
  decade <- year - (year%%10)
  folder <- paste("wc2.1_2.5m_",tminmax,"_",decade,"-",decade+9,sep="")
  return(folder)
}

#------------------------------------------------------------------------------
#  CALCULATE MONTHLY AVERAGES
#------------------------------------------------------------------------------

# Returns raster with temp average (min + max)/2 on specified month
# year, month = year and month (1-12) on which to compute average temperature
# bbox = bounding box of area of interest (WorldClim2 data is global)

month_avg <- function(year,month,bbox){
  
  # these paths depend on where downloaded data has been stored
  min_rast <- raster(here("raw_data", 
                          "world_clim_historical_monthly_data",
                          "min_avg_temp",
                          folder_name("tmin",year),
                          file_name("tmin",year,month)))
  min_rast <- crop(x= min_rast, y=bbox)
  
  max_rast <- raster(here("raw_data",
                          "world_clim_historical_monthly_data",
                          "max_avg_temp",
                          folder_name("tmax",year),
                          file_name("tmax",year,month)))
  max_rast <- crop(x= max_rast, y=bbox)
  
  return((min_rast+max_rast)/2)
}

#------------------------------------------------------------------------------

#------ create average over time period
# return raster stack with avg temperatures for every month over [year-n,year-1]
# ex: year=2001, n=30, then returns all monthly averages over [1971,2000]
# bbox = bounding box of area of interest
# structure of resulting raster stack is 
#    [(yr_1,mth_1), ..., (yr_1,mth_12), ... , (yr_n,mth_1), ...,(yr_n,mth_12)]

all_monthly_avgs <- function(year,n,bbox){
  months <- stack()
  for( i in c((year-n):(year-1))){
    for(j in 1:12){
      months <- addLayer(months, month_avg(i,j,bbox))
    }
  }
  return(months)
}

#------------------------------------------------------------------------------
#  CREATE CLIMATE REGIONS

# (-inf, 0) and every month avg<10: polar   (1)
# (-inf, 0): boreal                         (2)
#  [0,10) : cold temperate                  (3)
#  [10,18) : warm temperate                 (4)
#  [18,24) : subtropical                    (5)
#  [24,34) : tropical                       (6)
#------------------------------------------------------------------------------

# this is a pre-processing function that separates all climate regions except polar and boreal
# temp_map = raster with average temperatures (30 year avg)
pre_temp_regions <- function(temp_map){
  thresh<- matrix( c(-Inf, 0, 1, # this will be split into polar (1) and boreal (2)
                     0, 10 , 3,
                     10,18 , 4,
                     18,24 , 5,
                     24,43 , 6),
                   nrow=5,
                   byrow=TRUE) 
  # * coming out of here category (1) = polar or boreal *
  return(reclassify(temp_map,thresh, right=FALSE)) # a<= raster <b
}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# separate polar and boreal regions
# pre_regions = raster with temperature regions coming from pre_temp_regions (codes 1,3-6 and 1= polar or boreal)
# months_weather = raster stack with monthly average temperatures in 30 years 
#     [30 yr avg January, ... , 30 yr avg December]

separate_polar_boreal <- function(pre_regions, months_weather){
  
  # ..............................................................
  # id polar+boreal regions over each month
  polar_boreal <- pre_regions
  polar_boreal[polar_boreal!=1] <- NA 
  months_boreal = polar_boreal*months_weather  
  
  # ..............................................................
  # choose months in months_boreal with possible boreal regions (some month has avg temp>=10)
  drop <- c() # list of indices that need to be dropped
  for (i in 1:12){
    # if all temps on that month are <10, then cannot be boreal
    if(maxValue(months_boreal[[i]]) <10){ 
      drop <- c(drop,i)  
    }
  }
  months_boreal <- dropLayer(months_boreal, drop) 
  
  # ..............................................................
  # select pixel in months_boreal with avg temp >=10
  #boreal_region <- months_boreal >= 10
  thresh <- matrix(c(-Inf, 10, 0, 10, Inf, 1), nrow=2,byrow=TRUE)
  boreal_region <- reclassify(months_boreal,thresh)
  if(nlayers(boreal_region)>1){
    boreal_region <- sum(boreal_region) 
    boreal_region[boreal_region>0] <- 1 
  }
  # at this point every boreal pixel = 1, polar pixels = 0, all others NA
  
  # ..............................................................
  # add boreal region to original temp regions raster
  boreal_region[is.na(boreal_region) == TRUE] <- 0 # so you don't have NA + 0 adding the raster layers in next step
  return(pre_regions + boreal_region) # original layer 1 + 1 = 2 (boreal)
  
}

# ---------------------------------------------------------------------------
#   CREATE AVERAGE TEMP + REGIONS
# ---------------------------------------------------------------------------

# returns the raster of temperature regions for temp_map
# temp_map = raster with average temperatures (30 year avg)
# months_weather = raster stack with every monthly average in time period
#    [(yr_1,mth_1), ..., (yr_1,mth_12), ... , (yr_n,mth_1), ...,(yr_n,mth_12)]
# can be obtained from all_monthly_avgs function
temp_regions <- function(temp_map, all_months_weather){
  # ..............................................................
  # create raster stack with monthly average temperatures in time period
  #     avg_per_month = [n yr avg January, ... , n yr avg December]
  avg_per_month <- stack()
  
  n = nlayers(all_months_weather)/12  # assumes you have data for every month in year
  for(i in 1:12){
    aux <- stack()
    for ( j in 0:(n-1)){
      aux <- addLayer(aux, all_months_weather[[j*12+i]])  
    }
    avg_per_month <- addLayer(avg_per_month, mean(aux))
    rm(aux)
  }
  # ..............................................................  
  return(separate_polar_boreal(pre_temp_regions(temp_map),
                               avg_per_month))
}

#------------------------------------------------------------------------------

# ---- create average over time period + regions

# year (int): 1990 through 2018
# n:time period over which to average (suggested geq to 30)
# bbox: bounding box of area of interest (WorldClim2 data is global)
# returns a raster stack
#   1st layer = avg over time period [year-n,year-1]
#      ex: year=2001, n=30, then returns average temperature over [1971,2000]
#   2nd layer = regions based on 1st layer

climate_and_regions <- function(year,n,bbox){
  months_weather <- all_monthly_avgs(year,n,bbox)
  
  result <- stack()
  result <- addLayer(result, mean(months_weather))
  result <- addLayer(result, temp_regions(result[[1]],months_weather))
  
  return(result)
}


#########################################################
#                     LANDFORMS
#########################################################

landform_regions <- function(landform_rast){
  thresh<- matrix(c(26,1,
                    27,2,
                    31,4,
                    32,3), nrow=4,byrow=TRUE)
  return(reclassify(landform_rast,thresh))
}