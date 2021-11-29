#library(raster)
#library(sf)
#library(here)
#library(tidyverse)

#*****************************************************************
# COPIED FROM comparing_elu_maps.Rmd
#*****************************************************************
#*
# -----------------------------------------------------------------------------

# see how category i in raster A changes in raster B  
# B has categories catsB = (b_1,...,b_n)  (b_i < b_(i+1) integers)
# ** modified from landcover_comparing_SB.rmd **

change_i <- function(rasterA,i, rasterB,catsB){
  change <- rasterA
  change[change!=i] <- NA     # select category A_i
  change <- change - rasterB  # change between A_i and B (change can equal 0)
  # if categories for rasters = (b_1,...,b_n), then change categories = (i-b_1,...,i-b_n)
  # ex: a pixel in change with value i-b_1 means A changed from category i to category b_1
  
  df1 <- data.frame(freq(change)) %>% #count number of pixels in each change category
    filter(is.na(value)==FALSE)       # remove NAs
  
  # -- extend dataframe to include categories A_i did not change to --
  value <- setdiff( i-catsB ,df1[,1]) # A_i did not change to any of these categories
  count <- rep(0,each=length(value))
  df2 <- data.frame(value,count)
  
  all_change <- bind_rows(df1,df2) %>% 
    arrange(desc(value))  # recover categories in raster from order
  return(all_change[,2])
}

# ------------------------------------------------------------------------------------
# Returns matrix M = (M_ij)
# M_ij = # pixels changed from category i in rasterA to category j in rasterB
# ** modified from landcover_comparing_SB.rmd **

change_mtx <- function(rasterA, rasterB){
  
  catsA = unique(rasterA)
  catsB = unique(rasterB)
  
  # set up an empty matrix to fill
  m = length(catsA)
  n = length(catsB)
  mtx <- matrix(NA, m, n) 
  for (i in 1:m) {
    ## fill i-th row of the matrix
    mtx[i,] <- change_i(rasterA,catsA[i], rasterB,catsB)
  } 
  rownames(mtx) <- catsA
  colnames(mtx) <- catsB
  return(mtx)
}


# -----------------------------------------------------------------------------
# ** modified from landcover_comparing_SB.rmd **
change_df <- function(change_mtx, keep_equals=FALSE){
  
  change_tofrom <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("from", "to", "num_pixels"))))
  m <- nrow(change_mtx)
  n <- ncol(change_mtx)
  for(i in 1:m){
    from_name <- rownames(change_mtx)[i]
    for(j in 1:n){
      to_name <- colnames(change_mtx)[j]
      if(from_name == to_name){
        if(keep_equals==TRUE){
          change_tofrom[nrow(change_tofrom)+1,] <- list(from_name,to_name,change_mtx[i,j])    
        } 
      }
      else{
        change_tofrom[nrow(change_tofrom)+1,] <- list(from_name,to_name,change_mtx[i,j])    
      }
    }
  }
  
  change_tofrom <- change_tofrom %>% 
    arrange(desc(num_pixels)) %>% 
    filter(num_pixels!=0)
  
  change_tofrom$from <- as.integer(change_tofrom$from)
  change_tofrom$to <- as.integer(change_tofrom$to)
  
  return(change_tofrom)
}

# -----------------------------------------------------------------------------

raster_change_ij <- function(rasterA,i, rasterB,j){
  change <- rasterA
  change[change!=i] <- NA     # select category A_i
  change[change==i] <- 1
  
  change <- change * rasterB
  change[change!=j] <- NA
  return(change)
}


