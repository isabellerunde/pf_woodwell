library(rgdal)
library(raster)
library(ncdf4)
library(dplyr)
library(tidyr)
library(PCICt)
library(sp)         
library(stringr)
library(abind)
library(maps)
library(maptools)
library(mapproj)
library(lattice)
library(rgeos)
library(sf)
library(rnaturalearth)

data_dir <- paste0("/Users/irunde/Desktop/make_weights/for_new_regridded_pr/pr_data/min_extent/") # directory to precip and pet netcdfs
out_dir <- "/Users/irunde/Desktop/make_weights/for_new_regridded_pr/pr_weights/"

grd <- raster("/Users/irunde/Desktop/make_weights/lm_global.nc") # global lm file
# mask <- raster::stack("/Users/irunde/Desktop/make_weights/mosaiced_remo_mosaicing_files_mask.tif")


# function to crop raster to the least extent, minimizing NA
na_rm_raster <- function(x){
  r_isNA <- is.na(as.matrix(x))
  col_notNA <- which(colSums(r_isNA) != nrow(x))
  row_notNA <- which(rowSums(r_isNA) != ncol(x))
  r_extent <- extent(x, row_notNA[1], row_notNA[length(row_notNA)],
                     col_notNA[1], col_notNA[length(col_notNA)])
  r <- crop(x, r_extent)
  return(r)
}
# function to get the center of a REMO domain
centroid <- function(x, y) { # function to compute the centroid of a raster layer
  lon <- median(raster::xFromCol(x))
  lat <- median(rev(raster::yFromRow(x)))
  y <- c(lon, lat)
  return((y))
}
# function to switch -180 to 180 grid to 0 to 360 (need for AFR and EUR)
inv_rotate <- function(r){
  dat <- shift(rotate(shift(r, 180.2)), 179.8)
  return(dat)
}
# function to extend the regional grids to match the global grid
extend_grd <- function(x, y){
  e <- extent(y)
  return(raster::extend(x, e))
}



# load buffered coastlines layer 
buffered <- raster::stack("/Users/irunde/Desktop/make_weights/mosaiced_remo_mask_buffered.nc")
buffered <- shift(rotate(shift(buffered, 180.2)), 179.8)

# CHANGE EUR DOMAIN TO NOT INCLUDE NORTHERN AFRICA
# load Africa shapefile and convert to Spatial* and rasterize
africa_outline <- ne_countries(scale = 50, returnclass = 'sf', continent = "Africa") %>% st_combine() 
africa_outline <- africa_outline %>% st_buffer(1)  %>% st_wrap_dateline()
plot(africa_outline)
spd <- sf::as_Spatial(st_geometry(africa_outline), IDs = as.character(1:length(africa_outline[[1]])))
df <- africa_outline
df$geometry <- NULL
df <- as.data.frame(df)
spd <- sp::SpatialPolygonsDataFrame(spd, data = df)
r <- raster(ncol = 1800, nrow = 901)
ras <- rasterize(x = spd, y = r)
ras <- shift(rotate(shift(ras, 180)), 179.9)
extent(ras) <- extent(grd)
ras <- extend_grd(x = ras, y = grd)

rm(africa_outline, spd, r, df)


my_files <- list.files(path = paste0(data_dir) , pattern = paste0("*.tif"), full.names = TRUE)
my_files


# load each domain using the file containing exactly the domain name
afr <- raster((Filter(function(x) grepl("AFR", x), my_files)))
aus <- raster((Filter(function(x) grepl("AUS", x), my_files)))
cam <- raster((Filter(function(x) grepl("CAM", x), my_files)))
cas <- raster((Filter(function(x) grepl("CAS", x), my_files)))
eas <- raster((Filter(function(x) grepl("EAS", x), my_files)))
eur <- raster((Filter(function(x) grepl("EUR", x), my_files)))
nam <- raster((Filter(function(x) grepl("NAM", x), my_files)))
sam <- raster((Filter(function(x) grepl("SAM", x), my_files)))
sea <- raster((Filter(function(x) grepl("SEA", x), my_files)))
was <- raster((Filter(function(x) grepl("WAS", x), my_files)))


domains <- c("AUS", "CAM","CAS","EAS", "NAM", "SAM", "SEA", "WAS", "AFR",  "EUR")
my_rasters <- as.list(aus, cam, cas, eas, nam, sam, sea, was, afr, eur)
names(my_rasters) <- domains


# create a raster with the number of regional domains overlapping at each cell in the global domain
# also create distance rasters - the distance of each cell from the domain center
domain_extents <- list()
distance_rasters <- list()

for(i in 1:length(my_rasters)){
  rast <- na_rm_raster(my_rasters[[i]]) # load raster and get rid of empty rows / columns
  
  d <- 1 / ( (distanceFromPoints(grd, centroid(rast))) / 1000)^10  # compute inverse distance from each cell to domain center in km
  
  if(i > 8){
    dat <- inv_rotate(rast)  # rotate afr and eur
    dat <- extend_grd(x = dat, y = grd)
    
  } else {
    dat <- extend_grd(x = rast, y = grd)
  }
  
  
  # create a binary raster where the domain extent = 1 
  reclass <- raster::calc(dat, fun = function(x){ifelse(is.na(x) == "TRUE", 0, 1) })
  extent(reclass) <- extent(grd)
  
  if(i == 10){
    
    # create mask in area EUR domain overlaps with Africa
    ras[is.na(ras[])] <- 0 
    EUR_mask <- reclass - ras
    plot(EUR_mask)
    
    reclass <- raster::calc(EUR_mask, fun = function(x){ifelse(x == 1, 1, 0) })
    plot(reclass)
  }
  
  print(ncol(reclass))
  domain_extents[[i]] <- reclass
  
  # crop distance raster to domain extent 
  d <- d * reclass
  distance_rasters[[i]] <- d
  
  rm(d, reclass, dat, rast)
}

domain_extents <- raster::stack(domain_extents)
domain_extents <- domain_extents * buffered


# CONTINUE
sum_raster_extents <- raster::calc(domain_extents, fun = sum) # create a global raster of number of overlapping domians
plot(rotate(sum_raster_extents))

distance_rasters <- raster::stack(distance_rasters)
total_distances <- raster::calc(distance_rasters, fun = sum) # create a global raster of total inverse distances from domain centers

plot(total_distances)

weighted_var <- list()
weights <- list()

for(i in 1:length(my_rasters)){
  print(i)
  rast <- na_rm_raster(my_rasters[[i]]) # load raster and get rid of empty rows / columns
  
  if(i > 8){
    dat <- inv_rotate(rast)  # rotate afr and eur
    dat <- extend_grd(x = dat, y = grd)
    ncol(dat)
    
  } else {
    dat <- extend_grd(x = rast, y = grd)
  }
  
  # create a binary raster where the domain extent = 1 
  reclass <- raster::calc(dat, fun = function(x){ifelse(is.na(x) == "TRUE", 0, 1) })
  extent(reclass) <- extent(grd)
  
  if(i == 10){
    reclass <- raster::calc(EUR_mask, fun = function(x){ifelse(x == 1, 1, 0) })
    plot(reclass)
  }
  
  wght <- sum_raster_extents * reclass
  
  # set the raster to 1 where it is the only domain 
  wght1 <- raster::calc(wght, fun = function(x){ifelse(x == 1, 1, 0) })
  
  # set to 1 where there are more than 1 domains
  wght2 <- raster::calc(wght, fun = function(x){ifelse(x > 1, 1, 0) })
  
  # divide inverse distance by sum inverse distances from centers of all overlapping domains
  # multiply so output is only where there are 2 or more domains
  dist_rast <- ( distance_rasters[[i]] / total_distances ) * wght2
  
  # add wght1 where rast is the only domain
  my_wght <- dist_rast + wght1
  
  out_dat <- my_wght * dat 
  
  weighted_var[[i]] <- out_dat
  weights[[i]] <- my_wght
  
}


weighted_var$fun <- sum # function for mosaic
rast.mosaic <- do.call(mosaic, weighted_var)

mask <- raster::calc(sum_raster_extents, fun = function(x){ifelse(x == 0, NA, 1) })
rast.mosaic <- rast.mosaic * mask # set non-domain data, with weights = 0 to NA
rast.mosaic <- rotate(rast.mosaic) # convert to -180 to 180 
plot(rast.mosaic)
plot(raster::stack(weights))


weights <- raster::stack(weights)
domains <- c("AUS", "CAM", "CAS","EAS", "NAM", "SAM", "SEA", "WAS", "AFR",  "EUR")

for(i in 1:10){
  writeRaster(weights[[i]], 
              paste0(out_dir, "weight_", domains[i], ".tif"), 
              format = "GTiff")
  
}


x <- round(sum(weights), 0)
plot(x)
writeRaster(x, 
            paste0(out_dir, "mask.tif"), 
            format = "GTiff")

