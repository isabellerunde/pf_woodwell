library(narray)
library(chron)
library(lattice)
library(PCICt)
library(stringr)
library(ncdf4)

Sys.setenv('PATH' = "/home/irunde/miniconda3/bin:/home/irunde/miniconda3/condabin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games")

# enter information:: output data  
out_dir <- paste0("~/mybucket/probable_futures/working/remo_rerun4/")
out_name <- "wetbulbmax_3day_19712000_1in10"

# enter information:: input data  
data_dir <- paste0("~/climate_data/Probable_futures/heat_module/rcm_data/annual_max_wetbulbmax_3day/annual_by_time_span/")
var_short_name <- "annual_max_wetbulbmax_3day"
rcm <- "remo2015" # remo2015 or regcm

# other information
time_range <- "19712000"
model_table <- read.csv("~/mybucket/probable_futures/regcm_remo_model_info.csv")
domains <- c("AFR", "AUS", "CAM", "CAS", "EAS", "EUR", "NAM", "SAM", "SEA", "WAS")
if(rcm == "regcm"){
  domains <- domains[!domains == "CAS"]
}

for(d in 1:length(domains)){ # loop through domains
  
  dom <- domains[d]
  
  # loop through domains and generate a vector of models used in that domain
  if(rcm == "remo2015"){
    models <- c(as.character( model_table$remo_model_names_speadsheet[ which(dom == model_table$domains )]))
  }else{
    models <- c(as.character(model_table$reg_model_names_speadsheet[ which(dom == model_table$domains )]))
  }
  
  for(m in 1:length(models)){ # loop through models
    
    short_model_name <- models[m]
    
    # 1 LOAD DATA 
    print("load data")
    nc <- nc_open(paste0(data_dir, var_short_name, "_", dom, "_", rcm, "_", short_model_name, "_", time_range, "_30yrs.nc"), readunlim = FALSE) # open netcdf file
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    dat <- ncvar_get(nc, "wetbulbmax") # retrieve variable of interest
    ncdf4::nc_close(nc)
    rm(nc)
    
    # 2 TAKE THE MEAN OF THE TOP 3 VALUES 
    y <- array(dim = c(dim(dat)[1], dim(dat)[2]))
    for(i in 1:(dim(dat)[1])){
      for(j in 1:(dim(dat)[2])){
        m <- mean(head((sort(dat[i,j,], decreasing = TRUE)), 3))
        y[i, j] <- m
        m <- NULL
      }
    }

    
    # WRITE OUT NETCDF
    ncfname <- paste0(out_dir, out_name, "_", dom, "_", rcm, "_", short_model_name, ".nc")
    londim <- ncdim_def("lon", "degrees_east", as.double(lon)) 
    latdim <- ncdim_def("lat", "degrees_north", as.double(lat)) 
    fillvalue <- 1e32
    dname <- paste0("temperature of the 1-10 year 3-day wetbulbmax heat wave in ", time_range)
    var1 <- ncvar_def("temp", "temp", list(londim, latdim), fillvalue, dname, prec = "single") # define variables
    ncout <- nc_create(filename = ncfname, vars = var1) # create netCDF file and put array
    ncvar_put(ncout, var1, y) # put variables
    ncatt_put(ncout, "lon", "axis", "X") # put additional attributes into dimension and data variables
    ncatt_put(ncout,"lat", "axis", "Y")
    ncout # summary of the created file
    test <- nc_open(ncfname)
    ncdf4::nc_close(test)
    rm(y, dat)

  }
}

