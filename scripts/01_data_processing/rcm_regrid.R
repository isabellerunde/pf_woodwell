library(dplyr)
library(tidyr)
library(stringr)

# script to regrid RCM data using cdo bilinear interpolation

# 1 enter variable information 
freq <- "daily" 
var_long_name <- "surface_air_pressure" 
var_short_name <- "ps"
rcm <- "remo2015" # remo2015 or regcm

# 2 enter directory and environment information 
home_dir <- "~/climate_data/" 
Sys.setenv('PATH' = "/home/irunde/miniconda3/bin:/home/irunde/miniconda3/condabin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games")



# set vectors based on rcm of interest
rcm_dir <- ifelse(rcm == "remo2015", "REMO2015", "CORDEX_22")
domains <- c("AFR", "AUS", "CAM", "CAS", "EAS", "EUR", "NAM", "SAM", "SEA", "WAS")
if(rcm == "regcm"){
  domains <- domains[!domains == "CAS"]
}

for(d in 1:length(domains)){ 
  # 1 loop by domain
  
  dom <- domains[d]
  dom_grid <- paste0("/home/irunde/climate_data/RCM_raw_data/REMO2015/lm_files/lm_", dom, ".nc")
  data_dir <- paste0(home_dir, "RCM_raw_data/", rcm_dir, "/", dom, "/", freq, "_data/", var_long_name) # directory to netcdfs
  data_dir <- paste0(home_dir, "RCM_raw_data/", rcm_dir, "/", dom, "/", freq, "_data/surface_pressure") # directory to netcdfs
  
  if(rcm == "regcm" & dom == "EUR"){ # regcm EUR only at 11 km, switch data_dir when necessary 
    data_dir <- paste0(home_dir, "RCM_raw_data/CORDEX_11/", dom, "/", freq, "_data/", var_long_name) # directory to netcdfs
  }
  out_dir <- paste0(home_dir, "RCM_regridded_data/", rcm_dir, "/", dom, "/", freq, "/", var_long_name) 
  # remove existing files
  system(command = paste0("rm ", out_dir, "/*") )
  
  # get list of files to regrid
  files <- list.files(path = data_dir, pattern = var_short_name, all.files = TRUE, full.names = TRUE) 
  files <- c(Filter(function(x) grepl("historical", x), files), Filter(function(x) grepl("rcp85", x), files))
  files <- c(Filter(function(x) grepl("day", x), files))

  for(f in 1:length(files)){
    
    in_file <- paste(files[f]) 
    out_file <- paste0(out_dir, "/", basename(files[f]))
    print(out_file)
    
    system(command = paste0("cdo -P 16 remapbil,", dom_grid, " ", in_file, " ", out_file) )
    
  }
}

