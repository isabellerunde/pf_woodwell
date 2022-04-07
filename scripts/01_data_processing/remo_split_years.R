library(dplyr)
library(tidyr)
library(stringr)

# script to split 5-year REMO2015 files to annual files, use regridded data

# 1 enter variable information 
freq <- "daily" 
var_long_name <- "surface_specific_humidity" 
var_short_name <- "huss"

# 2 set other vectors 
home_dir <- "/home/irunde/climate_data/" 
Sys.setenv('PATH' = "/home/irunde/miniconda3/bin:/home/irunde/miniconda3/condabin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games")
domains <- c("AFR", "AUS", "CAM", "CAS", "EAS", "EUR", "NAM", "SAM", "SEA", "WAS")


for(d in 1:length(domains)){ 
  # 1 loop by domain
  
  dom <- domains[d]
  # get list of files to regrid
  data_dir <- paste0(home_dir, "RCM_regridded_data/REMO2015/", dom, "/", freq, "/", var_long_name) # directory to netcdfs
  files <- list.files(path = data_dir, pattern = var_short_name, all.files = TRUE, full.names = TRUE) 
  
  for(f in 1:length(files)){
    
    in_file <- files[f]
    print(in_file)
    out_file <- paste0(substr(files[f], 1, nchar(files[f]) - 20) )
    system(command = paste0("cdo splityear ", in_file, " ", out_file) )

  }
}

