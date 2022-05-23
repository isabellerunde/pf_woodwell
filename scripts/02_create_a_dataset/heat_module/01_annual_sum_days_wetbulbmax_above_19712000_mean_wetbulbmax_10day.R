library(lattice)
library(stringr)
library(raster)
library(ncdf4)
library(abind)

Sys.setenv('PATH' = "/home/irunde/miniconda3/bin:/home/irunde/miniconda3/condabin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games")

# enter information:: output data  
out_dir <- paste0("~/mybucket/probable_futures/working/remo_rerun1/")
out_name <- "01_annual_sum_days_wetbulbmax_above_19712000_mean_wetbulbmax_10day"

# enter information:: input data  
freq <- "daily" # for directories
var_long_name <- "maximum_wetbulb_temperature" 
var_short_name <- "wetbulbmax"
rcm <- "remo2015" # remo2015 or regcm
data_dir_hist <- "~/climate_data/Probable_futures/heat_module/rcm_data/annual_mean_wetbulbmax_10day/annual_by_time_span/"

# other information
domains <- c("AFR", "AUS", "CAM", "CAS", "EAS", "EUR", "NAM", "SAM", "SEA", "WAS")
time_range <- c("1C", "15C", "2C", "25C", "3C", "19712000")
thresholds <- seq(1, 3, by = 0.5)
col_nums <- c(2:6)
thresh_table <- read.csv("/home/irunde/mybucket/probable_futures/CMIP5_model_temp_thresholds.csv")
model_table <- read.csv("~/mybucket/probable_futures/regcm_remo_model_info.csv")

# set vectors based on rcm of interest
rcm_dir <- ifelse(rcm == "remo2015", "REMO2015", "CORDEX_22")
end_fname <- ifelse(rcm == "remo2015", ".nc", "0101")
domains <- c("AFR", "AUS", "CAM", "CAS", "EAS", "EUR", "NAM", "SAM", "SEA", "WAS")
if(rcm == "regcm"){
  domains <- domains[!domains == "CAS"]
}


for(d in 5:length(domains)){ # loop through domains
  
  dom <- domains[d]
  data_dir <- paste0("~/climate_data/RCM_regridded_data/", rcm_dir, "/",  dom, "/daily/", var_long_name, "/")
  file_name <- paste0("+", var_short_name, "_",  dom, "_*")
  nc_files <- list.files(path = data_dir, pattern = file_name, all.files = TRUE, full.names = TRUE) # create list of REMO2015 rotated files
  
  # loop through domains and generate a vector of models used in that domain
  if(rcm == "remo2015"){
    models <- c(as.character( model_table$remo_model_names_speadsheet[ which(dom == model_table$domains )]))
  }else{
    models <- c(as.character(model_table$reg_model_names_speadsheet[ which(dom == model_table$domains )]))
  }
  
  
  for(m in 1:length(models)){ # loop through models
    
    short_model_name <- models[m]
    model_files <- c(Filter(function(x) grepl(short_model_name, x), nc_files) )
    
    # compute the 1971-2000 mean of the annual mean of the hottest 10 days
    in_file_hist <- paste0(data_dir_hist, "annual_mean_wetbulbmax_10day_", dom, "_", rcm, "_", short_model_name, "_19712000_30yrs.nc")
    out_file_hist <- paste0(out_dir, "mean_annual_mean_wetbulbmax_10day_", dom, "_", rcm, "_", short_model_name, "_19712000.nc")
    system(command = paste0("cdo timmean ", in_file_hist, " ", out_file_hist) )
    system(command = paste0("cp ", out_file_hist, " /home/irunde/") )
    
    
    for(t in c(1:length(time_range))){
      
      # 1 GET THRESHOLD YEARS
      thresh <- time_range[t]
      if(thresh == "19712000"){
        threshold_years <- seq(1971, 2000, by = 1) # 21 years centered on threshold year
        if(rcm == "regcm" & dom == "EAS"){
          threshold_years <- seq(1980, 2000, by = 1) # 21 years centered on threshold year
        }
      } else{
        # get the year for the model surpassing the threshold and derive the years in the threshold
        yr_thresh <- thresh_table[thresh_table$Model == short_model_name, col_nums[t]] # threshold year
        print(paste0(dom, ", ", short_model_name, " : ", thresh, " : ", yr_thresh))
        threshold_years <- seq(yr_thresh - 10, yr_thresh + 10, by = 1) # 21 years centered on threshold year
      }
      
      out_fname <- paste0(out_name,  "_", dom, "_", rcm, "_", short_model_name, "_", thresh, "_")
      print(out_fname)
      
      # 2 LOOP BY YEAR
      for(yy in 1:length(threshold_years)){
        
        # get input file for year of interest and load to read number of days
        in_file <- c(Filter(function(x) grepl(paste0(threshold_years[yy], end_fname), x), model_files) )
        print(in_file)
        nc <- nc_open(in_file, readunlim = FALSE)
        t_days <- as.numeric(nc$dim$time$len)
        print(paste0(short_model_name, " - ", thresholds[t], " and num days = ", t_days))
        nc_close(nc)
        
        out_file <- paste0(out_dir, out_fname, threshold_years[yy], ".nc")
        
        system(command = paste0("cp ", in_file, " /home/irunde/") )
        # 1. subtract 1971-2000 mean - daily temp
        # 2. count number of days less than 0 (1971-2000 mean < daily temp)
        # 3. sum days that meet condition 
        # 4. divide by total number of days and multiply for 365 to address varying calendars 
        system(command = paste0("cdo mulc,365 -divc,", t_days, " -timsum -lec,0 -sub ", out_file_hist, " ", in_file, " ", out_file) )
        system(command = paste0("rm /home/irunde/", basename(in_file) ) )
        rm(nc, t_days)
        
      }
     
      # 3 USE CDO TO CAT AND DELETE OTHER FILES
      files <- list.files(path = out_dir, pattern = out_fname, all.files = TRUE, full.names = TRUE)
      input_files <- paste(files, collapse = ' ') 
      out_file <- paste0(out_dir, out_fname, length(threshold_years), "yrs.nc")
      system(command = paste0("cdo cat ", input_files, " ", out_file) )
      file.remove(files)
      
    }
    
    file.remove(out_file_hist)
    
  }
}


