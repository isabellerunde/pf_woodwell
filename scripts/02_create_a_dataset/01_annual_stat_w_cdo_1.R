library(narray)
library(chron)
library(lattice)
library(PCICt)
library(stringr)

Sys.setenv('PATH' = "/home/irunde/miniconda3/bin:/home/irunde/miniconda3/condabin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games")

# enter information:: output data  
out_dir <- paste0("~/mybucket/probable_futures/working/remo_rerun1/")
out_name <- "annual_max_tasmax_3day"
cdo_command <- "cdo addc,-273.15 -timmax -runmean,3"

# enter information:: input data  
freq <- "daily" # for directories
var_long_name <- "maximum_temperature" # 
var_short_name <- "tasmax"
rcm <- "remo2015" # remo2015 or regcm


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

for(d in 1:length(domains)){ # loop through domains
  
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
    
    for(t in c(1:length(time_range))){
      
      # 1 GET THRESHOLD YEARS
      thresh <- time_range[t]
      if(thresh == "19712000"){
        threshold_years <- seq(1971, 2000, by = 1) # 21 years centered on threshold year
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
        print(yy)
        
        in_file <- c(Filter(function(x) grepl(paste0(threshold_years[yy], end_fname), x), model_files) )
        print(in_file)
        out_file <- paste0(out_dir, out_fname, threshold_years[yy], ".nc")
        system(command = paste0("cp ", in_file, " /home/irunde/") )
        system(command = paste0(cdo_command, " ", in_file, " ", out_file) )
        system(command = paste0("rm /home/irunde/", basename(in_file) ) )
        
      }
      
      # 3 USE CDO TO CAT AND DELETE OTHER FILES
      files <- list.files(path = out_dir, pattern = out_fname, all.files = TRUE, full.names = TRUE)
      input_files <- paste(files, collapse = ' ') 
      out_file <- paste0(out_dir, out_fname, length(threshold_years), "yrs.nc")
      system(command = paste0("cdo cat ", input_files, " ", out_file) )
      file.remove(files)
      
    }
  }
}

