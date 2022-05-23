library(narray)
library(chron)
library(lattice)
library(PCICt)
library(stringr)

Sys.setenv('PATH' = "/home/irunde/miniconda3/bin:/home/irunde/miniconda3/condabin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games")

# enter information:: output data  
out_dir <- paste0("~/mybucket/probable_futures/working/")
out_name <- "binary_pr_days_ge1mm"
cdo_command <- "cdo gec,1 -mulc,86400"

# enter information:: input data  
freq <- "daily" # for directories
var_long_name <- "precipitation" # 
var_short_name <- "pr"
rcm <- "remo2015" # remo2015 or regcm

# other information
domains <- c("AFR", "AUS", "CAM", "CAS", "EAS", "EUR", "NAM", "SAM", "SEA", "WAS")
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
    
    out_fname <- paste0(out_name,  "_", dom, "_", rcm, "_", short_model_name, "_")
    print(out_fname)
    
    if(dom == "EAS" & short_model_name == "MPI-ESM-LR"){
      years <- seq(1980, 2099, 1)
    }
    years <- seq(1970, 2099, 1)
    
    # 2 LOOP BY YEAR
    for(yy in 1:length(years)){
      print(yy)
      
      in_file <- c(Filter(function(x) grepl(paste0(years[yy], end_fname), x), model_files) )
      print(in_file)
      out_file <- paste0(out_dir, out_fname, years[yy], ".nc")
      system(command = paste0("cp ", in_file, " /home/irunde/") )
      system(command = paste0(cdo_command, " ", in_file, " ", out_file) )
      system(command = paste0("rm /home/irunde/", basename(in_file) ) )
    }
  }
}

