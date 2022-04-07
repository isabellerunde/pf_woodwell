library(narray)
library(ncdf4)
library(abind)
library(chron)
library(lattice)
library(raster)

# script to compute the ensemble 5th, 10th, 50th, 90th, 95th percentile, mean, REMO mean, RegCM mean

# 3/31 - error at v=7 - RERUN 

# enter variable name and units 
var_name <- c("annual_sum_pr",
              "annual_sum_dry_days",
              "annual_sum_wet_days",
              "annual_max_consecutive_dry_days",
              "annual_max_consecutive_wet_days",
              "annual_sum_dry_hot_days",
              "annual_max_monsum_pr_month",
              "annual_sum_snowy_days",
              "annual_max_pr_30day",
              "annual_max_pr_90day",
              "annual_max_pr",
              "annual_max_pr_3day",
              "annual_max_pr_10day")

# other information for script to run, no need to edit unless directories change
model_table <- read.csv("~/mybucket/probable_futures/regcm_remo_model_info.csv")
thresholds <- c("1C", "15C", "2C", "25C", "3C", "19712000")


for(v in 8:length(var_name)){
  
  data_dir <- paste0("~/climate_data/Probable_futures/water_module/rcm_data/", var_name[v], "/annual_by_time_span/")
  out_dir <- paste0("~/climate_data/Probable_futures/water_module/rcm_data/", var_name[v], "/summarize_model_ensemble/")
  temp_out_dir <- paste0("~/mybucket/probable_futures/working/summarized_data/")
  system(command = paste0("mkdir ", out_dir) )

    for(d in c(1, 3:10)){
      
      # loop through domains and generate a vector of REMO and RegCM models used in that domain
      dom <- unique(model_table$domains)[d]
      domain_models <- na.omit( c( as.character( model_table$remo_model_names_speadsheet[ which(dom == model_table$domains )] ), 
                                   as.character( model_table$reg_model_names_speadsheet[ which(dom == model_table$domains )] ) ) )
      print(paste0(dom, " with ", length(domain_models), " models"))
      
      # load file to mask to min REMO RegCM extent 
      domain_mask_file <- paste0("~/mybucket/probable_futures/water_module/regcm_remo_output/scripts/domain_extent_mask/min_regcm_remo_extent_", dom, ".nc")
      domain_mask <- raster::raster(domain_mask_file)
      # domain_mask <- t(apply(as.matrix(domain_mask), 2, rev))
      rm(domain_mask_file)
      
      for(t in 1:length(thresholds)){
        
        # loop through thresholds
        thresh <- thresholds[t]
        remo_stack <- raster::stack()
        regcm_stack <- raster::stack()
        
        for (m in 1:length(domain_models)){
          
          mod <- domain_models[m]
          
          if(m < 4){ 
            rcm <- "remo2015"
            in_file <- list.files(data_dir, paste0(var_name[v], "_",  dom, "_", rcm, "_", mod, "_", thresh, "_*"), full.names = TRUE)
            dat <- raster::stack(in_file)
            dat <- dat * domain_mask
            remo_stack <- raster::stack(remo_stack, dat)
          } else{ 
            rcm <- "regcm"
            in_file <- list.files(data_dir, paste0(var_name[v], "_",  dom, "_", rcm, "_", mod, "_", thresh, "_*"), full.names = TRUE)
            dat <- raster::stack(in_file)
            dat <- dat * domain_mask
            regcm_stack <- raster::stack(regcm_stack, dat)
          }
          
          rm(dat, in_file)
        }
        
        # compute and output model mean for REMO2015 and RegCM ensembles
        
        remo_mean <- raster::calc(remo_stack, fun = mean)
        writeRaster(remo_mean, overwrite = TRUE, 
                    paste0(temp_out_dir, var_name[v], "_",  dom, "_", thresh, "_remo2015_mean.nc"), format = "CDF")
        
        if(dom != "CAS"){
          regcm_mean <- raster::calc(regcm_stack, fun = mean)
          writeRaster(regcm_mean, overwrite = TRUE, 
                      paste0(temp_out_dir, var_name[v], "_",  dom, "_", thresh, "_regcm_mean.nc"), format = "CDF")
        }
        
        all_models <- raster::stack(remo_stack, regcm_stack)
        rm(regcm_stack, remo_stack, remo_mean, regcm_mean)
        print(paste0(var_name[v], " ", dom, " - ", thresh, " - ", dim(all_models)[3], " years"))
        all_models_num_years <- dim(all_models)[3]
        
        # 1 GET 5TH PERCENTILE VALUE
        all_models_5pctl <- raster::calc(all_models, fun = function(x){
          sort(x, decreasing = FALSE)[round( (0.05 * all_models_num_years) , 0)] })
        print(paste0("5th - ", round(max(all_models_5pctl[], na.rm = T), 1)))
        writeRaster(all_models_5pctl, paste0(temp_out_dir, var_name[v], "_",  dom, "_", thresh, "_5pctl.nc"), overwrite = TRUE, format = "CDF")
        rm(all_models_5pctl)
        
        # 2 GET 10TH PERCENTILE VALUE
        all_models_10pctl <- raster::calc(all_models, fun = function(x){
          sort(x, decreasing = FALSE)[round( (0.1 * all_models_num_years) , 0)] })
        print(paste0("10th - ", round(max(all_models_10pctl[], na.rm = T), 1)))
        writeRaster(all_models_10pctl, paste0(temp_out_dir, var_name[v], "_",  dom, "_", thresh, "_10pctl.nc"), overwrite = TRUE, format = "CDF")
        rm(all_models_10pctl)    
        
        # 3 GET 50TH PERCENTILE VALUE
        all_models_50pctl <- raster::calc(all_models, fun = function(x){
          sort(x, decreasing = FALSE)[round( (0.5 * all_models_num_years) , 0)] })
        print(paste0("50th - ", round(max(all_models_50pctl[], na.rm = T), 1)))
        writeRaster(all_models_50pctl, paste0(temp_out_dir, var_name[v], "_",  dom, "_", thresh, "_50pctl.nc"), overwrite = TRUE, format = "CDF")
        rm(all_models_50pctl)          
        
        # 4 GET 90TH PERCENTILE VALUE
        all_models_90pctl <- raster::calc(all_models, fun = function(x){
          sort(x, decreasing = FALSE)[round( (0.9 * all_models_num_years) , 0)] })
        print(paste0("90th - ", round(max(all_models_90pctl[], na.rm = T), 1)))
        writeRaster(all_models_90pctl, paste0(temp_out_dir, var_name[v], "_",  dom, "_", thresh, "_90pctl.nc"), overwrite = TRUE, format = "CDF")
        rm(all_models_90pctl)       
        
        # 5 GET 95TH PERCENTILE VALUE
        all_models_95pctl <- raster::calc(all_models, fun = function(x){
          sort(x, decreasing = FALSE)[round( (0.95 * all_models_num_years) , 0)] })
        print(paste0("95th - ", round(max(all_models_95pctl[], na.rm = T), 1)))
        writeRaster(all_models_95pctl, paste0(temp_out_dir, var_name[v], "_",  dom, "_", thresh, "_95pctl.nc"), overwrite = TRUE, format = "CDF")
        rm(all_models_95pctl)      
        
        # 5 GET MEAN VALUE
        all_models_mean <- raster::calc(all_models, fun = mean)
        print(paste0("mean - ", round(max(all_models_mean[], na.rm = T), 1)))
        writeRaster(all_models_mean, paste0(temp_out_dir, var_name[v], "_",  dom, "_", thresh, "_mean.nc"), overwrite = TRUE, format = "CDF")
        rm(all_models_mean, all_models)  
        
    }
    }
  
  system(command = paste0("cp ", temp_out_dir, "* ", out_dir) )
  system(command = paste0("cp ", temp_out_dir, "* ", out_dir) )
  system(command = paste0("rm ", temp_out_dir, "* ") )
  
  
  
}

