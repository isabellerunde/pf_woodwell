library(raster)


domains <- c("AFR", "AUS", "CAM", "EAS", "EUR", "NAM", "SAM", "SEA", "WAS", "CAS")

remo_dir <- c("/Users/irunde/Desktop/make_weights/for_new_regridded_pr/pr_data/remo/")
remo_files <- list.files(path = paste0(remo_dir) , pattern = paste0("*.nc"), full.names = TRUE)
regcm_dir <- c("/Users/irunde/Desktop/make_weights/for_new_regridded_pr/pr_data/regcm/")
regcm_files <- list.files(path = paste0(regcm_dir) , pattern = paste0("*.nc"), full.names = TRUE)

out_dir <- c("/Users/irunde/Desktop/make_weights/for_new_regridded_pr/pr_data/min_extent/")

for(d in 1:length(domains)){
  
  d_remo <- raster(Filter(function(x) grepl(paste0(domains[d]), x), remo_files))
  d_regcm <- raster(Filter(function(x) grepl(paste0(domains[d]), x), regcm_files))
  
  d_remo_extent <- raster::calc(d_remo[[1]], fun = function(x){ifelse(x >-1, 1, NA) })
  d_regcm_extent <- raster::calc(d_regcm[[1]], fun = function(x){ifelse(x >-1, 1, NA) })
  
  writeRaster(d_remo_extent * d_regcm_extent, format = "CDF", 
              paste0(out_dir, "min_regcm_remo_extent_", domains[d], ".nc"))
  
}
