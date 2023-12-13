###########################################################################
### Mask Ocean Color Datasets using STRM Bathymetry and Topography data ###
###########################################################################

rm(list = ls())

library(raster)
library(dplyr)
library(maps)
library(readr)
library(ncdf4)
library(filesstrings)

select = dplyr::select

source("scripts/eds_functions.R")

#################################################################
### load STRM Bathymetry and Topography data                  ###
### Shuttle Radar Topography Mission (SRTM)                   ###
### Global Bathymetry and Topography at 15 Arc Sec: SRTM15+   ###
### https://doi.org/10.1029/2019EA000658                      ###
#################################################################
dir = "M:/Environmental_Data_Summary/Data_Download/"
dir = "/mnt/ldrive/ktanaka/Environmental_Data_Summary/"
dir = paste0(file.path(dirname(path.expand('~')),'Desktop'), "/Environmental_Data_Summary/") # Local without VPNs

# These files are generated from "rotate_STRM.R"
bathy_360 = raster("M:/Environmental_Data_Summary/Data_Download/Bathymetry_SRTMPlus_v1_15s/Bathymetry_SRTMPlus_v1_15s_all_units_Long360.nc")
bathy_180 = raster("M:/Environmental_Data_Summary/Data_Download/Bathymetry_SRTMPlus_v1_15s/Bathymetry_SRTMPlus_v1_15s_all_units.nc")

bathy_360 = raster("M:/Environmental_Data_Summary/Data_Download/Bathymetry_ETOPO_2022_v1_15s/Bathymetry_ETOPO_2022_v1_15s_all_units_Long360.nc")
bathy_180 = raster("M:/Environmental_Data_Summary/Data_Download/Bathymetry_ETOPO_2022_v1_15s/Bathymetry_ETOPO_2022_v1_15s_all_units.nc")

##############################################
### list ocean color datasets need masking ###
############################################## 
oc = read_csv("data/EDS_parameters.csv")

oc = oc %>%
  dplyr::select("Dataset", "Download", "Mask") %>%
  filter(Mask == T & Download == "YES")

oc

#################################
### mask ocean color datasets ###
################################# 
var_list <- basename(list.dirs(dir, recursive = F))
var_list <- var_list[var_list %in% oc$Dataset]; var_list

start_time <- Sys.time()

cat(sprintf("Running through %d OC datasets:\n", length(var_list)))
cat(paste(var_list, collapse = "\n"))

for (i in 1:length(var_list)) {
  
  # i = 1
  
  var_name = var_list[i]; var_name
  
  unmask_path = paste0(dir, var_name, "/Unit_Level_Data", "/unmasked/")
  mask_path = paste0(dir, var_name, "/Unit_Level_Data/")
  
  if (!dir.exists(unmask_path)) {
    
    cat(paste0("...Masking ", var_list[i], "...\n")) 
    dir.create(unmask_path)
    
  } else {
    
    cat(paste0("Masking already done for ", var_name, "...Skipping this unit...\n")) 
    next
    
  }
  
  unit_nc_files = list.files(path = mask_path, pattern = ".nc"); unit_nc_files
  
  # copy unmasked files to separate folder
  if(all(!file.exists(paste0(unmask_path, unit_nc_files))) == T){
    
    setwd(mask_path)
    current = getwd()
    new = unmask_path
    list.of.files = list.files(current, ".nc$")
    file.move(list.of.files, new)
    setwd(paste0("C:/Users/", Sys.info()[7], "/env_data_summary"))
    
  } else {
    
    next
    
  }
  
  for (unit_i in 1:length(unit_nc_files)) {
    
    # unit_i = 1
    
    # Get file names
    unit_nc_file_name = unit_nc_files[unit_i]
    unit_masked_nc_file_name = paste0(tools::file_path_sans_ext(unit_nc_file_name), "_30m_5pct_masked.nc")
    
    if(!file.exists(paste0(mask_path, unit_masked_nc_file_name))){
      
      print_isl = strsplit(unit_nc_files[unit_i], "_")[[1]][1]
      cat(paste0("Masking ", print_isl, "...\n")) 
      cat(paste0(print_isl, ": Reading data file...\n")) 
      
      var_df = stack(paste0(unmask_path,unit_nc_file_name))
      
      if (var_df@extent@xmin < 0) {
        
        strm = bathy_180
        
      } else {
        
        strm = bathy_360
        
      }
      
      #Crop Rotated STRM15, convert to SPDF
      cat(paste0(print_isl,": Cropping STRM file...\n"))
      cropped_strm = crop(strm, extent(var_df));# beepr::beep(2)
      spatial_strm = data.frame(rasterToPoints(cropped_strm))
      
      if (nrow(spatial_strm) == 0) {
        
        cat("No STRM data available for masking. Skipping this unit...\n")
        next
        
      }
      
      coordinates(spatial_strm) <- ~x+y
      crs(spatial_strm) = crs(var_df)
      
      # Build Mask 
      cat(paste0(print_isl,": Building depth mask...\n")) 
      depth_mask = rasterize(x = spatial_strm, y = var_df[[1]], vals = "layer ", fun = maskfun)$layer
      
      # Apply Mask
      cat(paste0(print_isl,": Applying depth mask...\n")) 
      var_df_masked = mask(x = var_df, mask = depth_mask)
      
      var_df_masked = readAll(var_df_masked) #maybe optional 
      
      # grab var name and unit from unmasked nc file
      nc = nc_open(paste0(unmask_path, unit_nc_file_name))
      
      variable_name = as.character(nc$var[[1]][2])
      variable_unit = as.character(nc$var[[1]][8])
      
      x_name = nc$dim$longitude$name
      y_name = nc$dim$latitude$name
      
      z_name = nc$dim$time$name
      z_unit = nc$dim$time$units
      
      nc_close(nc)
      
      # write out masked nc.file
      cat(paste0(print_isl,": Writing out masked values...\n")) 
      
      writeRaster(var_df_masked, 
                  paste0(mask_path, unit_masked_nc_file_name), 
                  overwrite = T,
                  varname = variable_name, 
                  varunit = variable_unit, 
                  xname = x_name, 
                  yname = y_name, 
                  zname = z_name, 
                  zunit = z_unit)
      
      cat(paste0(print_isl,": Done...\n")) 
      
      names <- names(var_df_masked)
      names = gsub("X", "", names)
      names = substr(names, 1, 10)
      
      # fix time step labels
      nc = nc_open(paste0(mask_path, unit_masked_nc_file_name), write = T)
      zvals = lubridate::parse_date_time(names, orders = 'y.m.d', tz = 'UTC')
      zvals = as.integer(zvals)
      ncdf4::ncvar_put(nc, 'time', zvals)
      nc_close(nc)
      
    } else {
      
      cat(paste0(unit_masked_nc_file_name, " already exists. Skipping...\n")) 
      
    }
    
  }# unit loop
  
  cat(paste0("All units complete for ", var_list[i], "...\n"))
  
} #param loop

end_time <- Sys.time()
end_time - start_time
