#Metric functions

#Install and import packages
#Install and import libraries

pkgs = c(
  "sf",
  "sp",
  "raster",
  "rgdal",
  
  "dplyr",
  "units",
  "purrr",
  "lwgeom",
  "gdalUtils",
  "fasterize",
  "xlsx",
  "nngeo",
  'installr',
  'RSAGA'
)
to_install = !pkgs %in% installed.packages()
if (any(to_install)) {
  install.packages(pkgs[to_install])
}

# load libraries and set working directory
library(sp)
library(sf)
library(raster)
#library(gdalUtils) #doesn't exist anymore???
library(dplyr)
library(units)
library(purrr)
library(xlsx)
library (nngeo)
library(installr)
#library(RSAGA) #doesn't exist anymore??
library(terra)
library(exactextractr)
library(landscapemetrics)
#library(whitebox)
setwd = 'C:/Meghana/Belgique'


LateralWidthFunction <- function(rives, voronoi){
  for (i in 1:nrow(rive)){
    rive = rives[i,]
    voronoi_lines <- st_cast(voronoi , to = 'LINESTRING')
    voronoi_lines <- st_intersection(voronoi , rive)
    width_UREC = st_length(voronoi_lines)
    median_width = median(width_UREC)
    rive$width = median_width
    st_write(rive, 'C:/Meghana/Belgique/traitements/results/UREC_MaxCam1.shp', delete_layer = T)
  }

}


continuity_metric <- function(UREC_full, path_sampling, raster_UT, classe_UT) {
  #var1 = shapefile of UREC (UREC_merge) as sf data.frame
  #raster_UT = raster of Utilisation du territoire masked for values we are interested in. e.g.  <Forest> (need to do this step beforehand , in ArcMap sometimes)
  # var 3  = character string of name of colum referencing the specific UT selection eg. "Forest" 
  list_files_sampling_rive = list.files(path = path_sampling, pattern = '*.shp', full.names = T)
  new_col_name = paste0('continuity_', classe_UT)
  
  for (r in 1:nrow(UREC_full)) {
    shp = UREC_full[r,]
    for (i in 1:length(list_files_sampling_rive)) {
      rive = st_read(list_files_sampling_rive[i])
      if (rive$id[1] == shp$id){
        
        UREC_vect = st_read(list_files_sampling_rive[i])
        UREC_vect = st_transform(UREC_vect, crs(raster_UT))
        
        ut19_clip = terra::crop(raster_UT, UREC_vect) #Reduce the extent of the ESA raster to the extent of UREC
        UREC_raster = terra::rasterize(UREC_vect, ut19_clip) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
        ut_mask = terra::mask(ut19_clip, UREC_raster)  #create mask on raster with UREC shapefile
        
        
        
        #Extract the surface of forested/ grassland/urban/agriculture pixels in each UREC to get other metric
        UREC_vect$surface_full = exactextractr::exact_extract(ut_mask, UREC_vect, 'count') *
          10 #multiply sum by area of each pixel (meed to do this with sf object!)
        
        UREC_vect$area = as.numeric(st_area(UREC_vect)) #calculate the area of each sampling unit in the UREC
        UREC_vect$prop = UREC_vect$surface_full / UREC_vect$area # get the ratio of forested area in each sampling unit by the area of the sampling unit
        
        continuity = median(UREC_vect$prop) #create a variable of the median for continuity of forested areas
        
      }
      UREC_full[r, new_col_name] = continuity
    }
  }
  return (UREC_full)
}




PDV_fragementation_metric_function <- function(UREC_full, raster_file, col_name) {
  #UREC_full = shapefile of UREC (UREC_merge) as sf data.frame
  #raster = raster of Utilisation du territoire masked for values we are interested in. e.g.  <Forest> (need to do this step beforehand , in ArcMap sometimes)
  # cil_name  = character string of name of colum referencing the specific UT selection eg. "Forest" 
  area_new_col = paste0('area_', col_name)
  pd_new_col = paste0('pd_', col_name)
  raster_file[!is.na(raster_file)] <-1
 
  
  for (i in 1:nrow(UREC_full)) {
    UREC = UREC_full[i,]
    UREC = st_transform(UREC, st_crs(raster_file))
    ut_urec = terra::crop(raster_file, UREC)
    urec_raster = terra::rasterize(UREC, ut_urec) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
    ut_mask = terra::mask(ut_urec, urec_raster)
    check = check_landscape(ut_mask, verbose = T)
    print(paste0('Calculating landscape metrics for iteration ', i, ' with id ', UREC$id ))
    if (check$OK == "✖") {
      area_mn = 0   #Needs to be changed
      pd = "NA"        #Needs to be changed
      
    } else {
      #Calculate landscape metrics
      
      area_mn =  lsm_l_area_mn(ut_mask)
      area_mn = area_mn$value     #Mean area of patches
      pd = lsm_l_pd(ut_mask)
      pd = pd$value               #Patch Density 
      
      #Add values to UREC table based on primary key
      
      # UREC$area_mnU = area_mn$value       #Needs to be changed
      #UREC$pdU= pd$value                 #Needs to be changed
    }
    
    UREC_full[i, area_new_col] = area_mn
    UREC_full[i, pd_new_col] = pd
  }
  return(UREC_full)
}

