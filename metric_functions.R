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
library(tidyr)
#library(whitebox)



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
  #UREC_full = shapefile of UREC (UREC_merge) as sf data.frame
  #path_sampling : character string leading to folder where the individual sampling files are for each UREC. 
  #raster_UT = raster of Utilisation du territoire masked for values we are interested in. e.g.  <Forest, Milieu humide> (need to do this step beforehand , in ArcMap sometimes)
  # classe_UT = character string of name of the new colum referencing the specific UT selection eg. "Vegetation Optimale" 
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
  #col_name  = character string of name of colum referencing the specific UT selection eg. "Forest" 
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
      pd = NA        #Needs to be changed
      
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


Indice_Naturalite_functions <-function(UREC_merge, raster_file, csv_class_correspondence ){
  #UREC_merge = sf file of all spatiale units
  #raster_file = raster file of utilisation du territoire cliped to only have area covered by spatial units
  #csv__class_correspondence : group csv of class correspondence between UT classes and IQBR classes (this must have weights) --> then use group_by function with CODE UT column
  for (r in 1:nrow(UREC_merge)) {
    shp = UREC_merge[r,]
    print(paste0('reading', shp$id))
    shp = st_transform(shp, crs = st_crs(raster_file)) # project vector UREC file to same projection as raster file 
    ut_shp = terra::crop(raster_file, shp) #Reduce the extent of the ESA raster to the extent of UREC
    shp_raster = terra::rasterize(shp, ut_shp) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
    ut_mask = terra::mask(ut_shp, shp_raster)
    
    #Calculate IQBR for each masked raster area
    #Get frequency of each class in the raster 
    freq_dist = freq(ut_mask)
    freq_dist= as.data.frame(freq_dist)
    #freq_dist$value = as.data.frame.integer(freq_dist$value) 
    # join_tbl = left_join(freq_dist, IQBR_UT_2018_correspondence, c('value'='Value')) #raster needs to be loaded with raster library to work 
    join_tbl = left_join(freq_dist, csv_class_correspondence, c('value'= 'CODE_UT'))
    join_tbl= na.omit(join_tbl) #remove NA from table
    total_pix = sum(join_tbl$count) #Count the number of pixels to get proportional coverage
    join_tbl$perc_coverage = (join_tbl$count/ total_pix)*10
    join_tbl$IndiceNat = join_tbl$perc_coverage*join_tbl$Poids_IndiceNat
    indice_Naturalite = sum(join_tbl$IndiceNat)/10
    
    #shp$IQBR = indice_IQBR
    UREC_merge[r,'Indice_Nat'] = indice_Naturalite
    
    
    
  }
  return(UREC_merge)
}


# UREC_water = UREC_water_merge_proj
# raster_file = mhc_vrt
# EPSG = 'EPSG:2949'
# urban_vector_mask = mask_urbain_vectoriel
# i = 1
OverhangingCanopy <- function(UREC_water, raster_file, EPSG,  urban_vector_mask){
  #UREC_water : spatVector file of water surface per UREC that has been reprojected to match raster file projetion and clipped to extent of raster_file (see main)
  #raster_file : raster file (or vrt) of MHC mosaic split based on projection (either MTM7 or MTM8)
  #EPSG : Character string of projection. example : "EPSG: 2949"
  #Urban_vector_mask : Spactvector file of vecotirized urban areas at same resolution as raster_file
  UREC_water$canopyRatio = NA
  for (i in 1:nrow(UREC_water)){
    water =  UREC_water[i,]
    print(paste0('processing UREC : ', water$id))
    waters_sf = st_as_sf(water) #load polygon as sf object to be used in exact extract
    
    vrt_mhc_mask7_clip = terra::crop(raster_file, waters_sf)
    vrt_mhc_mask7_clip = vrt_mhc_mask7_clip[vrt_mhc_mask7_clip>1.5]
    water_raster = terra::rasterize(waters_sf, vrt_mhc_mask7_clip)
    vrt_mhc_mask7_water = terra::mask(vrt_mhc_mask7_clip, water_raster)
    porj_urbain = terra::project(urban_vector_mask, EPSG)
    proj_urbain_sf = st_as_sf(porj_urbain)
    vrt_mhc_mask7_water_urbain = terra::mask(vrt_mhc_mask7_water, proj_urbain_sf, inverse = T)
    
    water$canopySurf =as.numeric(exactextractr::exact_extract(vrt_mhc_mask7_water_urbain, waters_sf, 'count'))
    water$area = as.numeric(st_area(waters_sf))
    water$canopyRatio = water$canopySurf/water$area
    UREC_water[i,]$canopyRatio = water$canopyRatio
    
  }
  return(UREC_water)
}


IQBR_functions <-function(UREC_merge, raster_file, csv_class_correspondence ){
  #UREC_merge = sf file of all spatiale units
  #raster_file = raster file of utilisation du territoire cliped to only have area covered by spatial units
  #csv__class_correspondence : group csv of class correspondence between UT classes and IQBR classes (this must have weights) --> then use group_by function with CODE UT column
  for (r in 1:nrow(UREC_merge)) {
    shp = UREC_merge[r,]
    print(paste0('reading', shp$id))
    shp = st_transform(shp, crs = st_crs(raster_file)) # project vector UREC file to same projection as raster file 
    ut_shp = terra::crop(raster_file, shp) #Reduce the extent of the ESA raster to the extent of UREC
    shp_raster = terra::rasterize(shp, ut_shp) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
    ut_mask = terra::mask(ut_shp, shp_raster)
    
    #Calculate IQBR for each masked raster area
    #Get frequency of each class in the raster 
    freq_dist = freq(ut_mask)
    freq_dist= as.data.frame(freq_dist)
    #freq_dist$value = as.data.frame.integer(freq_dist$value) 
    # join_tbl = left_join(freq_dist, IQBR_UT_2018_correspondence, c('value'='Value')) #raster needs to be loaded with raster library to work 
    join_tbl = left_join(freq_dist, csv_class_correspondence, c('value'= 'CODE_UT'))
    join_tbl= na.omit(join_tbl) #remove NA from table
    total_pix = sum(join_tbl$count) #Count the number of pixels to get proportional coverage
    join_tbl$perc_coverage = (join_tbl$count/ total_pix)*10
    join_tbl$IQBR = join_tbl$perc_coverage*join_tbl$Poids_IQBR
    indice_IQBR = sum(join_tbl$IQBR)/10
    
    #shp$IQBR = indice_IQBR
    UREC_merge[r,'IQBR'] = indice_IQBR
    
    
    
  }
  return(UREC_merge)
}
