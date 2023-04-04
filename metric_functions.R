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



LateralWidthFunction <- function(UREC_merge, UEA_merge){
  #UREC_merge : sf object of spatial units with Id_UEA
  #UEA_merge : merged shapefile (as sf object) of UEA used to create UREC. Must also have Id_UEA has primary key
  UREC_merge$meanlenght = NA
  for (i in 1:nrow(UREC_merge)){
    print(paste0('measuring ', UREC_merge[i,]$id))
    rive = UREC_merge[i,]
    Id_uea=  rive$Id_UEA
    #Create a query to find the appropraite UEA 
    query = UEA_merge$Id_UEA == Id_uea
    uea = filter(UEA_merge, query) #Find the correct UEA based on Id_uea
    uea = st_cast(uea, to = 'LINESTRING')
    class = class(uea)
    if(class[2]!= 'sfc'){
      uea = st_as_sfc(uea)
    }
    
    semis = st_line_sample(uea, density = 0.02, type = 'regular') #create semis every 50m
    voronoi = st_voronoi(semis)%>% st_cast() #create polygone voronoi
    voronoi = st_cast(voronoi, to = 'LINESTRING')#convert polygone voronoi to linestring
    polygon1 =rive %>% st_cast(t0 = 'POLYGONE') #make sure rive is a polygone type
    voronoi1 = st_intersection(voronoi,polygon1 )  #cut voronoi linestring to shell of rive polygone
    lenghts = st_length(voronoi1)
    average_lenght = mean(lenghts)
    UREC_merge[i,]$meanlenght = average_lenght
    x = 7
  }
  return(UREC_merge)
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

UREC_merge = c
raster_file = vrt_mhc_mask7
EPSG= 'EPSG:2949'
urban_vector_mask = urban_vector_mask
i = 1
TreeStats <- function(UREC_merge, raster_file, EPSG,  urban_vector_mask){
  #UREC_merge : spatVector file of feature surface per UREC that has been reprojected to match raster file projetion and clipped to extent of raster_file (see main)
  #raster_file : raster file (or vrt) of MHC mosaic split based on projection (either MTM7 or MTM8)
  #EPSG : Character string of projection. example : "EPSG: 2949"
  #Urban_vector_mask : Spactvector file of vecotirized urban areas at same resolution as raster_file
  UREC_merge$meanHeight = NA
  UREC_merge$medianHeight =NA
  UREC_merge$sdHeight = NA
  UREC_merge$majorityHeight =NA
  for (i in 1:nrow(UREC_merge)){
    feature =  UREC_merge[i,]
    print(paste0('processing UREC : ', feature$id))
    features_sf = st_as_sf(feature) #load polygon as sf object to be used in exact extract
    features_sf = st_transform(features_sf, st_crs(raster_file))
    vrt_mhc_mask7_clip = terra::crop(raster_file, features_sf)
    vrt_mhc_mask7_clip[vrt_mhc_mask7_clip<1.5]<-NA
    feature_raster = terra::rasterize(features_sf, vrt_mhc_mask7_clip)
    vrt_mhc_mask7_feature = terra::mask(vrt_mhc_mask7_clip, feature_raster)
    porj_urbain = terra::project(urban_vector_mask, EPSG)
    proj_urbain_sf = st_as_sf(porj_urbain)
    vrt_mhc_mask7_feature_urbain = terra::mask(vrt_mhc_mask7_feature, proj_urbain_sf, inverse = T)
    
    feature$meanHeight =as.numeric(exactextractr::exact_extract(vrt_mhc_mask7_feature_urbain, features_sf, 'mean'))
    feature$medianHeight =as.numeric(exactextractr::exact_extract(vrt_mhc_mask7_feature_urbain, features_sf, 'median'))
    feature$sdHeight = as.numeric(exactextractr::exact_extract(vrt_mhc_mask7_feature_urbain, features_sf, 'stdev'))
    feature$majorityHeight = as.numeric(exactextractr::exact_extract(vrt_mhc_mask7_feature_urbain, features_sf, 'majority'))
    
    UREC_merge[i,]$meanHeight = feature$meanHeight
    UREC_merge[i,]$medianHeight = feature$medianHeight
    UREC_merge[i,]$sdHeight = feature$sdHeight
    UREC_merge[i,]$majorityHeight = feature$majorityHeight
    
    
  }
  return(UREC_merge)
}

NumberClasses <- function(UREC_merge, raster_file){
  UREC_merge$nbr_class = NA
  for (i in 1:nrow(UREC_merge)){
    feature =  UREC_merge[i,]
    print(paste0('processing UREC : ', feature$id))
    features_sf = st_as_sf(feature) #load polygon as sf object to be used in exact extract
    features_sf = st_transform(features_sf, st_crs(raster_file))
    raster_file_clip = terra::crop(raster_file, features_sf)
    #vrt_mhc_mask7_clip[vrt_mhc_mask7_clip<1.5]<-NA
    feature_raster = terra::rasterize(features_sf, raster_file_clip)
    raster_file_mask_feature = terra::mask(raster_file_clip, feature_raster)
    # porj_urbain = terra::project(urban_vector_mask, EPSG)
    #  proj_urbain_sf = st_as_sf(porj_urbain)
    # vrt_mhc_mask7_feature_urbain = terra::mask(vrt_mhc_mask7_feature, proj_urbain_sf, inverse = T)
    
    feature$nbr_class =as.numeric(exactextractr::exact_extract(raster_file_mask_feature, features_sf, 'variety'))
    UREC_merge[i,]$nbr_class = feature$nbr_class
  }
  return(UREC_merge)
}

SurfaceClass <-
  function (UREC_merge,
            csv_class_correspondence,
            raster_file) {
    
    #UREC_merge = sf file of all spatiale units
    #raster_file = raster file of utilisation du territoire cliped to only have area covered by spatial units !!!Has to be loaded with raster library: raster("file_path")
    #csv__class_correspondence : group csv of class correspondence between UT classes and Values. This needs to be grouped by using group_by function with CODE UT column
    
    for (r in 1:nrow(UREC_merge)) {
      shp = UREC_merge[r, ]
      print(paste0('reading', shp$id))
      shp = st_transform(shp, crs = st_crs(raster_file)) # project vector UREC file to same projection as raster file
      ut_shp = terra::crop(raster_file, shp) #Reduce the extent of the ESA raster to the extent of UREC
      shp_raster = terra::rasterize(shp, ut_shp) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
      ut_mask = terra::mask(ut_shp, shp_raster)
      
      #Get frequency of each class in the raster
      freq_dist = freq(ut_mask)
      freq_dist = as.data.frame(freq_dist)
      
      #freq_dist$value = as.data.frame.integer(freq_dist$value)
      # join_tbl = left_join(freq_dist, IQBR_UT_2018_correspondence, c('value'='Value')) #raster needs to be loaded with raster library to work
      join_tbl = left_join(freq_dist,
                           csv_class_correspondence,
                           c('value' = 'CODE_UT'))
      join_tbl = na.omit(join_tbl) #remove NA from table
      total_pix = sum(join_tbl$count) #Count the number of pixels to get proportional coverage
      join_tbl$perc_coverage = round((join_tbl$count / total_pix), 2)
      summarize_joint_tbl = join_tbl %>% group_by(DESC_CAT) %>% dplyr::summarise(prop_surf = sum(perc_coverage))
      
      df_summarize_joint_tbl = as.data.frame(summarize_joint_tbl)
      transpose = as.data.frame(t(df_summarize_joint_tbl))
      names(transpose)<- transpose[1,] #make first row column names
      
      
      transpose <- transpose[-1,] #remove first row that now is present twice
      
      for (c in colnames(transpose)){
        UREC_merge[r,c] = transpose[,c]
      } 
      
    }
    return(UREC_merge)
    
  }

#Create function for average slope
AverageSlope <- function(UREC_merge, slope_raster){
  #UREC_merge : SpatVector of special units overlapping raster projection (ie MTM7 or MTM8)
  #slope_raster : Slope raster mosaic or vrt seperated into one projection (ie. MTM7 or MTM8)
  UREC_merge$avrSlope = NA
  for (i in 1:nrow(UREC_merge)){
    feature =  UREC_merge[i,]
    print(paste0('processing UREC : ', feature$id))
    features_sf = st_as_sf(feature) #load polygon as sf object to be used in exact extract
    features_sf = st_transform(features_sf, st_crs(slope_raster))
    slope_crop = terra::crop(slope_raster, features_sf)
    #slope_crop_mask  = terra::mask(slope_crop, feature)
    
    feature$avrSlope =as.numeric(exactextractr::exact_extract(slope_crop, features_sf, 'mean'))
    UREC_merge[i,]$avrSlope = feature$avrSlope
    
  }
  
  return(UREC_merge)
}
