#=============================================================
#
# Production de paramètres descripteurs des bandes riveraines dans l'environnement R
# 
# Version : mai à août 2022 - Stage Léa LETASSEY
#
#=============================================================

## Installation des packages 
pkgs = c( "sf",
          "sp",
          "raster",
          "rgdal",
          "dplyr",
          "units",
          "purrr",
          "lwgeom",
          "gdalUtils",
          "fasterize",
          "terra",
          "nngeo",
          'installr',
          'RSAGA') 
to_install = !pkgs %in% installed.packages()
if (any(to_install)) {
  install.packages(pkgs[to_install])
}

# Chargement des librairies
library(sp)
library(sf)
library(raster)
library(gdalUtilities)
library(dplyr)
library(units)
library(purrr)
library(lwgeom)
library (nngeo)
library(installr)
library(RSAGA)
library(terra)
library(exactextractr)
library(whitebox)

#************************************************************#
#              HAUTEUR EMERGEE AU PIED DE BERGE              #
#************************************************************#

#import list of files of sampling_rives
path_sampling_rives = 'C:/Meghana/Belgique/decembre/data/sampling/'
list_files_sampling_rives = list.files(
  path_sampling_rives,
  pattern = '*.shp',
  full.names = T
)


#import extent of MNT 
#etendu_shade7 = vect('C:/Users/Hp/Documents/STAGE_SHERBROOKE/donnees_brutes/etendue_MTM7_8/extent_mtm7.shp')
#etendu_shade8 = vect('C:/Users/Hp/Documents/STAGE_SHERBROOKE/donnees_brutes/etendue_MTM7_8/extent_mtm8.shp')
etendu_shade7 = vect('C:/Meghana/donnee_brutes/MNT/mnt4.vrt')


#convert extents to CRS of MTM7 and MTM8
etendu_shade7 = terra::project(etendu_shade7, 'epsg:2949') #MTM7
etendu_shade8 = terra::project(etendu_shade8,'epsg:2950' ) #MTM8


#import UREC_merge
#UREC_merge = vect('C:/Users/Hp/Documents/STAGE_SHERBROOKE/donnees_brutes/UREC_rive_merge/UREC_merge_new.shp')
UREC_merge = vect('C:/Users/Hp/Documents/STAGE_SHERBROOKE/UREC_rives_new/UREC_rives_new_merge.shp')

#convert UREC_merge to CRS of MTM7 and MTM8
UREC_merge7 = terra::project(UREC_merge, 'epsg:2949') #MTM7
UREC_merge8 = terra::project(UREC_merge,'epsg:2950' ) #MTM8


#intersect UREC_merge and extents
UREC_v7 = terra::intersect(UREC_merge7, etendu_shade7) 
UREC_v8 = terra::intersect(UREC_merge8, etendu_shade8)

#load MTM7 & MTM8
mtm7_merge = terra::rast('C:/Users/Hp/Documents/STAGE_SHERBROOKE/donnees_brutes/MNT/mtm7/MNT_mtm7_merge.tif')
mtm8_merge = terra::rast('C:/Users/Hp/Documents/STAGE_SHERBROOKE/donnees_brutes/MNT/mtm8/MNT_mtm8_merge.tif')

#loop MTM7
for (r in 1:length(UREC_v7)) {
  #load UREC data
  feature = UREC_v7[r]
  #get identifiers
  Id_uea = feature$Id_UEA
  Id_uea
  Id_rive = feature$Id_rive
  Id_rive
  name = 'C:/Users/Hp/Documents/STAGE_SHERBROOKE/VFinale_HEmPiedDeBerge' # output path
  couche = paste0(Id_uea,'_rive',Id_rive) #output layer name
  
  for (i in 1:length(list_files_sampling_rives)){
    #load sampling_rives data
    seg_sf = st_read(list_files_sampling_rives[i]) #read an sf object for the function exact_extract 
    #convert sampling_rives data to CRS of MTM7
    seg_sf = st_transform(seg_sf, 'epsg:2949')
    #get identifiers
    Id_seg_uea = seg_sf$ID_UEA[1]
    Id_seg_uea
    Id_seg_rive = seg_sf$Id_rive[1]
    Id_seg_rive
    #verify that the sampling_rives is in the UREC
    if ( (Id_uea == Id_seg_uea) & (Id_rive == Id_seg_rive) ){
      #keep MTM7 in the extent of UREC
      mnt_crop_extent = terra::crop(mtm7_merge,feature)
      plot(mnt_crop_extent)
      
      #create a new raster that has the same values as mnt_crop_extent
      mnt <- terra::mask(mnt_crop_extent,feature)
      plot(mnt)
      
      #calculate the minimum elevation of the entire raster
      min_max = minmax(mnt)
      feature$alt_min <- min_max[1,]
      
      #create a raster representating the minimum elevation 
      mnt_min <- rasterize(feature,mnt,field='alt_min')
      plot(mnt_min)
      
      ## generate relative DEM
      mnt_rel <- mnt - mnt_min
      plot(mnt_rel)
      
      seg_sf$alt_mean <- exact_extract(mnt_rel,seg_sf,fun='mean')
      
      ## calculate median of sampling_rives pixels values
      HEmPiedBerge = median(seg_sf$alt_mean, na.rm = T)
      feature$HEmPiedBerge = HEmPiedBerge
      
      #write output file
      terra::writeVector(feature, filename = name, layer = couche, filetype="ESRI Shapefile", overwrite = TRUE)
    }
  }
}


#loop MTM8
for (r in 1:length(UREC_v8)) {
  #load UREC data
  feature = UREC_v8[r]
  #get identifiers
  Id_uea = feature$Id_UEA
  Id_uea
  Id_rive = feature$Id_rive
  Id_rive
  name = 'C:/Users/Hp/Documents/STAGE_SHERBROOKE/VFinale_HEmPiedDeBerge' # output path
  couche = paste0(Id_uea,'_rive',Id_rive) #output layer name
  
  for (i in 1:length(list_files_sampling_rives)){
    #load sampling_rives data
    seg_sf = st_read(list_files_sampling_rives[i]) #read an sf object for the function exact_extract 
    #convert sampling_rives data to CRS of MTM8
    seg_sf = st_transform(seg_sf, 'epsg:2950')
    #get identifiers
    Id_seg_uea = seg_sf$ID_UEA[1]
    Id_seg_uea
    Id_seg_rive = seg_sf$Id_rive[1]
    Id_seg_rive
    #verify that the sampling_rives is in the UREC
    if ( (Id_uea == Id_seg_uea) & (Id_rive == Id_seg_rive) ){
      #keep MTM8 in the extent of UREC
      mnt_crop_extent = terra::crop(mtm8_merge,feature)
      plot(mnt_crop_extent)
      
      #create a new raster that has the same values as mnt_crop_extent
      mnt <- terra::mask(mnt_crop_extent,feature)
      plot(mnt)
      
      #calculate the minimum elevation of the entire raster
      min_max = minmax(mnt)
      feature$alt_min <- min_max[1,]
      
      #create a raster representating the minimum elevation 
      mnt_min <- rasterize(feature,mnt,field='alt_min')
      plot(mnt_min)
      
      ## generate relative DEM
      mnt_rel <- mnt - mnt_min
      plot(mnt_rel)
      
      seg_sf$alt_mean <- exact_extract(mnt_rel,seg_sf,fun='mean')
      
      ## calculate median of sampling_rives pixels values
      HEmPiedBerge = median(seg_sf$alt_mean, na.rm = T)
      feature$HEmPiedBerge = HEmPiedBerge
      
      #write output file
      terra::writeVector(feature, filename = name, layer = couche, filetype="ESRI Shapefile", overwrite = TRUE)
    }
  }
}
