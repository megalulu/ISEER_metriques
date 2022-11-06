#Script to calculate IQBR 
###############################################################################
#load libraries
library(terra)
library(raster)
#load correspondance entre IQBR et classes de UT 2018 10m
IQBR_UT_2018_correspondence <- read.csv("C:/Meghana/donnee_brutes/IQBR_UT_2018_correspondence.csv", sep=";")

#load Utilisation du territoire raster data
ut = terra::rast('C:/Meghana/donnee_brutes/UTILISATION_TERRITOIRE/utilisation_territoire_2019/utilisation_territoire_2019/utilisation_territoire_2019.tif')
#load UREC_merge data (to get extent)
UREC_merge = st_read('C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_new.shp')
IQBR = UREC_merge[,c('Id_UEA', 'Id_rive')]


#UT clip should already be savec in your files. Otherwise create this file
#ut_clip = terra::crop(ut, UREC_merge)
#Load UT_clip with raster
path_ut_clip = 'C:/Meghana/Belgique/traitement/data/UTILISATION_TERRITOIRE_2018_10m/test.tif'
ut_clip_r = raster::raster('C:/Meghana/Belgique/traitements/data/UTILISATION_TERRITOIRE_2018_10m/Utilisation_territoire_2018_10m_clip.tif')

#Crop Utilisation du territoire to the extent of UREC_merge
#IQBR = project(IQBR, ut)
IQBR = st_transform(IQBR, st_crs(ut_clip_r))






#Do for loop to get UT data per UREC_rive file, save this in a folder
r = 1

for (r in 1:length(IQBR)) {
  shp = IQBR[r,]

  shp = st_transform(shp, crs = st_crs(ut_clip_r)) # project vector UREC file to same projection as ESA raster file (WGS84)
  ut_shp = terra::crop(ut_clip_r, shp) #Reduce the extent of the ESA raster to the extent of UREC
  shp_raster = terra::rasterize(shp, ut_shp) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
  ut_mask = terra::mask(ut_shp, shp_raster)
      
   #Calculate IQBR for each masked raster area
  #Get frequency of each class in the raster 
  freq_dist = freq(ut_mask)
  freq_dist= as.data.frame(freq_dist)
  #freq_dist$value = as.data.frame.integer(freq_dist$value) 
  join_tbl = left_join(freq_dist, IQBR_UT_2018_correspondence, c('value'='Value')) #raster needs to be loaded with raster library to work 
  
  join_tbl= na.omit(join_tbl) #remove NA from table
  total_pix = sum(join_tbl$count) #Count the number of pixels to get proportional coverage
  join_tbl$perc_coverage = (join_tbl$count/ total_pix)*100
  join_tbl$IQBR = join_tbl$perc_coverage*join_tbl$Poids_IQBR
  indice_IQBR = sum(join_tbl$IQBR)/10
  
  shp$IQBR = indice_IQBR
  IQBR[r,]$IQBR = shp$IQBR

  

}

st_write(IQBR, 'C:/Meghana/Belgique/traitements/data/IQBR/UREC_merge_IQBR.shp', overwrite = T)
