#Forest UT in UREC export
install.packages("erer")
library(erer)
#Create batch files 
path_raster_forest = "C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Forest_UREC/"
file_list_raster_forest = list.files(path_raster_forest, pattern = '*.tif', full.names = T)  
path_raster_grass = "C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Grass_UREC/"
file_list_raster_grass = list.files(path_raster_grass, pattern = '*.tif', full.names = T)  
  
txt_path =paste0(path_raster_grass, "geotiffbatch_grass.fbt")
batch_list = c()
ls = as.list(file_list_raster_grass)
f = 1
for (f in 1:length(ls)){
  file = ls[f]
  file = paste0(file, ", x, 999, x, x, 1, x, IDF_GeoTIFF")
  batch_list = c(batch_list,file)
  
}

batch_list = as.list(batch_list)
erer::write.table(batch_list, path_raster_grass)  
capture.output(summary(batch_list), file = "geotiffbatch_grass.fbt")
lapply(batch_list, function(x) write.table( data.frame(x), 'geotiffbatch.fbt'  , append= T, sep=',' ))  
write.list(batch_list, file ="geotiffbatch_grass.fbt" , sep = ',', row.names = F)


#load ESA data and UREC data 
esa = rast('C:/Meghana/Belgique/donnees_brutes/ESA_WORLDCOVER/data/esa_Qc_lamb.tif')

UREC_merge #this should already exist

i = 1
UREC_merge_v = vect(UREC_merge)  

for (i in 1:nrow(UREC_merge_v)){
  shp = UREC_merge_v[i]
  name = paste0('C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Grass_UREC/', shp$Id_UEA, '_rive', shp$Id_rive, '.tif')
  shp = terra::project(shp, esa)
  esa_clip = terra::crop(esa, shp)
  shp_raster = terra::rasterize(shp, esa_clip)
  esa_mask = terra::mask(esa_clip, shp_raster)
  esa_G= esa_mask
  esa_G[esa_G !=30] <- NA
  esa_G[esa_G== 30]<- 1
  writeRaster(esa_G, name, overwrite = T)
  
}


UREC_raster = terra::rasterize(UREC_merge_v, esa_clip)

esa_mask = terra::mask(esa_clip, UREC_raster)
esa_F= esa_mask
esa_F[esa_F!=10] <-NA
esa_F[esa_F == 10]<- 1
##########################################################################
#Get Land use data from clipped ESA to UREC for urban class

#Read and put in list all files for landuse clipped to UREC
path_esa_UREC = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/ESA_UREC/'
list_files_esa_UREC = list.files(path_esa_UREC, pattern = '.*tif', full.names = T)

#Do for loop to extract only urban land use and write out tif file.
i = 47
for (i in 1:length(list_files_esa_UREC)){
  tile = rast(list_files_esa_UREC[i])
  tile[tile != 50] <- NA
  tile[tile == 50]<- 1
  tile_name = list_files_esa_UREC[i]
  name =  sub(".*ESA_UREC/ESA_", "", tile_name)
  name = paste0('C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Urban_UREC/', name)
  writeRaster(tile, name, overwrite = T)

}

