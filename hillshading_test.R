#Some tests to clip MNT_files 
library(gdalUtils)
source('metric_functions.R')

#Build VRT file of MNT
mnt_path = 'C:/Meghana/donnee_brutes/MNT'
mnt_list_file = list.files(mnt_path,pattern="*.tif$",full.names=TRUE)
mnt_file_vrt = 'C:/Meghana/donnee_brutes/MNT/mnt4.vrt'
terra::vrt(mnt_list_file, filename=mnt_file_vrt, options=NULL, overwrite=FALSE)


#Build VRT file of MHC files
mhc_path = 'C:/Meghana/donnee_brutes/MHC'
mhc_list_file = list.files(mhc_path,pattern="*.tif$",full.names=TRUE)
mhc_file_vrt = 'C:/Meghana/donnee_brutes/MHC/mhc2.vrt'
terra::vrt(mhc_list_file, filename=mhc_file_vrt, options=NULL, overwrite=FALSE)

i=1
for (i in 1:length(mhc_list_file)){
  mhc_file = mhc_list_file[i]
  name = sub(".*MHC/", "", mhc_file)
  name = sub(".tif*.","",name)
  name_file = paste0(mhc_path, '/VRT/', name, ".vrt") 
  terra::vrt(mhc_file, filename=name_file, options=NULL, overwrite=T)

}

#Mask all MHC_vrt files with UREC_merge
list_file_mhc_vrt = list.files('C:/Meghana/donnee_brutes/MHC/VRT', pattern = "*.vrt", full.names = T)
for( i in 1:length(list_file_mhc_vrt)){
  tile = terra::rast(list_file_mhc_vrt[i])
  name_file = paste0(mhc_path, '/VRT/', name, ".vrt") 
  UREC_merge_vect = terra::vect(UREC_merge)
  UREC_merge_vect = terra::project(UREC_merge_vect, crs(tile))
  UREC_clip = terra::crop(UREC_merge_vect, ext(tile))
  UREC_rast = terra::rasterize(UREC_clip, tile)
  tile_mask = terra::mask(tile, UREC_merge_vect)
  name = list_file_mhc_vrt[i]
  name = sub(".*MHC/", "", mhc_file)
  name = sub(".tif*.","",name)
  name_file = paste0('C:/Meghana/Belgique/decembre/traitements/MNS_mask/new_mask/', name, '.tif')
  writeRaster(tile_mask, name_file, overwrite = T )
}

################################
#Build vrt of MHC_masks

mhc_mask_files = list.files('C:/Meghana/Belgique/decembre/traitements/MHC_mask/', pattern = '*.tif', full.names = T)
output_mhc_mask = 'C:/Meghana/Belgique/decembre/traitements/MHC_mask/mhc7_mask.vrt'
terra::vrt(mhc_mask_files, filename=output_mhc_mask, options=NULL, overwrite=TRUE)

#Build VRT file of MNS
mns_path = 'C:/Meghana/Belgique/decembre/traitements/MNS_mask/'
mns_list_file = list.files(mns_path,pattern="*.tif$",full.names=TRUE)
mns_file_vrt = 'C:/Meghana/Belgique/decembre/traitements/MNS_mask//mns1.vrt'
terra::vrt(mns_list_file, filename=mns_file_vrt, options=NULL, overwrite=FALSE)


#Build VRT file of ombrage
ombrage_path = 'C:/Meghana/Belgique/decembre/traitements/ombrage/'

ombrage_list_file = list.files(ombrage_path,pattern="*.tif$",full.names=TRUE)
ombrage_file_vrt = 'C:/Meghana/Belgique/decembre/traitements/ombrage/ombrage2.vrt'
terra::vrt(ombrage_list_file, filename=ombrage_file_vrt, options=NULL, overwrite=FALSE)
#create vrt for ombrage2
ombrage_file_path = 'C:/Meghana/Belgique/decembre/traitements/ombrage/ombrage2.tif'
ombrage_file_vrt = 'C:/Meghana/Belgique/decembre/traitements/ombrage/ombrage2.vrt'
terra::vrt(ombrage_file_path, filename=ombrage_file_vrt, options=NULL, overwrite=FALSE)


#make vrt of all ombrage vrt
ombrage_all = list(ombrage_file_vrt, )
#######LOAD VRT FILES and UREC file

mnt_vrt1 = terra::rast(file_vrt)
extent_mnt_vrt1 = extent(mnt_vrt1)
UREC_hillshade = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
UREC_hillshade=st_transform(UREC_hillshade, crs(mnt_vrt1))

clipUREC_hillshade= st_intersection(UREC_hillshade, extent_mnt_vrt1)
  
mnt_clip = terra::crop(mnt_vrt1, UREC_hillshade)
mnt_mask = terra::mask(mnt_clip, UREC_hillshade)



ut19_clip = terra::crop(raster_UT, UREC_vect) #Reduce the extent of the ESA raster to the extent of UREC
UREC_raster = terra::rasterize(UREC_vect, ut19_clip) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
ut_mask = terra::mask(ut19_clip, UREC_raster)  #create mask on raster with UREC shapefile



test1 = raster( 'C:/Meghana/Belgique/decembre/traitements/ombrage//ombrage1.vrt')
test = rast(ombrage_file_vrt)
