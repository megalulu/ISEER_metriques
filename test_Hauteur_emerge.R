#Metrique Hauteur emerge au pied de berge 

#Import UREC_merge 
UREC_merge =st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
UREC_merge_mtm8 = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_MTM8.shp')
#Create virtual raster: these files are located on my external hard drive because there was no more space on the computer
list_files_mnt = list.files(path = 'D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM7', pattern = '*.tif', full.names = T)
output_vrt = 'D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM7/mnt_mtm7.vrt'
terra::vrt(list_files_mnt, output_vrt, overwrite = T)
#load virtual rasters previously created from external hard drive
mnt_8 = rast('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM8/mnt_mtm8.vrt')
mnt_7 = rast('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM7/mnt_mtm7.vrt')
#Open UREC_merge mtm8 et 7 
UREC_merge_mtm8 = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_MTM8.shp')
UREC_merge_mtm8 = st_transform(UREC_merge_mtm8, st_crs(mnt_8))

UREC_merge_mtm7 = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_MTM7.shp')
UREC_merge_mtm7 = st_transform(UREC_merge_mtm7, crs(mnt_7))
#list files of sampling 
sampling_files = list.files('C:/Meghana/Belgique/decembre/data/sampling', pattern = '*.shp', full.names = T)

for(f in 1:length(sampling_files)){
  file = st_read(sampling_files[f])
  if (f == 1){
    UREC_sampling = file
  } else{
    UREC_sampling = rbind(UREC_sampling, file)
  }
}
UREC_sampling_grp = group_by(UREC_sampling, by = 'id')

i = 1
r = 89

#This function does not work because it requires too much memory
HauteurEmergeeAuxPiedDeBerge <-
  function(UREC_merge_mtm, sampling_files, mnt_mtm) {
    UREC_merge_mtm$hepb = NA
    for (i in 1:nrow(UREC_merge_mtm)) {
      rive = UREC_merge_mtm[i, ]
      rive_id = rive$id
      #find correct sampling rive
      for (r in 1:length(sampling_files)) {
        sampling__path = sampling_files[r]
        sampling_id = sub(".*/sampling/", "", sampling__path)
        sampling_id = sub('.shp*','', sampling_id)
        
        
        if (sampling_id == rive_id) {
          sampling_rive = st_read(sampling_files[r])
          #sampling_rive_id = sampling_rive$id
          
          #make sure crs of sampling rive is the same as rive
          sampling_rive = st_transform(sampling_rive, crs(rive))
          
          #reduce extent of raster to area of UREC
          mnt_clip = terra::crop(mnt_mtm, rive) #Reduce the extent of the ESA raster to the extent of UREC
          rive_raster = terra::rasterize(sampling_rive, mnt_clip) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
          mnt_mask = terra::mask(mnt_clip, rive_raster)  #create mask on raster with UREC shapefile
          #extract min value of raster for each sampling unit
          sampling_rive$minalt = raster::extract(mnt_mask, sampling_rive, fun = min)
          
          sampling_raster = raster::rasterize(sampling_rive, mnt_mask, field = sampling_rive$minalt[, 2])
          mnt_min = mnt_mask - sampling_raster
          hePb = median(values(mnt_min, na.rm = T))
          UREC_merge_mtm[i, ]$hepb = hePb
        }
      }
    }
    return(UREC_merge_mtm)
  }



UREC_mtm8_hepb = HauteurEmergeeAuxPiedDeBerge(UREC_merge = UREC_merge_mtm8,
                                              sampling_files = sampling_files,
                                              mnt_mtm = mnt_8)
st_write(UREC_mtm8_hepb, 'C:/Meghana/Belgique/decembre/traitements/Hauteur_emerge/UREC_mtm8_hepb.shp')


UREC_merge_mtm7_a = UREC_merge_mtm7[3:4,]



i=1
r = 154
UREC_merge_mtm=UREC_merge_mtm7
UREC_merge_mtm$hepb = NA
mnt_mtm = mnt_7


#New try -> Seems to work. Need to clear memory (sometimes turn on and off computers to clear hidden memory)
#this is for MTM7 
i=1
for (i in 1:nrow(UREC_merge_mtm)) {
  rive = UREC_merge_mtm[i,]
  rive_id = rive$id

  mnt_mtm = mnt_7
  #reduce extent of raster to area of UREC
  mnt_clip = terra::crop(mnt_mtm, rive) #Reduce the extent of the ESA raster to the extent of UREC
  rm(mnt_mtm)
  rive_raster = terra::rasterize(rive, mnt_clip) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
  mnt_mask = terra::mask(mnt_clip, rive_raster)  #create mask on raster with UREC shapefile
  #extract min value of raster for each sampling unit
  #rive$minalt = terra::extract(mnt_mask, rive, fun = min)
  min_max = minmax(mnt_mask)
  #rive_raster = raster::rasterize(rive, mnt_mask, field = rive$minalt)
  mnt_min = mnt_mask - min_max[1]
  rm(mnt_mask)
  rm(mnt_clip)
  #writeRaster(mnt_min,paste0('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM7/clip_mtm7/', rive_id, '.tif'), overwrite = T)
  values_mnt_mask = values(mnt_min)
  hePb = median(values_mnt_mask, na.rm = T)
  # hePb = median(values(mnt_min, na.rm = T))
  UREC_merge_mtm[i, ]$hepb = hePb
  rm(mnt_min)
  rm(rive_raster)
  
}

UREC_hepb_mtm7 = UREC_merge_mtm
st_write(UREC_hepb_mtm7, 'C:/Meghana/Belgique/decembre/traitements/Hauteur_emerge/UREC_mtm7_hepb.shp')

#########MERGE both help UREC files 
UREC_hepb_mtm8 = st_read( 'C:/Meghana/Belgique/decembre/traitements/Hauteur_emerge/UREC_mtm8_hepb.shp')
UREC_hepb_mtm8 = st_transform(UREC_hepb_mtm8, crs(UREC_hepb_mtm7))
UREC_hepb = rbind(UREC_hepb_mtm7, UREC_hepb_mtm8)
UREC_hepb = st_transform(UREC_hepb, crs(UREC_merge))
st_write(UREC_hepb, 'C:/Meghana/Belgique/decembre/traitements/Hauteur_emerge/UREC_hepb.shp')
