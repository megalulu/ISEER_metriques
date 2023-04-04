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
mnt7 = rast('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM7/mnt_mtm7.vrt')
#Open UREC_merge mtm8 et 7 
UREC_merge_mtm8 = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_MTM8.shp')
UREC_merge_mtm8 = st_transform(UREC_merge_mtm8, st_crs(mnt_8))

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


HauteurEmergeeAuxPiedDeBerge <-
  function(UREC_merge_mtm, sampling_files, mnt_mtm) {
    UREC_merge_mtm$hepb = NA
    for (i in 1:nrow(UREC_merge_mtm)) {
      rive = UREC_merge_mtm[i, ]
      rive_id = rive$id
      #find correct sampling rive
      for (r in 1:length(sampling_files)) {
        sampling_rive = st_read(sampling_files[r])
        sampling_rive_id = sampling_rive$id
        if (sampling_rive_id[1] == rive_id) {
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
  }



UREC_mtm8_hepb = HauteurEmergeeAuxPiedDeBerge(UREC_merge = UREC_merge_mtm8,
                                              sampling_files = sampling_files,
                                              mnt_mtm = mnt_8)
