#Whitebox test 

#Import MNS 
mns7 = rast('C:/Meghana/Belgique/traitements/data/MNS/MNS_calculer/mns7.tif')
#load virtual mhc rasters per given projection
UREC_merge #should already exist from past creation
UREC_merge_v = vect(UREC_merge) #convert sf object into SpatVect object
#Convert UREC_merge_v to CRS of MHC7 and MHC8
UREC_v7 = terra::project(UREC_merge_v, 'epsg:2949') #MTM7

#Get extents of rasters in both projections
etendu_mns7 = ext(mns7)#MHC7
#Clip UREC_v 7 and 8 to extent of mhc7 and 8 
UREC7 = terra::crop(UREC_v7, etendu_mns7) #Cropped UREC_v7 to only include UREC that overlay the extent of raster mns_shade7

l = 1
a = 3
i = 3
#for loop test on mns_shade7 EPSG : 2949
for (l in 1:nrow(UREC7)) {#change this depending on projection needed
  rive = UREC7[l] #changed this depending on projection needed
  Id_uea = rive$Id_UEA
  Id_uea
  Id_rive = rive$Id_rive
  Id_rive
  for (a in 1:length(list_files_UREC_rives)) {
    seg = vect(list_files_UREC_rives[a])
    Id_seg = seg$Id_UEA
    Id_seg
    Id_seg_rive = seg$Id_rive
    Id_seg_rive
    
    if ((Id_uea == Id_seg) & (Id_seg_rive == Id_rive)) {
      for (i in 1:length(list_files_water)) {
        water = vect(list_files_water[i])
        water_uea = water$ID_UEA
        water_uea
        water_rive = water$Id_rive
        water_rive
        water = terra::project(water, rive) #project water to same projection as rive (either MTM7 --> espg : 2949 OR MTM8 --> espg: 2950)
        if ((water_uea == Id_uea) & (water_rive == Id_rive)) {
          name = paste0(
            'C:/Meghana/Belgique/traitements/results/UREC_rives_new/',
            Id_uea,
            '_rive',
            Id_rive,
            '.shp'
          )
          name
          ombre = mns7 #Needs to be changed depending on UREC projection (UREC_v7 or UREC_v8)
          ombre = terra::crop(ombre, water)
          water_raster = terra::rasterize(water,ombre)
          ombre <-
            terra::mask(ombre, water_raster)# masquer le raster ombre avec la surface de l'eau
    
          writeRaster(ombre,'C:/Meghana/Belgique/traitements/whitebox_tests/ombre1.tif' )
          path_dem = 'C:/Meghana/Belgique/traitements/whitebox_tests/ombre1.tif'
          
          #Get centerpoit lat long of rive
          rive_centroid = terra::centroids(dem)
          rive_centroid = terra::project(rive_centroid, "+proj=longlat +datum=WGS84") #project to WGS84 lat/long
          latlong = terra::geom(rive_centroid)
          
          
          time_in_day = whitebox::wbt_time_in_daylight(dem = dem ,
                                                       output = 'C:/Meghana/Belgique/traitements/whitebox_tests/test.tif',
                                                       lat =-71.90995  , long = 46.12234,
                                                       utc_offset = -05:00, start_day = 151, end_day = 243,
                                                       start_time = 'sunrise', end_time = 'sunset')
          
          
          water_raster = terra::rasterize(water,ombre)
          ombre <-
            terra::mask(ombre, water_raster)# masquer le raster ombre avec la surface de l'eau
          # normaliser et inverser :
          #ombre <- ombre / 90
          #ombre <- abs(ombre - 1)
          IndOmbrage = median(values(ombre), na.rm = T)
          seg$IndOmbrage = IndOmbrage
          terra::writeVector(seg, name, overwrite = T)
          print(paste0('writing file UEA ', Id_seg, ' rive ',Id_seg_rive) )
        }
      }
    }
  }
}