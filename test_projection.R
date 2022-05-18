#Tests on mnt and mhc data for projection purposes
UREC_merge
UREC_merge_v = vect(UREC_merge)
mhc7 = rast('C:/Meghana/Belgique/traitements/data/MHC/mtm7/mhc_mtm7.vrt') 
mhc8 = rast('C:/Meghana/Belgique/traitements/data/MHC/mtm8/mhc_mtm8.vrt')

#Convert UREC_merge_v to CRS of MHC7 and MHC8
UREC_v7 = terra::project(UREC_merge_v, 'epsg:2949') #MTM7
UREC_v8 = terra::project(UREC_merge_v,'epsg:2950' ) #MTM8

#Get extents of SpatRaster
etendu7 = ext(mhc7)#MHC7
etendu8 = ext(mhc8) #MHC8

#Clip UREC_v 7 and 8 to extent of mhc7 and 8 
UREC_v7 = terra::crop(UREC_v7, etendu7) #Cropped UREC_v7 to only include UREC that overlay the extent of raster mhc7
UREC_v8 = terra::crop(UREC_v8, etendu8)

#Extract surface of canopee with mhc_v7 and mhc_v8
file_list_sampling_full #Variable already exists and represents all the shp for UREC sampling
l=1
i = 3
r=2
#For loop to extract the area of canopy over the river for each UREC. 
for (l in 1:nrow(UREC_v7)) { #Needs to be changed 
  rive = UREC_v7[l] #Read UREC AND needs to be changed 
  Id_seg = rive$Id_UEA
  Id_seg
  Id_side = rive$Id_rive
  Id_side
  name = paste0(
    path_name_UREC_rives,
    Id_uea,
    '_rive',
    Id_rive,
    '.shp'
  )
  name
  for (i in 1:length(list_files_sampling_rive)) {
    shp = vect(list_files_sampling_rive[i]) #Read sampling shp for UREC
    shp = terra::project(shp, 'epsg:2949') #needs to be changed depending on projection
    shp_sf = st_read(list_files_sampling_rive[i]) #read vector file as sf object
    shp_sf = st_transform(shp_sf, crs = st_crs(shp)) #convert sf vector ro MTM7 coordinate system
    Id_uea = shp$ID_UEA[1]
    Id_uea
    Id_rive = shp$Id_rive[1]
    Id_rive
    if (Id_seg == Id_uea & Id_side == Id_rive) {
      for (r in 1:length(list_file_water_surface)) {
        water = vect(list_file_water_surface[r]) #Read water surface polygon 
        Id_water = water$ID_UEA
        Id_water
        water = terra::project(water, 'epsg:2949') #needs to be changed depending on projection
        water_sf = st_read(list_file_water_surface[r]) #read water surface polygon as sf object
        water_sf = st_transform(water_sf, crs = st_crs(water)) #transform vector sf object to MTM7 projection
        if (Id_uea == Id_water) {#match water surface to sampling shp
          sampling_water = terra::intersect(water, shp)# extract polygon intersection between sampling and water surface (sf object)
          sampling_water_sf = st_intersection(water_sf, shp_sf)#convert SpatVect to sf object
          #Reduce the extent of the MHC raster to the extent of UREC
          mhc_clip7 = terra::crop(mhc7, sampling_water) #Needs to be changed depending on which extent we are using
          sampling_water_raster = terra::rasterize(sampling_water, mhc_clip7, 'ID_UEA' ) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
          mhc_mask = terra::mask(mhc_clip7, sampling_water_raster)
          mhc_surf = mhc_mask
          mhc_surf[mhc_surf<3] <- 0 # replace what is not forest by 0 (a tree has to be taller than 3m height)
          mhc_surf[mhc_surf>= 3] <- 1 #replace all values >= 3 to 1 (all cells with trees are given the same value)
    
          sampling_water$srfCan =  exactextractr::exact_extract(mhc_surf, sampling_water_sf, 'sum')
          median_srfCan = median(sampling_water$srfCan, na.rm = T)
          rive$srfCan_m2 = median_srfCan
          rive = terra::project(rive, 'epsg:32198')
          rive$Id_UEA
          rive$Id_rive
          crs(rive)
          writeVector(rive, name, overwrite = T)
          
        }
      }
    }
  }
  
}



