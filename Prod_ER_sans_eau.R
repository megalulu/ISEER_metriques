#Prod_ER_sans eau


UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water.shp')

#Surface des UT sans la zones eau sont deja calculer dans maintient et creation d'habitat
#Load data 
UREC_surface = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/UREC_surfaces_UT_sans_eau.sqlite')#This seeems wrong...
##New set of surface classe 
ut19_full= raster('C:/Meghana/Belgique/decembre/data/raster/ut19_mask.tif')
csv__class_correspondence = read.csv2("C:/Meghana/donnee_brutes/UT_2019_10m.csv")
UREC_merge = UREC_merge[145,]
UREC_surface_class = SurfaceClass(UREC_merge = UREC_merge, csv_class_correspondence = csv__class_correspondence,
                            raster_file = ut19_full)

#####
#Conductivite hydraulique sans eau
#####################################
avr_Hc = raster('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/avr_HydraulicConductivity.tif')
mask_urbain1 = raster('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/mask_ urbain_HydCon1.tif')
mask_urbain1_t = extend(mask_urbain1, avr_Hc)
UREC_Hc_urb = UREC_merge
UREC_hc_urb = st_transform(UREC_Hc_urb, st_crs(mask_urbain1))

for (i in 1:nrow(UREC_Hc_urb)){
  print(UREC_Hc_urb[i,])
  urec_sub = UREC_Hc_urb[i,]
  avr_Hc_clip = crop(avr_Hc,urec_sub)#avr_hc to extent of urec_sub
  avr_hc_clip_mask = terra::mask(avr_Hc_clip, urec_sub) #only keep values that are in the unit
  
  #crop raster mask urbain to size of sub unit
  mask_urbain1_clip = crop(mask_urbain1_t, urec_sub)
  #Mask avr_hc_clip_mask with cropped urbain1_clip but use inverse = T and update values of inverse 
  avr_hc_clip_mask_urb = terra::mask(avr_hc_clip_mask, mask_urbain1_clip, inverse = T, updatevalue = 0)
  HC_t = exact_extract(avr_hc_clip_mask_urb,urec_sub , 'median')#Extract median value per UREC of average hydrolic conductivity in the soil 'column' (i.e 1 pixel)
  UREC_Hc_urb[i,'HydCond'] = HC_t #add data to UREC file
}
UREC_Hc_urb$rive = UREC_Hc_urb$rive.x
UREC_Hc_urb[is.na(UREC_Hc_urb)]<-0
st_write(UREC_Hc_urb, 'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/siig_sol_sans_eau/urec_hydrocond_maskUrb_sans_eau.shp')
st_write(UREC_Hc_urb, 'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/siig_sol_sans_eau/urec_hydrocond_maskUrb_sans_eau.sqlite')

#####
#Pente lateral 
#################################
#Load raster of slopes 
pentes_mtm8 = raster('C:/Meghana/donnee_brutes/LIDAR/Pentes/pente_mtm8.vrt')
UREC_merge_v = vect('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_wtaer_MTM8.shp')
urec_pente_mtm8 = AverageSlope(UREC_merge =UREC_merge_v, slope_raster = pentes_mtm8 )
writeVector(urec_pente_mtm8, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/urec_pente_mtm8.sqlite', filetype = 'SQLITE')

pentes_mtm7 = raster('C:/Meghana/donnee_brutes/LIDAR/Pentes/pente_mtm7.vrt')
UREC_merge_v = vect('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water_MTM7.shp')
urec_pente_mtm7 = AverageSlope(UREC_merge =UREC_merge_v, slope_raster = pentes_mtm7 )
writeVector(urec_pente_mtm7, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/urec_pente_mtm7.sqlite', filetype = 'SQLITE')
#Put them together
urec_pente_mtm8 = terra::project(urec_pente_mtm8,'EPSG: 32198' ) #transform into one projection 
urec_pente_mtm7 = terra::project(urec_pente_mtm7,'EPSG: 32198' ) #transform into one projection 

urec_pente =rbind(urec_pente_mtm7, urec_pente_mtm8)
writeVector(urec_pente, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/urec_pente.sqlite', filetype = 'SQLITE')



#####
#HEPB
#####################################
#Load data 
#load virtual rasters previously created from external hard drive
mnt_8 = rast('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM8/mnt_mtm8.vrt')
mnt_7 = rast('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM7/mnt_mtm7.vrt')



UREC_merge_v8 = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_wtaer_MTM8.shp')
UREC_merge_v8 = st_transform(UREC_merge_v8, st_crs(mnt_8))

UREC_merge_v7 = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water_MTM7.shp')
UREC_merge_v7 = st_transform(UREC_merge_v7, st_crs(mnt_7))

UREC_merge_mtm = UREC_merge_v7
UREC_merge_mtm$hepb = NA

urbain1 = st_read('C:/Meghana/Belgique/decembre/traitements/ut_urbain_vect.shp')
urbain1 = st_transform(urbain1,st_crs(UREC_merge_mtm ))

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
  #mask urbain area and assign all pixels with urbain things a value of 0
  mnt_mask_urb = terra::mask(mnt_min, urbain1, inverse = T, updatevalue = 0)
  
  #writeRaster(mnt_min,paste0('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM7/clip_mtm7/', rive_id, '.tif'), overwrite = T)
  values_mnt_mask = values(mnt_min)
  hePb = median(values_mnt_mask, na.rm = T)
  # hePb = median(values(mnt_min, na.rm = T))
  UREC_merge_mtm[i, ]$hepb = hePb
  rm(mnt_min)
  rm(rive_raster)
  
}

st_write(UREC_merge_mtm, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/urec_hep_mtm8.sqlite', filetype = 'SQLITE' )
UREC_merge_mtm8 = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/urec_hep_mtm8.sqlite')
UREC_merge_mtm8 = st_transform(UREC_merge_mtm8, st_crs(UREC_merge))
UREC_merge_mtm7=UREC_merge_mtm
st_write(UREC_merge_mtm7, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/urec_hep_mtm7.sqlite', filetype = 'SQLITE' )
UREC_merge_mtm7 = st_read( 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/urec_hep_mtm7.sqlite')
UREC_merge_mtm7 = st_transform(UREC_merge_mtm7, st_crs(UREC_merge))

UREC_hepb = rbind(UREC_merge_mtm7, UREC_merge_mtm8)
st_write(UREC_hepb, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/urec_hepb_full.sqlite' )


########################################################