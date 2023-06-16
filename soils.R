#TESTS ON SOIL TEXTURE AND HYDRAULIC CONDUCTIVITY

##################################################################
#Pretraitement SIIGSOL
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
#Limon
limon = terra::rast('C:/Meghana/donnee_brutes/SIIGSOL/limon_fr_siigsol/limon_fr_siigsol.tif')
limon_crop =  terra::crop(limon, UREC_merge)
writeRaster(limon_crop,'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/limon_crop.tif' )
#Argile
argile = terra::rast('C:/Meghana/donnee_brutes/SIIGSOL/argile_fr_siigsol/argile_fr_siigsol.tif')
argile_crop =  terra::crop(argile, UREC_merge)
writeRaster(argile_crop,'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/argile_crop.tif' )
#Sable
sable = terra::rast('C:/Meghana/donnee_brutes/SIIGSOL/sable_fr_siigsol/sable_fr_siigsol.tif')
sable_crop =  terra::crop(sable, UREC_merge)
writeRaster(sable_crop,'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/sable_crop.tif' )

########Étape 1 : créer un raster avec les moyenne hydraulique par pixel 
#Load data
texture = read.csv(file = 'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/Classes_textures3.csv', header = TRUE, sep = ';')
r1_texture = rast('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/Soil Texture_band1.sdat')                   
r2_texture = rast('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/Soil Texture_band2.sdat')                   
r3_texture = rast('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/Soil Texture_band3.sdat')                   
r4_texture = rast('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/Soil Texture_band4.sdat')                   
r5_texture = rast('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/Soil Texture_band5.sdat')                   
r6_texture = rast('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/Soil Texture_band6.sdat')                   
###################################################
#Join texture_csv with raster 
#r1_texture
  r1_texture_hc = t(matrix(as.numeric(texture[,"Hydraulic.Conductivity"][as.vector(r1_texture)+1]), nrow = 1507, ncol = 2052))  
reclass_r1_texture_hc <-
  terra::rast(
    r1_texture_hc,
    crs = crs(r1_texture),
    extent = terra::ext(r1_texture)
  )
#r2_texture
r2_texture_hc = t(matrix(as.numeric(texture[,"Hydraulic.Conductivity"][as.vector(r2_texture)+1]), nrow = 1507, ncol = 2052))
reclass_r2_texture_hc <-
  terra::rast(r2_texture_hc, extent = ext(r2_texture), crs = crs(r2_texture)
  )
class(reclass_r2_texture_hc)

#r3_texture
r3_texture_hc = t(matrix(as.numeric(texture[,"Hydraulic.Conductivity"][as.vector(r3_texture)+1]), nrow = 1507, ncol = 2052))
reclass_r3_texture_hc <-
  terra::rast(
    r3_texture_hc,
    crs = crs(r3_texture),
    extent = terra::ext(r3_texture)
  )

#r4_texture
r4_texture_hc = t(matrix(as.numeric(texture[,"Hydraulic.Conductivity"][as.vector(r4_texture)+1]), nrow = 1507, ncol = 2052))
reclass_r4_texture_hc <-
  terra::rast(
    r4_texture_hc,
    crs = crs(r4_texture),
    extent = terra::ext(r4_texture)
  )

#r5_texture
r5_texture_hc = t(matrix(as.numeric(texture[,"Hydraulic.Conductivity"][as.vector(r5_texture)+1]), nrow = 1507, ncol = 2052))
reclass_r5_texture_hc <-
  terra::rast(
    r5_texture_hc,
    crs = crs(r5_texture),
    extent = terra::ext(r5_texture)
  )

#r6_texture
r6_texture_hc = t(matrix(as.numeric(texture[,"Hydraulic.Conductivity"][as.vector(r6_texture)+1]), nrow = 1507, ncol = 2052))
reclass_r6_texture_hc <-
  terra::rast(
    r6_texture_hc,
    crs = crs(r6_texture),
    extent = terra::ext(r6_texture)
  )

#combining rasters
avr_Hc = mean(reclass_r1_texture_hc, reclass_r2_texture_hc, reclass_r3_texture_hc, reclass_r4_texture_hc, reclass_r5_texture_hc, reclass_r6_texture_hc)
writeRaster(avr_Hc,'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/avr_HydraulicConductivity.tif')
 

#Create for loop to extract median value per UREC
UREC_HC = UREC_merge

i =1

for (i in 1:nrow(UREC_HC)){
  print(UREC_HC[i,])
  shp = UREC_HC[i,]
  HC = exact_extract(avr_Hc, shp, 'median')#Extract median value per UREC of average hydrolic conductivity in the soil 'column' (i.e 1 pixel)
  UREC_HC[i,'HydCond'] = HC #add data to UREC file
}

#Write results
st_write(UREC_HC, 'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/results_urec_hydrocond.shp')
st_write(UREC_HC, 'C:/Meghana/Belgique/decembre/traitements/results_urec_hydrocond.shp')

###Remove mask urbain from SIIGSOL 
#Any pixel under the mask should be given a value of 0

#Create for loop incorparinting this new set of rules
#Load urbain_raster mask layer
avr_Hc = raster('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/avr_HydraulicConductivity.tif')
mask_urbain1 = raster('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/mask_ urbain_HydCon1.tif')
mask_urbain1_t = extend(mask_urbain1, avr_Hc)
UREC_Hc_urb = UREC_merge
UREC_hc_urb = st_transform(UREC_Hc_urb, st_crs(mask_urbain1))
i=69

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
keep=c("id_uea", "rive",'id','HydCond')
UREC_Hc_urb_1 = UREC_Hc_urb[UREC_Hc_urb,names(UREC_Hc_urb)%in%keep]
st_write(UREC_Hc_urb_1, 'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/results_urec_hydrocond_maskUrb.shp')
st_write(UREC_Hc_urb_1, 'C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/results_urec_hydrocond_maskUrb.sqlite')


