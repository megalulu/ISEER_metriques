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



