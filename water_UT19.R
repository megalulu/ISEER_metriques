#Shade on water surface 
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
UREC_water_merge = st_read('C:/Meghana/Belgique/decembre/data/water_fromUT_dissolved.shp')#new water surface for all UREC made with UT2019


# i = 1
# rive = UREC_merge[i,]
# water = st_crop(UREC_water_merge, rive)
# UREC_water = st_intersection(water, rive)
# UREC_water

# 
# for (i in 2:nrow(UREC_merge)){
#   rive = UREC_merge[i,]
#   water = st_crop(UREC_water_merge, rive)
#   water = st_intersection(water, rive)
#   is_empty_geometry <- any(st_is_empty(water$geometry[1]))
#   
#   if(is_empty_geometry == FALSE){
#     water$id = rive$id
#     UREC_water = rbind(UREC_water, water)
#   }
# }
# st_write(UREC_water, 'C:/Meghana/Belgique/decembre/data/UREC_water/UREC_water.shp')
# st_write(UREC_water, 'C:/Meghana/Belgique/decembre/data/UREC_water/UREC_water.sqlite')


# ########################################################
# #Calcul de la canopee en surplomb sur la riviere 
# #Load mhc data 
# mhc_vrt7 = rast('C:/Meghana/Belgique/decembre/traitements/MHC_mask/mhc7_mask.vrt')
# #Load UREC water if not already in environment
# UREC_water = st_read('C:/Meghana/Belgique/decembre/data/UREC_water/UREC_water.sqlite')
# UREC_water = vect(UREC_water)
# UREC_water = terra::project(UREC_water, crs(mhc_vrt7))#reproject to raster file
# UREC_water = terra::crop(UREC_water, ext(mhc_vrt7))
# 
# 
# EPSG = "EPSG:2949"
# ut19_Urbain_vec = vect('C:/Meghana/Belgique/decembre/data/polygon_UT_urban.shp')
# ut19_Urbain_vec = terra::project(ut19_Urbain_vec, crs(mhc_vrt7))
# 
# UREC_temp7 = OverhangingCanopy(UREC_water = UREC_water, raster_file = mhc_vrt7, EPSG = EPSG, urban_vector_mask = ut19_Urbain_vec)
# writeVector(UREC_temp7, 'C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_MTM7.shp')
# writeVector(UREC_temp7, 'C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_MTM7.sqlite', filetype = 'SQLITE')
# 
# #Do the same but for MTM8 
# mhc_vrt8 = rast('C:/Meghana/Belgique/decembre/traitements/MHC_mask/MTM8/mhc_mtm8_new1.vrt')
# UREC_water = st_read('C:/Meghana/Belgique/decembre/data/UREC_water/UREC_water.sqlite')
# UREC_water = vect(UREC_water)
# UREC_water = terra::project(UREC_water, crs(mhc_vrt8))#reproject to raster file
# UREC_water = terra::crop(UREC_water, ext(mhc_vrt8))
# 
# EPSG = "EPSG:2950"
# ut19_Urbain_vec = vect('C:/Meghana/Belgique/decembre/data/polygon_UT_urban.shp')
# ut19_Urbain_vec = terra::project(ut19_Urbain_vec, crs(mhc_vrt8))
# 
# UREC_temp8 = OverhangingCanopy(UREC_water = UREC_water, raster_file = mhc_vrt8, EPSG = EPSG, urban_vector_mask = ut19_Urbain_vec)
# writeVector(UREC_temp8, 'C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_MTM8.shp')
# writeVector(UREC_temp8, 'C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_MTM8.sqlite', filetype = 'SQLITE')
# 
# 
#Merge data together
UREC_merge_mtm7 = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water_MTM7.shp')
UREC_temp7 = st_read('C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_MTM7.sqlite')
UREC_temp7= st_transform(UREC_temp7, st_crs(UREC_merge_mtm7))
UREC_temp7 = st_drop_geometry(UREC_temp7)
UREC_merge_mtm7 = left_join(UREC_merge_mtm7,UREC_temp7, by = 'id' )
sum(is.na(UREC_merge_mtm7$canopyratio))
UREC_merge_mtm7$Can7 = UREC_merge_mtm7$canopyratio
UREC_merge_mtm7 = st_drop_geometry(UREC_merge_mtm7)
drop_temp = c('rive.x', 'rive.y', 'fid', 'dn', 'canopyratio', 'Id_UEA')
UREC_merge_mtm7 =UREC_merge_mtm7[,!names(UREC_merge_mtm7)%in% drop_temp]
names(UREC_merge_mtm7)




UREC_merge_mtm8 = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_wtaer_MTM8.shp')
UREC_temp8 = st_read('C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_MTM8.sqlite')
UREC_temp8= st_transform(UREC_temp8, st_crs(UREC_merge_mtm8))
UREC_temp8 = st_drop_geometry(UREC_temp8)
UREC_merge_mtm8 = left_join(UREC_merge_mtm8,UREC_temp8, by = 'id' )
sum(is.na(UREC_merge_mtm8$canopyratio))
UREC_merge_mtm8$Can8 = UREC_merge_mtm8$canopyratio
UREC_merge_mtm8 = st_drop_geometry(UREC_merge_mtm8)
names(UREC_merge_mtm8)
drop_temp = c('rive.x', 'rive.y', 'fid', 'dn', 'canopyratio', 'Id_UEA')
UREC_merge_mtm8 =UREC_merge_mtm8[,!names(UREC_merge_mtm8)%in% drop_temp]

UREC_merge =  st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water.shp')
UREC_temp = UREC_merge

UREC_temp = left_join(UREC_merge,UREC_merge_mtm8, by = 'id' )


 UREC_temp = left_join(UREC_temp,UREC_merge_mtm7, by = 'id' )

UREC_temp$CanRatio = NA

i=1
for(i in 1:nrow(UREC_temp))
{
  urec = UREC_temp[i, ]
  temp7 = urec$Can7
  temp8 = urec$Can8
  if (is.na(temp7) & is.na(temp8)) {
    UREC_temp[i,]$CanRatio = NA}
    else {
      if (is.na(temp7)) {
        UREC_temp[i,]$CanRatio = temp8
        
        
      }
      UREC_temp[i,]$CanRatio = temp7
      
    }
  }

UREC_temp$id_uea = UREC_temp$Id_UEA
drop_temp = c('Id_UEA','id_uea.x', 'id_uea.y')
UREC_temp = UREC_temp[, !names(UREC_temp)%in%drop_temp ]

sum(is.na(UREC_temp$CanRatio)) 
st_write(UREC_temp,'C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_full.sqlite', delete_layer= T  )
st_write(UREC_temp,'C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_full.shp',  delete_layer= T )


####Normalize data and create indiex
UREC_temp_nrm = Normalization_function(UREC_merge = UREC_temp)
names(UREC_temp_nrm)
st_write(UREC_temp,'C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_full_nrm.shp',  delete_layer= T )
st_write(UREC_temp,'C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_full_nrm.sqlite',  delete_layer= T )

windows(10,5)
plot(UREC_temp_nrm$CanRatio_nrm)

