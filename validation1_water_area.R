#Getting new water area for validation
units = st_read("C:/Meghana/Belgique/decembre/data/Validation/validation_units2.shp")
#units1 = units[22,]
####Lets get the bounding box
# bbox1 = st_bbox(units1)
# bbox1_obj = st_as_sfc(bbox1)
#bbox1_obj_v = vect(bbox1_obj)
#Lets get the water
water = st_read("C:/Meghana/Belgique/decembre/data/UREC_water/UREC_wate_smoothr.shp")
water = st_transform(water, st_crs(units))
water = st_crop(water, units)

###Try to find water that touches units1 in order only to cut that unit


# intersection1 = st_intersection(water1, units1)
# query = water$id == '03AG_B2_35_rive11'
# s = filter(water, query)
#
# #i = 1
# library(sf)
# 
# # Create an empty sf object with specified columns
# water_df <- st_sf(
#   data.frame(
#     fid = numeric(),
#     DN = numeric(),
#     Id_UEA = character(),
#     rive = numeric(),
#     id = character(),
#     id_unit = numeric(),
#     rive_unit = numeric(),
#     id_uea_unit = character(),
#     geometry = st_sfc()
#   ),
#   crs = st_crs(32187)
# )
# 
# for (i in 1:nrow(units)) {
#   units1 = units[i,] #extract one units
#   water1 = st_transform(water, st_crs(units1)) #make water and units same projection
#   
#   #find water side that touches our unit
#   intersection1 = st_intersection(water1, units1) #find the intersection betewen water and units
#   if (nrow(intersection1) == 0) {
#     next
#   }
#   else{
#     id = intersection1$id #this is the id of water that intersects or touches our unit
#     query = water$id == id #do a query/filter thing
#     water1 = filter(water, query)
#     water1 = st_crop(water1, units1) #crop water that touches our unit to the extent the of unit1
#     water1$id_unit = units1$id
#     water1$rive_unit = units1$rive
#     water1$id_uea_unit = units1$id_uea
#     water_df = rbind(water_df, water1)
#     
#   }
# }
# 
# st_write(water_df,'C:/Meghana/Belgique/decembre/traitements/Validation1/water_units3.sqlite')
# 


