#TEST 2 : Pente avec transect
#100m
transect = st_read('C:/Meghana/Belgique/decembre/traitements/tests_pentes/transects_100_test_riveD.shp')
transect_points = st_read('C:/Meghana/Belgique/decembre/traitements/tests_pentes/points2_sampled.shp')
#500m
transect_500 = st_read('C:/Meghana/Belgique/decembre/traitements/tests_pentes/transects_500_test_riveD.shp')
transect500_points = st_read('C:/Meghana/Belgique/decembre/traitements/tests_pentes/points500_sampled.shp')
#50m 
transect_50 = st_read('C:/Meghana/Belgique/decembre/traitements/tests_pentes/transects_50_test_riveD.shp')
transect50_points = st_read('C:/Meghana/Belgique/decembre/traitements/tests_pentes/points50_sampled1.shp')
#25m 
transect_25 = st_read('C:/Meghana/Belgique/decembre/traitements/tests_pentes/transects_25_test_riveD.shp')
transect25_points = st_read('C:/Meghana/Belgique/decembre/traitements/tests_pentes/points25_sampled.shp')



# grp1  = transect_points %>%
#   grou_by(Trans_Id) %>%
#   summarize(h = (Trans_Id[vertex_pos == -1]))

#grp_trns_pts = transect_points %>% group_by(by = c('Trans_Id')) %>%summarize(m = mean(SAMPLE_1))
i = 2

####FOR LOOP THING
# new_transect = transect
# #new_transect = new_transect[1:2,]
# new_transect$pente = NA

#transect_points = transect_points[1:4,]
i = 13
r = 7
# SlopeTransectFunction <-function(new_transect, transect_points){
#   new_transect$pente = NA
#   print('Calculating mean slope')
#   for (i in 1:nrow(transect_points)) {
#     if ((i %% 2) == 1) {
#       #check if i is  odd
#       row1 = transect_points[i,]
#       for (r in 1:nrow(new_transect)) {
#         line = new_transect[r,]
#         if (row1$Trans_Id == line$Trans_Id) { #Check if transect Id are the same in both tables
#           elevation1 = row1$SAMPLE_1
#           row2 = transect_points[i + 1,]
#           elevation2 = row2$SAMPLE_1
#           h = elevation1 - elevation2
#           #h = abs(row1$SAMPLE_1 - row2$SAMPLE_1)
#           if(row2$vertex_pos == -1){
#             d = row2$distance_1
#           }else{
#             d = row1$distance_1
#           }
#           
#           height = h / d
#           angle = atan(height) * 180 / pi #Angle in degrees
#           new_transect[r,]$pente = angle
#           
#         }
#         
#       }
#       
#     }
#   }
#   mean_slope = mean(new_transect$pente, na.rm = T)
#   print(mean_slope)
# }
# 
# 
# mean_slop_100 = SlopeTransectFunction(new_transect, transect_points)
# mean_slop_500 = SlopeTransectFunction(new_transect= transect_500, transect_points = transect500_points)
# mean_slop_50 = SlopeTransectFunction(new_transect= transect_50, transect_points = transect50_points)
# mean_slop_25 = SlopeTransectFunction(new_transect= transect_25, transect_points = transect25_points)
# #Conclusion: none of these test converge 
# #this methode can be abadonned 
# 
# #########################################################################
# #Test slop mean with exact extract
# mnt_t = raster('C:/Meghana/Belgique/decembre/traitements/tests_pentes/Pentes_31H09NE.tif')
# UREC_merge_t = st_read('C:/Meghana/Belgique/decembre/traitements/tests_pentes/UREC_test.shp')
# UREC_merge_t = st_transform(UREC_merge_t,'EPSG:2950') #change projection of feature to projection of raster
# test_pente = exactextractr::exact_extract(mnt_t, UREC_merge_t, 'mean')

#previous try works. Need to find out what unit 'pente' derive du LiDAR is in. Otherwise create our new Pentes with MNT
i =1
UREC_merge = UREC_mtm7
slope_raster = penteMTM7_raster


####Create VRT of slope data!!!
# pente_files = list.files(path = 'C:/Meghana/donnee_brutes/LIDAR/Pentes/MTM7', pattern = '*.tif', 
#                          full.names = T )
# output = 'C:/Meghana/donnee_brutes/LIDAR/Pentes/pente_mtm7.vrt'
# terra::vrt(pente_files, output, overwrite = T)

#Load pente MTM7 et MTM8 avec rast
penteMTM7_raster = rast('C:/Meghana/donnee_brutes/LIDAR/Pentes/pente_mtm7.vrt')
penteMTM8_raster = rast('C:/Meghana/donnee_brutes/LIDAR/Pentes/pente_mtm8.vrt')
UREC_merge = vect('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
urbain1 = st_read('C:/Meghana/Belgique/decembre/traitements/ut_urbain_vect.shp')
urbain1 = st_transform(urbain1,st_crs(penteMTM8_raster ))

#Cut UREC_merge to the extent of pente rasters
#UREC_mtm7 = terra::project(UREC_merge, crs(penteMTM7_raster))
#UREC_mtm7 = terra::crop(UREC_mtm7, penteMTM7_raster)
UREC_mtm7 = vect('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_MTM7.shp')

#run slope function
mean_slopeMTM7 = AverageSlope(UREC_merge = UREC_mtm7, slope_raster = penteMTM7_raster)

#Cut UREC_merge to the extent of pente rasters
 # UREC_mtm8 = terra::project(UREC_merge, crs(penteMTM8_raster))
 # UREC_mtm8 = terra::crop(UREC_mtm8, penteMTM8_raster)
UREC_mtm8 = vect('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_MTM8.shp')
UREC_mtm8=UREC_mtm8[8,]
# #run slope function
 mean_slopeMTM8 = AverageSlope(UREC_merge = UREC_mtm8, slope_raster = penteMTM8_raster)
 mean_slopeMTM8_urb = AverageSlope(UREC_merge = UREC_mtm8, slope_raster = penteMTM8_raster, mask_urbain = urbain1)
 
 
 
#Combine mean_slopeMTM8 et 7 into one object
 UREC_merge_pente = rbind(mean_slopeMTM7, mean_slopeMTM8)
 writeVector(UREC_merge_pente, 'C:/Meghana/Belgique/decembre/traitements/UREC_slope.shp')
 writeVector(UREC_merge_pente, 'C:/Meghana/Belgique/decembre/traitements/UREC_slope.sqlite', filetype = 'SQLITE')
 