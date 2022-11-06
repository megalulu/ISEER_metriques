#########################################
#NEEDS TO BE ADDED TO METRICS
#     Metrics of proportion of land uses over total area
#### !!!! DIFFERENT THAN CONTINUITY of land use

path_esa_UREC = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/ESA_UREC/'
list_files_esa_UREC = list.files(path_esa_UREC, pattern = '.*tif', full.names = T)
r = 1
i=1
#Do for loop to extract only urban land use and write out tif file.
for (r in 1:length(list_files_UREC_rives)) {
  shp = st_read(list_files_UREC_rives[r])
  shp_name  = list_files_UREC_rives[r]
  shp_name = sub(paste0('.*', 'UREC_rives_new/'), '', shp_name)
  shp_name = sub('.shp.*', '', shp_name)
  shp_file_name = paste0(path_name_UREC_rives,
                         shp$Id_UEA,
                         '_rive',
                         shp$Id_rive,
                         '.shp')
  for (i in 1:length(list_files_esa_UREC)) {
    tile = rast(list_files_esa_UREC[i])
    tile_name = list_files_esa_UREC[i]
    tile_name = sub(paste0('.*', 'ESA_UREC/ESA_'), '', tile_name)
    tile_name = sub('.tif.*', '', tile_name)

    if (tile_name == shp_name) {
      
      #Create a forest mask
      tile_F = tile
      tile_F[tile_F != 10] <- NA # replace what is not forest by NA
      tile_F[tile_F == 10] <- 1 # replace 10 (forest) by 1
      #create grassland mask
      tile_G = tile
      tile_G[tile_G != 30] <- NA # replace what is not greassland by NA
      tile_G[tile_G == 30] <- 1 # replace 30 (grassland) by 1
      #Create an urban mask 
      tile_U = tile
      tile_U[tile_U != 50] <- NA # replace what is not urban (built-up) by NA
      tile_U[tile_U == 50] <- 1 # replace 50 (built-up) by 1
      #Create an agriculture mask 
      tile_A = tile
      tile_A[tile_A != 40] <- NA # replace what is not agriculture (cropland) by NA
      tile_A[tile_A == 40] <- 1 # replace 40 (cropland) by 1
      
      
      
      #Extract the surface of forested/ grassland/urban/agriculture pixels in each UREC to get other metric
      shp$surface_full_F = exactextractr::exact_extract(tile_F, shp, 'sum') *10 #multiply sum by area of each pixel (meed to do this with sf object!)
      shp$surface_full_G = exactextractr::exact_extract(tile_G, shp, 'sum') *10
      shp$surface_full_U = exactextractr::exact_extract(tile_U, shp, 'sum') *10
      shp$surface_full_A = exactextractr::exact_extract(tile_A, shp, 'sum') *10
      
      shp$area = as.numeric(st_area(shp)) #calculate the area of each sampling unit in the UREC
      shp$surface_propF = shp$surface_full_F / shp$area # get the ratio of forested area in each sampling unit by the area of the sampling unit
      shp$surface_propG = shp$surface_full_G / shp$area # get the ratio of grassland area in each sampling unit by the area of the sampling unit
      shp$surface_propU = shp$surface_full_U / shp$area # get the ratio of urban area in each sampling unit by the area of the sampling unit
      shp$surface_propA = shp$surface_full_A / shp$area # get the ratio of urban area in each sampling unit by the area of the sampling unit
      
      st_write(shp, shp_file_name, delete_layer = TRUE) #write shapefiles with new data
      
    }
  }
}
