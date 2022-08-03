install.packages("landscapemetrics")
library(landscapemetrics)

#Create functions
string_folder = 'Forest_UREC/'
list_ESA = list_rast_ESA_forest
list_UREC = list_files_UREC_rives

fragstat_function <- function(list_ESA, list_UREC, string_folder, letter_ESA) {
  
  for (i in 1:length(list_UREC)){
  UREC = vect(list_UREC[i])
  UREC_name = list_UREC[i]
  UREC_name=  sub(".*UREC_rives_new/", "", UREC_name)
  UREC_name = sub(".shp.*", "", UREC_name) 
  
  for (r in 1:length(list_ESA)){
    tile = rast(list_ESA[r])
    tile_name = list_ESA[r]
    tile_name = sub(paste0('.*', string_folder), '', tile_name)
    tile_name = sub('.tif.*', '', tile_name)
    
    if (UREC_name == tile_name){
      #Check that input data is properly formated (unit m)
      check = check_landscape(tile, verbose = T)
      #If the input data is not properly formatted, put NA for values in UREC table at primary key 
      #F is for Forest!! NEEDS TO BE CHANGED FOR DIFFERENT TILE TYPES !!!!
      if (check$OK == 'x'){
        UREC[,paste0('shape_mn', letter_ESA)] = -999
        UREC[,paste0('area_mn', letter_ESA)] = -999
        UREC[,paste0('pd', letter_ESA)] = -999
        UREC[,paste0('lpi', letter_ESA)] = -999
        
      }else {
        #Calculate landscape metrics
        shape_mn = lsm_l_shape_mn(tile) #Mean shape Index
        area_mn =  lsm_l_area_mn(tile) #Mean area of patches
        pd = lsm_l_pd(tile) #Patch Density
        lpi = lsm_l_lpi(tile) #Largest Patch index
        #Add values to UREC table based on primary key
        UREC[,paste0('shape_mn', letter_ESA)] = shape_mn$value
        UREC[,paste0('area_mn', letter_ESA)] = area_mn$value
        UREC[,paste0('pd', letter_ESA)]= pd$value
        UREC[,paste0('lpi', letter_ESA)] = lpi$value
        
      }
      writeVector(UREC, filename = paste0('C:/Meghana/Belgique/traitements/FRAGSTAT_test/TBD_UREC_rive_new/', tile_name, '.shp'), overwrite = T)
    }
  }
}
return(UREC)  
}


#list_ESA files:
path_ESA_Forest = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Forest_UREC' #Forest
list_rast_ESA_forest = list.files(path = path_ESA_Forest, pattern = '*.tif', full.names = T) #Forest list of files

path_ESA_Grass = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Grass_UREC' #Grass
list_rast_ESA_Grass = list.files(path = path_ESA_Grass, pattern = '*.tif', full.names = T) #Grass of files

path_ESA_Agriculture = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Agriculture_UREC'
list_rast_ESA_Agriculture = list.files(path = path_ESA_Agriculture, pattern = '*.tif', full.names = T) #Agriculture list of files

path_ESA_Urban = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Urban_UREC'
list_rast_ESA_Urban = list.files(path= path_ESA_Urban, full.names = T)
#list_UREC
path_UREC_rives_TBD = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/TBD_UREC_rive_new'
list_files_UREC_rives_TBD = list.files(path_UREC_rives_TBD, pattern = '*.shp', full.names = T)
#string_folder 
f_folder = 'Forest_UREC/'
g_folder = 'Grass_UREC/'
a_folder = 'Agriculture_UREC/'

#String for letter_ESA
letter_ESA_F = 'F'
letter_ESA_G = 'G'
letter_ESA_A = 'A'


fragstat_function(list_ESA = list_rast_ESA_Grass,
                  list_UREC = list_files_UREC_rives,
                  string_folder = g_folder,
                  letter_ESA = letter_ESA_G
                  )






#Create a list of files for the matrices of landuse cut and mask to a specific land use type 
path_ESA_Forest = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Forest_UREC'
list_rast_ESA_forest = list.files(path = path_ESA_Forest, pattern = '*.tif', full.names = T)

i =1
r= 1
for (i in 1:length(list_files_UREC_rives_TBD)){
  UREC = vect(list_files_UREC_rives_TBD[i])
  UREC_name = list_files_UREC_rives_TBD[i]
  UREC_name=  sub(".*TBD_UREC_rive_new/", "", UREC_name)
  UREC_name = sub(".shp.*", "", UREC_name) 
  name = list_files_UREC_rives_TBD[i]
  
  for (r in 1:length(list_rast_ESA_Grass)){ #Needs to be changed
    tile = rast(list_rast_ESA_Grass[r])  #Needs to be changed
    tile_name = list_rast_ESA_Grass[r]   #Needs to be changed
    tile_name = sub('.*Grass_UREC/', '', tile_name) #Needs to be changed
    tile_name = sub('.tif.*', '', tile_name)
    
    if (UREC_name == tile_name){
      #Check that input data is properly formated (unit m)
      check = check_landscape(tile, verbose = T)
      #If the input data is not properly formatted, put NA for values in UREC table at primary key 
      #F is for Forest!! NEEDS TO BE CHANGED FOR DIFFERENT TILE TYPES !!!!
      if (check$OK == 'x'){

        UREC$shape_mnG = -999  #Needs to be changed
        UREC$area_mnG = -999   #Needs to be changed
        UREC$pdG = -999        #Needs to be changed
        UREC$lpiG = -999       #Needs to be changed
      }else {
        #Calculate landscape metrics
        shape_mn = lsm_l_shape_mn(tile) #Mean shape Index
        area_mn =  lsm_l_area_mn(tile) #Mean area of patches
        pd = lsm_l_pd(tile) #Patch Density
        lpi = lsm_l_lpi(tile) #Largest Patch index
        #Add values to UREC table based on primary key
        UREC$shape_mnG = shape_mn$value     #Needs to be changed
        UREC$area_mnG = area_mn$value       #Needs to be changed
        UREC$pdG = pd$value                 #Needs to be changed      
        UREC$lpiG = lpi$value               #Needs to be changed
        
        
      }
      
      writeVector(x= UREC, filename = name, filetype = 'ESRI Shapefile', overwrite = T)
    }
  }
}

#########################################################################################################################################

#Check that input data is properly formated (unit m)
check_landscape(tile, verbose = T)

#Calculate landscape metrics
lsm_l_shape_mn(tile) #Mean shape Index
lsm_l_area_mn(tile) #Mean area of patches
lsm_l_pd(tile) #Patch Density
lsm_l_lpi(tile) #Largest Patch index


test_esa1 = terra::rast('C:/Meghana/Belgique/traitements/FRAGSTAT_test/data/ESA_Clip_UREC_merge_NAD83.tif')
r = 1
i = 1
for (r in 1:length(list_files_UREC_rives)) 
{
  shp = vect(list_files_UREC_rives[r])
  mask_file_name = paste0('C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/ESA_UREC/',
                          'ESA_',
                         shp$Id_UEA,
                         '_rive',
                         shp$Id_rive,
                         '.tif')
  shp = terra::project(shp, test_esa1) # project vector UREC file to same projection as ESA raster file (WGS84)
  esa_clip = terra::crop(test_esa1, shp) #Reduce the extent of the ESA raster to the extent of UREC
  shp_raster = terra::rasterize(shp, esa_clip) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
  esa_mask = terra::mask(esa_clip, shp_raster)  #create mask on raster with UREC shapefile
  terra::writeRaster(esa_mask, filename = mask_file_name, overwrite = T)
}


#################################################################################
#TRYING OUT FUNCTIONS IN R

addition_function <- function(x,y){
  add = x+y
  return(add)
  
}

addition_function(2,5)
  