#MaxCAM call and test functions

#Source file with functions : metric_functions.R
source('metric_functions.R')

#Set variables 
#Set variables for paths and files linked to UREC
#path_UREC_rive = 'C:/Meghana/Belgique/traitements/UREC_MaxCam/UREC_MaxCam_rive/' #path towards folder with individual UREC shapefiles
path_UREC_sampling = 'C:/Meghana/Belgique/decembre/data/sampling/' #path towards folder with individual UREC sampling shapefiles (made previously)
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_merge.shp') #read UREC file with all URECs merged. This is where we will put the results of our metrics

#Set variables files and paths linked to raster files (land use/ Utilisation du territoire)
ut19_Veg = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_Veg.tif') #this raster file has been masked to only show Vegetation (Foret, MH, Agriculture)
ut19_Forest = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_Foret.tif') ##this raster file has been masked to only show Vegetation (Foret)
ut19_Agriculture =  raster('C:/Meghana/Belgique/decembre/data/raster/ut19_Agriculture.tif') ##this raster file has been masked to only show Vegetation (Agriculture)
ut19_MH = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_MH.tif')##this raster file has been masked to only show Vegetation (MH)
  


#Call functions and add results to a new variable 
#Connectivity and fragementation metric functions for UT Vegetation
results = continuity_metric(UREC_full = UREC_merge, path_sampling = path_UREC_sampling,  raster_UT = ut19_Veg, classe_UT = 'Vegetation' )
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp')

results = PDV_fragementation_metric_function(UREC_full= results, raster_file = ut19_Veg, col_name = 'Vegetation')
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp', delete_layer = T)
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.sqlite', delete_layer = T)
#Connectivity and fragementation metric functions for UT Forest
results = continuity_metric(UREC_full = results, path_sampling = path_UREC_sampling,  raster_UT = ut19_Forest, classe_UT = 'Forest' )
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp', delete_layer = T)
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.sqlite', delete_layer = T)

results = PDV_fragementation_metric_function(UREC_full= results, raster_file = ut19_Forest, col_name = 'Forest')
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp', delete_layer = T)
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.sqlite', delete_layer = T)

#Connectivity and fragementation metric functions for UT Agriculture
results = continuity_metric(UREC_full = results, path_sampling = path_UREC_sampling,  raster_UT = ut19_Agriculture, classe_UT = 'Agriculture' )
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp', delete_layer = T)
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.sqlite', delete_layer = T)

results = PDV_fragementation_metric_function(UREC_full= results, raster_file = ut19_Agriculture, col_name = 'Agriculture')
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp', delete_layer = T)
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.sqlite', delete_layer = T)

#Connectivity and fragementation metric functions for UT MH
results = continuity_metric(UREC_full = results, path_sampling = path_UREC_sampling,  raster_UT = ut19_MH, classe_UT = 'MH' )
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp', delete_layer = T)
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.sqlite', delete_layer = T)
results = PDV_fragementation_metric_function(UREC_full= results, raster_file = ut19_MH, col_name = 'MH')
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp', delete_layer = T)
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.sqlite', delete_layer = T)

#Write out results to results folder
st_write(results, 'C:/Meghana/Belgique/traitements/UREC_MaxCam/results_MaxCam1.shp') #write with ESRI drive (this can be changed to SQLITE driver to keep column names)

#############################################################################
#Load UREC_merge
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_merge.shp')





#############################################################################
#Some preprocessing for getting data ready
path_sampling = 'C:/Meghana/Belgique/traitements/results/sampling_rives/'
list_files_sampling = list.files(path_sampling, pattern = '*.shp', full.names = T)
i=1
for (i in 1:length(list_files_sampling)){
  sampling = st_read(list_files_sampling[i])
  sampling$Id_UEA = sampling$ID_UEA
  sampling$rive = sampling$Id_rive
  sampling$id = paste0(sampling$Id_UEA, '_rive', sampling$rive)
  sampling = subset(sampling, select = c(Id_UEA, rive, id) )
  file_name = paste0('C:/Meghana/Belgique/decembre/data/sampling/', sampling$id[1], '.shp')
  st_write(sampling,file_name)
  
  
}
  

                     





