#Scripts of step to follow

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
  

################################################################################
# Calculate metrics on UREC_merge
###############################################################################
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
st_write(results, 'C:/Meghana/Belgique/decembre/results/output2.shp', delete_layer = T)
st_write(results, dsn = "C:/Meghana/Belgique/decembre/results/",  layer = 'output2.sqlite', driver = 'SQLITE', delete_layer = T)

#Connectivity and fragementation metric functions for UT MH
results = continuity_metric(UREC_full = results, path_sampling = path_UREC_sampling,  raster_UT = ut19_MH, classe_UT = 'MH' )
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp', delete_layer = T)
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.sqlite', delete_layer = T)
results = PDV_fragementation_metric_function(UREC_full= results, raster_file = ut19_MH, col_name = 'MH')
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp', delete_layer = T)
st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.sqlite', delete_layer = T)




results_new = subset(results, select=-c(pd_mh, area_mh, pd_forest,area_forest, pd_agriculture, area_agriculture))
st_write(results_new, 'C:/Meghana/Belgique/decembre/results/output2.shp', delete_layer = T)
st_write(results_new, 'C:/Meghana/Belgique/decembre/results/output2.SQLite', driver = 'SQLITE')


################################################################################
#Normalization_functions
################################################################################
source('Normalization_functions.R')
results_new = st_read('C:/Meghana/Belgique/decembre/results/output2.sqlite') #load UREC_merge with metrics s if not already loaded 

results_norm = Normalization_function(UREC_merge = results_new)
st_write(results_norm, 'C:/Meghana/Belgique/decembre/results/output2_norm.shp', delete_layer = T)
st_write(results_norm, 'C:/Meghana/Belgique/decembre/results/output2_norm.SQLite', driver = 'SQLITE')



################################################################################
#Do statistics to find least correlated metrics
################################################################################
source('Statistics_new.R')

#Load UREC_norm as vect data 

ContH_vec =  vect('C:/Meghana/Belgique/decembre/results/output2_norm.SQLite')

#Select only columns you are interested in 
stats_contH1 = ContH_vec[,c("id_uea", "rive", "id","continuity_vegetation_nrm",
             "area_vegetation_nrm" ,
             "pd_vegetation_nrm",
             "continuity_forest_nrm",
             "continuity_agriculture_nrm",
             "continuity_mh_nrm",
             "area_forest_nrm",
             "pd_forest_nrm" ,
             "area_agriculture_nrm",
             "pd_agriculture_nrm",
             "area_mh_nrm",
             "pd_mh_nrm")]

#Run correlation matrix on all data
corr_contH1 = Correlation_matrix(df = stats_contH1, var2 = 'ContPaysage') 

#Create new data set by Select only variable you need more information on to make a choise (i.e. Variables correlation at more than 0.6 and that can be immediately eliminated)
stats_contH2 = ContH_vec[, c("id_uea", "rive",'id', 'pd_forest_nrm', 'area_forest_nrm')] #here only two variables need to be tested 
#Do ACP on these pairs of variables that are correlated and select one
pca_contH2 = PCA_graph_function(df = stats_contH2, df_name = 'ContPaysageH2', axe = c(1,2))

#Create final dataset with only variables to be included in index of ecological function 
stats_contH3 = ContH_vec[,c("id_uea", "rive", "id","continuity_vegetation_nrm",
                            "area_vegetation_nrm" ,
                            "continuity_forest_nrm",
                            "continuity_agriculture_nrm",
                            "continuity_mh_nrm",
                            "pd_forest_nrm" ,
                            "area_agriculture_nrm",
                            "pd_agriculture_nrm")]
#Do PCA on the final set to make sure variability is explained in the entire dataset
pca_contH3 = PCA_graph_function(df = stats_contH3, df_name = 'ContPaysageH3', axe = c(3,4))

#############################################################################
#Create Indice de connectivite du paysage with metrics from previous step
############################################################################
#TODO create a generic function for this 
UREC_indice = ContH_vec[,c("continuity_vegetation_nrm",
                           "area_vegetation_nrm" ,
                           "continuity_forest_nrm",
                           "continuity_agriculture_nrm",
                           "continuity_mh_nrm",
                           "pd_forest_nrm" ,
                           "area_agriculture_nrm",
                           "pd_agriculture_nrm")]
n=1

for(n in 1:nrow(UREC_indice)){
  UREC_indice[n,]$FEContP = round(sum(
    UREC_indice[n,]$continuity_vegetation_nrm + UREC_indice[n,]$area_vegetation_nrm +
      UREC_indice[n,]$continuity_forest_nrm + UREC_indice[n,]$continuity_agriculture_nrm + UREC_indice[n,]$continuity_mh_nrm +
      UREC_indice[n,]$pd_forest_nrm + UREC_indice[n,]$area_agriculture_nrm + UREC_indice[n,]$pd_agriculture_nrm
  )/8, 2)
}
#Write out results
writeVector(UREC_indice, 'C:/Meghana/Belgique/decembre/results/Indice_ContPaysage.shp', overwrite = T)
writeVector(UREC_indice, 'C:/Meghana/Belgique/decembre/results/Indice_ContPaysage.sqlite',filetype = 'SQLite',   overwrite = T)

#Make plot of FE indice
windows(10,5)
plot(UREC_indice$FEContP, xlab = 'id', ylab = 'Indice de Continuite du Paysage')
titre = "Indice de continuite du paysage"



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
  

                     





