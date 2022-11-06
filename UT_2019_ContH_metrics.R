###############################################################################
###############################################################################
############ NEW CODE for metrics using UT_2019 10m resolution 
library(landscapemetrics)

##############################################################################

#Load UREC_merge and extract only columns we are interested in 
path_UREC_merge_UT19 = 'C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019/UREC_merge_new.SQLite'
UREC_merge = st_read(path_UREC_merge_UT19)
UREC_merge = UREC_merge[, c( "id_uea" ,   "id_rive")]
UREC_merge$id = paste0(UREC_merge$id_uea, '_rive', UREC_merge$id_rive)

#Load UT_2019
path_ut19_clip = 'C:/Meghana/Belgique/traitement/data/UTILISATION_TERRITOIRE_2018_10m/test.tif'
ut19_clip_r = raster::raster('C:/Meghana/Belgique/traitements/data/UTILISATION_TERRITOIRE_2019_10m/utilisation_territoire_2019_10m_clip.tif')
IQBR_UT_2019_10m_Correspondence <- read.csv("C:/Meghana/donnee_brutes/IQBR_UT_2019_10m_Correspondence.csv", sep=";")
IQBR_UT_2019_10m_Correspondence$CODE_UT = as.numeric(IQBR_UT_2019_10m_Correspondence$CODE_UT)

####################################
#Made land use sub classes in ArcGIS using 1.extract by mask, 2. extract by attribute tool !!! Needs to be done in advance
#TODO : Figure out how to add RAT raster attribute table to raster layer in R, so that you can categorize and select by attribute

#Load UT_2019 Forestier 
ut19F = raster::raster('C:/Meghana/Belgique/traitements/data/UTILISATION_TERRITOIRE_2019_10m/ut19_Forest2.tif')
UREC_merge = st_transform(UREC_merge, st_crs(ut19F))

#Load UT_2019 Agricole 
ut19A = raster::raster('C:/Meghana/Belgique/traitements/data/UTILISATION_TERRITOIRE_2019_10m/ut19_Agriculture2.tif')
#Load UT_2019 Urban 
ut19U = raster::raster('C:/Meghana/Belgique/traitements/data/UTILISATION_TERRITOIRE_2019_10m/ut19_Urban2.tif')
##Load UT_2019 Urban 
ut19H = raster::raster('C:/Meghana/Belgique/traitements/data/UTILISATION_TERRITOIRE_2019_10m/Ut19_Humide2.tif')

#Load correspondance entre code UT2019 and IQBR classes 
IQBR_UT_2019_10m_Correspondence <- read.csv("C:/Meghana/donnee_brutes/IQBR_UT_2019_10m_Correspondence.csv", sep=";")
IQBR_UT_2019_10m_Correspondence$CODE_UT = as.numeric(IQBR_UT_2019_10m_Correspondence$CODE_UT)
ut19_groups = IQBR_UT_2019_10m_Correspondence %>% group_by(DESC_CAT)
#Transform CRS of UREC_merge to crs of ut_19_clip_r
UREC_merge = st_transform(UREC_merge, st_crs(ut19_clip_r))
UREC_merge$id = paste0(UREC_merge$id_uea, '_rive', UREC_merge$id_rive)
#For loop to extract metrics of landscape (FRAGSTAT metrics)


fragementation_metric_function <- function(var1, var2, var3) {
  #var1 = shapefile of UREC (UREC_merge) as sf data.frame
  #var2 = raster of Utilisation du territoire masked for values we are interested in. e.g.  <Forest> (need to do this step beforehand , in ArcMap sometimes)
  # var 3  = character string of name of colum referencing the specific UT selection eg. "Forest" 
   area_new_col = paste0('area_', var3)
   pd_new_col = paste0('pd_', var3)
  
  for (i in 1:nrow(var1)) {
    UREC = var1[i,]
    UREC = st_transform(UREC, st_crs(var2))
    ut_urec = terra::crop(var2, UREC)
    urec_raster = terra::rasterize(UREC, ut_urec) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
    ut_mask = terra::mask(ut_urec, urec_raster)
    
    check = check_landscape(ut_mask, verbose = T)
    if (check$OK == "✖") {
      area_mn = 0   #Needs to be changed
      pd = 0        #Needs to be changed
      
    } else {
      #Calculate landscape metrics
      
      area_mn =  lsm_l_area_mn(ut_mask)
      area_mn = area_mn$value     #Mean area of patches
      pd = lsm_l_pd(ut_mask)
      pd = pd$value               #Patch Density
      
      #Add values to UREC table based on primary key
      
      # UREC$area_mnU = area_mn$value       #Needs to be changed
      #UREC$pdU= pd$value                 #Needs to be changed
    }
    
    var1[i, area_new_col] = area_mn
    var1[i, pd_new_col] = pd
  }
  return(var1)
}


#RUN FRAGMENTATION METRIC

UREC_merge = fragementation_metric_function(var1= UREC_merge, var2 = ut19F, var3 = 'Forest')
UREC_merge = fragementation_metric_function(var1= UREC_merge, var2 = ut19A, var3 = 'Agricole')
UREC_merge = fragementation_metric_function(var1= UREC_merge, var2 = ut19U, var3 = 'Urban')
UREC_merge = fragementation_metric_function(var1= UREC_merge, var2 = ut19H, var3 = 'Humide')


##############################################################################
#Create function for metric de continuite 
var1 = UREC_merge
var1 = var1[1:3,]
var2 = ut19A
#var2 = terra::rast(var2)
var3 = 'Agriculture'
test = UREC_merge
#test = st_transform(test, ut19A)
r =3
i = 3


continuity_metric <- function(var1, var2, var3) {
  #var1 = shapefile of UREC (UREC_merge) as sf data.frame
  #var2 = raster of Utilisation du territoire masked for values we are interested in. e.g.  <Forest> (need to do this step beforehand , in ArcMap sometimes)
  # var 3  = character string of name of colum referencing the specific UT selection eg. "Forest" 
  new_col_name = paste0('continuity_', var3)
  
  for (r in 1:nrow(var1)) {
    shp = var1[r,]
    for (i in 1:length(list_files_sampling_rive)) {
      name = list_files_sampling_rive[i]
      name = sub('.*sampling_rives/', '', name) #Needs to be changed
      name = sub('.shp.*', '', name)
      if (name == shp$id){
        
        UREC_vect = st_read(list_files_sampling_rive[i])
        UREC_vect = st_transform(UREC_vect, crs(var2))
 
        ut19_clip = terra::crop(var2, UREC_vect) #Reduce the extent of the ESA raster to the extent of UREC
        UREC_raster = terra::rasterize(UREC_vect, ut19_clip) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
        ut_mask = terra::mask(ut19_clip, UREC_raster)  #create mask on raster with UREC shapefile
        
        
        
        #Extract the surface of forested/ grassland/urban/agriculture pixels in each UREC to get other metric
        UREC_vect$surface_full = exactextractr::exact_extract(ut_mask, UREC_vect, 'count') *
          10 #multiply sum by area of each pixel (meed to do this with sf object!)
        
        UREC_vect$area = as.numeric(st_area(UREC_vect)) #calculate the area of each sampling unit in the UREC
        UREC_vect$prop = UREC_vect$surface_full / UREC_vect$area # get the ratio of forested area in each sampling unit by the area of the sampling unit
        
        continuity = median(UREC_vect$prop) #create a variable of the median for continuity of forested areas
        
    }
      var1[r, new_col_name] = continuity
  }
  }
  return (var1)
  }




UREC_merge = continuity_metric(var1 = UREC_merge, var2 = ut19U, var3 = 'Urban')
UREC_merge = continuity_metric(var1 = UREC_merge, var2 = ut19F, var3 = 'Forest')
UREC_merge = continuity_metric(var1 = UREC_merge, var2 = ut19H, var3 = 'Humide')
UREC_merge = continuity_metric(var1 = UREC_merge, var2 = ut19A, var3 = 'Agricole')

st_write(UREC_merge, dsn = 'C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019', layer = 'UREC_merge_new_ut19_2.shp', driver = 'ESRI Shapefile', overwrite = T)
st_write(UREC_merge, dsn = 'C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019/UREC_merge_new_ut19_2.SQLite', driver = 'SQLite', delete_dsn  = T)




##########################################################################################################################
#Do stats on metrics 
source('Statistics_new.R')
UREC_norm = vect('C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019/UREC_merge_norm_2.SQLite')
UREC_merge_sub = UREC_norm[, c('id_uea', 'id_rive','id', 'continuity_urban_nrm','continuity_forest_nrm', 
                                'continuity_humide_nrm' ,'continuity_agricole_nrm', 'area_forest_nrm', 'pd_forest_nrm',  
                                'area_agricole_nrm' ,'pd_agricole_nrm', 'area_urban_nrm','pd_urban_nrm', 'area_humide_nrm',
                               'pd_humide_nrm' )]

#Correlation between metrics
df_corUT2019 = Correlation_matrix(UREC_merge_sub, 'UT2019 metrics for ContH')

#After figuring out which metrics are correlated more than |0.6| check correlation between variables to see which are most correlated
UREC_merge_sub2 = UREC_merge_sub[,c('id_uea', 'id_rive','id','continuity_urban_nrm',
                                 'continuity_forest_nrm', 'continuity_agricole_nrm',
                                 'area_forest_nrm', 'area_urban_nrm', 
                                 'area_humide_nrm', 'pd_humide_nrm')]

df_corUT2019_sub2 = Correlation_matrix(UREC_merge_sub2, 'UT2019 sub2 metrics for ContH')#Use this information to select one metric per correlated pairs
#DO ACP on selected metrics from sub2 
acp_corUT2019_sub = PCA_graph_function(df_corUT2019_sub2, 'ACP des metriques corr selection', c(1,2))

UREC_merge_sub3 = UREC_norm[, c('id_uea', 'id_rive','id',
  'continuity_urban_nrm',
  'continuity_forest_nrm',
  'continuity_agricole_nrm',
  'area_forest_nrm',
  'area_urban_nrm',
  'area_humide_nrm',
  'pd_humide_nrm'
)]

df_corUT2019_sub3 =  Correlation_matrix(UREC_merge_sub3, 'UT2019 sub3 metrics for ContH')#
acp_corUT2019_sub3 = PCA_graph_function(df_corUT2019_sub3, 'ACP des metriques corr selection', c(1,2))


#Do ACP on final selection based on previous data analysis 
df_UT2019_ContH = UREC_norm[, c(
  'continuity_humide_nrm',
  'pd_forest_nrm',
  'area_agricole_nrm' ,
  'pd_agricole_nrm',
  'pd_urban_nrm',
  'continuity_forest_nrm',
  'continuity_agricole_nrm',
  'area_urban_nrm'
)]
acp_UT2019_ContH = PCA_graph_function(df_UT2019_ContH, 'ACP des metrique finales', c(1,2))

#Create new column with indice 

UT2019_ContH = UREC_norm


UT2019_ContH$indContH = (
  UT2019_ContH$continuity_humide_nrm + UT2019_ContH$pd_forest_nrm  +
    UT2019_ContH$area_agricole_nrm
  + UT2019_ContH$pd_agricole_nrm + UT2019_ContH$pd_urban_nrm + UT2019_ContH$continuity_forest_nrm
  + UT2019_ContH$continuity_agricole_nrm + UT2019_ContH$area_urban_nrm
) * (1 / 8)

terra::writeVector(UT2019_ContH, filename =  'C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019/IndiceContH_2.shp', 
                   filetype = 'ESRI Shapefile', overwrite = T)
terra::writeVector(UT2019_ContH, filename =  'C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019/IndiceContH_2.SQLite', 
                   filetype = 'SQLite', overwrite = T)

#open UREC merge shapefile with IQBR calculation done with UT2019
IQBR_19 = st_read('C:/Meghana/Belgique/traitements/results/UREC_IQBR/UREC_merge_IQBR_19.shp')
IQBR_19$id = paste0(IQBR_19$Id_UEA, '_rive', IQBR_19$Id_rive)
UT2019_ContH_sf = st_read('C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019/IndiceContH_2.SQLite')
UT2019_ContH_sf$geom = st_geometry(UT2019_ContH_sf)
#Transform data from shapefile objects to datafram
df_IQBR_19 = as.data.frame(IQBR_19)
df_UT2019_indContH = as.data.frame(UT2019_ContH)
#Join dataframe with indice Continuity of habitat and IQBR calculated with UT2019 (10m)
join_iqbr19_UT19_ContH = left_join(df_IQBR_19, df_UT2019_indContH, by= c('id'='id'))
join_iqbr19_UT19_ContH = join_iqbr19_UT19_ContH %>% select ('id', 'id_uea', 'id_rive', 'indContH', 'IQBR','nrm', 'geometry')
#Transdorm df into sf object
sf_join_iqbr19_UT19_ContH = st_set_geometry(join_iqbr19_UT19_ContH, join_iqbr19_UT19_ContH$geometry)

vect_join_iqbr19_UT19_ContH = vect(sf_join_iqbr19_UT19_ContH )
terra::writeVector(
  vect_join_iqbr19_UT19_ContH,
  filename =  'C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019/UT19_IQBR_IndiceContH_2.SQLite',
  filetype = 'SQLite',
  overwrite = T
)
terra::writeVector(
  vect_join_iqbr19_UT19_ContH,
  filename =  'C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019/UT19_IQBR_IndiceContH_2.shp',
  filetype = 'ESRI Shapefile',
  overwrite = T
)
##Do correlation between IQBR and indContH
corr_ut2019_IndContH = Correlation_matrix(vect_join_iqbr19_UT19_ContH, 'Correlation UT2019_IQBR and Indice de ContH' )
##############################################################################################################################
#This is to normalise IQBR values to indice de continuity de l<habitat
max_IQBR19= max(IQBR_19$IQBR)
max_indContH= max(UT2019_ContH$indContH)
min_IQBR19 = min(IQBR_19$IQBR)
min_indContH = min(UT2019_ContH$indContH)
UT2019_ContH$nrm_indContH = (UT2019_ContH$indContH - min_IQBR19)/ (max_IQBR19 - min_IQBR19)

IQBR_19$nrm = ((max_indContH-min_indContH)*(IQBR_19$IQBR-min_IQBR19)/(max_IQBR19 - min_IQBR19))+min_indContH

st_write(
  IQBR_19,
  dsn = 'C:/Meghana/Belgique/traitements/results/Test_ContH/test_UT_2019',
  layer = 'UREC_merge_new_ut19_nrm1.shp',
  driver = 'ESRI Shapefile',
  overwrite = T
)
#######################################################################################################################
  
               