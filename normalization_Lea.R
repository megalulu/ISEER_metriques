#This script is for normalizing the metrics
install.packages('naniar')
install.packages('tidyr')
library(sf)
library(naniar)
library(tidyr)
#If UREC_merge and UERC_norm already exists then open from the correct location 
UREC_merge = vect('C:/Users/Hp/Documents/STAGE_SHERBROOKE/UREC_rives_new/UREC_rives_new_merge.shp')
#If UREC_norm exists : 
UREC_norm = st_read('C:/Users/Hp/Documents/STAGE_SHERBROOKE/results_normalization/UREC_merge_norm.shp')

#import list of files of sampling_rives
path_UREC_rives = 'C:/Users/Hp/Documents/STAGE_SHERBROOKE/VFinale_HEmPiedDeBerge'
list_files_UREC_rives = list.files(
  path_UREC_rives,
  pattern = '*.shp',
  full.names = T
)

################################################################################
#This code is to check that each UREC_file has the same number of columns 
i =2
for (i in 1:length(list_files_UREC_rives)){ #TO be changed
  test = st_read(list_files_UREC_rives[i])  #TO be changed
  if (i == 1){
    check = test
    cnt_col_check = ncol(check)}
  else{
    cnt_col = ncol(test)
    if (cnt_col != cnt_col_check){
      print(paste0('The number of columns in ', test$Id_UEA, ' rive ', test$Id_rive, ' is: ', cnt_col ))
      print(paste0('The number of columns in check is : ', cnt_col_check))
      print('The number of columns does not match previous feature')
    } else {
      print('The number of columns match.')
      print(paste0('iteration ', i))
    }
  }
}
################################################################################
#This script is to merge all UREC_files into one. This may not be necessary if the UREC_merge file already exists. 
#merge all the UREC_rive shapefiles with pre-calculated metrics. ATTENTION : may need to change save file name
i = 2
#Initilize variable UREC_merge outside of for loop
UREC_merge = st_read(list_files_UREC_rives[1])
UREC_merge = st_transform(UREC_merge,'epsg:32198')
i = 2
for (i in 2:length(list_files_UREC_rives))
  #Start for loop at posiion 2 because of initialization process
{
  shp = st_read(list_files_UREC_rives[i])
  shp = st_transform(shp,'epsg:32198')
  print(paste0('reading UEA file ', shp$Id_UEA, ' at rive ', shp$Id_rive))
  print(paste0(
    'binding UREC_merge with UEA ',
    shp$Id_UEA,
    ' at rive ',
    shp$Id_rive
  ))
  names = names(UREC_merge)
  print(names)
  names_shp = names(shp)
  name = 'C:/Users/Hp/Documents/STAGE_SHERBROOKE/results_normalization/UREC_merge_new.shp' # output path
  couche = 'UREC_merge_new' #output layer name
  UREC_merge = rbind(UREC_merge, shp)
  print(paste0('UREC_merge has been updtated ', i, ' times'))
  st_write(UREC_merge, dsn = name, layer = couche , filetype="ESRI Shapefile", delete_layer = T)
  st_write(UREC_merge, 'C:/Users/Hp/Documents/STAGE_SHERBROOKE/results_normalization/UREC_merge_new.SQLite', delete_layer = T) #write the file in sqlite format because it doesn't abbreviate column names
}
################################################################################


#Normalizing script for UREC_merge
#ELse : Script to Initiate normalizing of metrics with min max 
UREC_norm = UREC_merge #create normalization table
UREC_norm = st_cast(UREC_norm, to = 'MULTIPOLYGON')
UREC_norm$ext_geometry = st_geometry(UREC_norm) #Extract geometry for  into a new column
UREC_norm = st_drop_geometry(UREC_norm) #Drop geometry (drops geometry column )
#UREC_norm = naniar::replace_with_na(data = UREC_norm, replace = c(NA))
#Do normalization with min max values by iterating through sf dataframe columns 
f=11
for (f in colnames(UREC_norm)) {
  feature_table = select(UREC_norm, all_of(f)) #select one column
  colname = names(feature_table)
  print(colname)
  #Check if column name is 'Id_UEA' or 'Id_rive' or 'ext_geometry' or IndOmbrage (already has values between 0 and 1) : these columns should not be normalized
  if (colname != 'Id_UEA' &
      colname != 'Id_rive' & colname != 'ext_geometry' & colname != 'layer' & colname != 'path' & colname != 'FID') {
    print('Colename is not Id_UEA or Id_rive or geometry.')
    new_colname =   paste0(names(feature_table), '_nrm')#Create name for normalized column
    print(new_colname) #Print new column name to make sure it looks correct
    feature_table_clean = subset(feature_table, feature_table[,1]>-999)
    max_val = max(feature_table_clean, na.rm = T) #find maximum value in column and put it in a variable
    print(max_val)
    min_val = min(feature_table_clean, na.rm = T) #find minimum value in column and put it in a variable
    print(min_val)
    #iterate though each row of the column we are looking at (f)
    for (n in 1:nrow(UREC_norm)) {
      if(UREC_norm[n,colname] == -999){
        UREC_norm[n,new_colname] = -999
      }
      else {
        normalisation = (feature_table[n,1] - min_val) / (max_val - min_val) #Min max normalisation equation
        #Create a new column with new column name and add normalisation value for the row. (This may not be optimal)
        UREC_norm[n, new_colname] <- normalisation}
    }
  }
}
#Convoluted way of adding geometry back to sf dataframe with the label 'geometry' to the df 
UREC_norm = st_set_geometry(UREC_norm, UREC_norm$ext_geometry)
UREC_norm$geometry = UREC_norm$ext_geometry
UREC_norm = st_drop_geometry(UREC_norm)
UREC_norm = st_set_geometry(UREC_norm, UREC_norm$geometry)
UREC_norm = vect(UREC_norm)
writeVector(UREC_norm, 'C:/Users/Hp/Documents/STAGE_SHERBROOKE/results_normalization/UREC_merge_norm.SQLite', filetype = 'SQLite',  overwrite = TRUE ) # Need to write in SQlite to keep names 
writeVector(UREC_norm, 'C:/Users/Hp/Documents/STAGE_SHERBROOKE/results_normalization/UREC_merge_norm.shp',  overwrite = TRUE ) # Need this will change the col names (abbreviate them weirdly)
