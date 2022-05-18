#This script is for normalizing the metrics

library(sf)
#If UREC_merge and UERC_norm already exists then open from the correct location 
UREC_merge = st_read('C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_new.shp')
#If UREC_norm exists : 
UREC_norm = st_read('C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_norm.shp')

################################################################################
#This code is to check that each UREC_file has the same number of columns 
i =2
for (i in 1:length(list_files_UREC_rives)){
  test = st_read(list_files_UREC_rives[i])
  if (i ==1){
    check = test
    cnt_col_check = ncol(check)}
  else{
    cnt_col = ncol(test)
    if (cnt_col != cnt_col_check){
      print(paste0('The number of columns in ', test$Id_UEA, ' rive ', test$Id_rive, ' is: ', cnt_col ))
      print(paste0('The number of columns in check is : ', cnt_col_check))
    }
  }
}
################################################################################
#This script is to merge all UREC_files into one. This may not be necessary if the UREC_merge file already exists. 
#merge all the UREC_rive shapefiles with pre-calculated metrics. ATTENTION : may need to change save file name
i = 2
for (i in 1:length(list_files_UREC_rives)) 
{
  shp = st_read(list_files_UREC_rives[i])
  print(paste0('reading UEA file ', shp$Id_UEA, ' at rive ', shp$Id_rive))
  if (i == 1) 
  {
    UREC_merge = shp
  } 
  else 
  {
    print(paste0('binding UREC_merge with UEA ', shp$Id_UEA, ' at rive ', shp$Id_rive))
    names = names(UREC_merge)
    print(names)
    names_shp = names(shp)
    UREC_merge = rbind(UREC_merge, shp) 
    st_write(UREC_merge, 'C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_new.shp', delete_layer = T)
    st_write(UREC_merge, 'C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_new.SQLite', delete_layer = T) #write the file in sqlite format because it doesn't abbreviate column names
    
  }
}
################################################################################


#Normalizing script for UREC_merge
#ELse : Script to Initiate normalizing of metrics with min max 
UREC_norm = UREC_merge #create normaliation table
UREC_norm = st_cast(UREC_norm, to = 'MULTIPOLYGON')
UREC_norm$ext_geometry = st_geometry(UREC_norm) #Extract geometry for  into a new column
UREC_norm = st_drop_geometry(UREC_norm) #Drop geometry (drops geometry column )
#Do normalization with min max values by iterating through sf dataframe columns 
f=3
for (f in colnames(UREC_norm)) {
  feature_table = select(UREC_norm, all_of(f)) #select one column
  colname = names(feature_table)
  print(colname)
  #Check if column name is 'Id_UEA' or 'Id_rive' or 'ext_geometry' or IndOmbrage (already has values between 0 and 1) : these columns should not be normalized
  if (colname != 'Id_UEA' &
      colname != 'Id_rive' & colname != 'ext_geometry') {
    print('Colename is not Id_UEA or Id_rive or geometry.')
    new_colname =   paste0(names(feature_table), '_nrm')#Create name for normalized column
    print(new_colname) #Print new column name to make sure it looks correct
    max_val = max(feature_table) #find maximum value in column and put it in a variable
    print(max_val)
    min_val = min(feature_table) #find minimum value in column and put it in a variable
    print(min_val)
    #iterate though each row of the column we are looking at (f)
    for (n in 1:nrow(UREC_norm)) {
      normalisation = (feature_table - min_val) / (max_val - min_val) #Min max normalisation equation
      #Create a new column with new column name and add normalisation value for the row. (This may not be optimal)
      UREC_norm[, new_colname] <- normalisation 
    }
  }
}
#Convoluted way of adding geometry back to sf dataframe with the label 'geometry' to the df 
UREC_norm = st_set_geometry(UREC_norm, UREC_norm$ext_geometry)
UREC_norm$geometry = UREC_norm$ext_geometry
UREC_norm = st_drop_geometry(UREC_norm)
UREC_norm = st_set_geometry(UREC_norm, UREC_norm$geometry)
UREC_norm = vect(UREC_norm)
writeVector(UREC_norm, 'C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_norm.SQLite', filetype = 'SQLite',  overwrite = TRUE ) # Need to write in SQlite to keep names 
writeVector(UREC_norm, 'C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_norm.shp',  overwrite = TRUE ) # Need this will change the col names (abbreviate them weirdly)
