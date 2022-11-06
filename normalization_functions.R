#Script to Normalized data  with  min and max 




#Do normalisation with min and max of data
Normalization_function <- (UREC_merge){
  UREC_norm = UREC_merge
  #Transform sf object into dataframe
  UREC_norm = st_cast(UREC_norm, to = 'MULTIPOLYGON')
  UREC_norm$ext_geometry = st_geometry(UREC_norm) #Extract geometry  into a new column
  UREC_norm = st_drop_geometry(UREC_norm) #Drop geometry drops geometry column 
  
  #Do for loop to do normalisation
  for (f in colnames(UREC_norm)) {
    feature_table = select(UREC_norm, all_of(f)) #select one column
    colname = names(feature_table)
    print(colname)
    #Check if column name is 'Id_UEA' or 'Id_rive', 'ext_geometry', 'id, or 'IndOmbrage' (already has values between 0 and 1) : these columns should not be normalized
    if (colname != 'id_uea' &
        colname != 'id_rive' & colname != 'ext_geometry'& colname != 'id') {
      print('Colename is not Id_UEA or Id_rive or geometry.')
      new_colname =   paste0(names(feature_table), '_nrm')#Create name for normalized column
      print(new_colname) #Print new column name to make sure it looks correct
      feature_table_clean = subset(feature_table, feature_table[,1]=='NA')
     # feature_table_clean = subset(feature_table, select(-c(NA)))
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
  
}




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
  if (colname != 'id_uea' &
      colname != 'id_rive' & colname != 'ext_geometry'& colname != 'id') {
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