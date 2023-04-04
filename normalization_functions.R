#Script to Normalized data  with  min and max 



# UREC_norm = UREC_merge_norm
# col_names = c('pd_vegetation_optimale_nrm')
# col = col_names[1]
# n=1
#Do normalisation with min and max of data
UREC_merge= df

Normalization_function <- function(UREC_merge) {
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
        colname != 'id_rive' &
        colname != 'ext_geometry' &
        colname != 'id' &
        colname != 'rive'&
        colname != 'Id_UEA'&
        colname != "rive.x"&
        colname != 'ogc_fid'&
        colname != 'rive.y') {
      print('Colename is not Id_UEA or Id_rive or geometry.')
      new_colname =   paste0(names(feature_table), '_nrm')#Create name for normalized column
      print(new_colname) #Print new column name to make sure it looks correct
      #feature_table_clean = subset(feature_table, feature_table[,1]!='NA')
      #feature_table = feature_table[]
      # feature_table_clean = subset(feature_table, select(-c(NA)))
      max_val = max(feature_table, na.rm = T) #find maximum value in column and put it in a variable
      print(max_val)
      min_val = min(feature_table, na.rm = T) #find minimum value in column and put it in a variable
      print(min_val)
      #If min and max of column are equal then everything gets a value of 1
      if (max_val == min_val) {
        UREC_norm[, new_colname] <- 1
      }
      else{
        #iterate though each row of the column we are looking at (f)
        for (n in 1:nrow(UREC_norm)) {
          if (!is.na(UREC_norm[n, colname])) {
            #if the value at a row is NA, then we give it a value of 0 (because only pd have a values of NA)
            normalisation = (feature_table[n, 1] - min_val) / (max_val - min_val)
            UREC_norm[n, new_colname] <- normalisation
            
          } else {
            UREC_norm[n, new_colname] = NA
            
          }
          
          
        }
      }
    }
  }
  #Convoluted way of adding geometry back to sf dataframe with the label 'geometry' to the df
  UREC_norm = st_set_geometry(UREC_norm, UREC_norm$ext_geometry)
  UREC_norm$geometry = UREC_norm$ext_geometry
  UREC_norm = st_drop_geometry(UREC_norm)
  UREC_norm = st_set_geometry(UREC_norm, UREC_norm$geometry)
  #UREC_norm = vect(UREC_norm)
  
}



#Function to inverse patch density rows
New_inverse_fun <- function(UREC_norm , col_names) {
  #UREC_norm : sf dataframe that has the normalized columns to be inversed (thoses linked with patch density : pd_....)
  #col_names : vector of characters with names of columns that need to be inversed. The columns have to present in UREC_norm
  
  #converting sf object into dataframe with a seperate geometry
  UREC_norm$ext_geometry = st_geometry(UREC_norm) #Extract geometry  into a new column
  UREC_norm = st_drop_geometry(UREC_norm) #Drop geometry drops geometry column
  #iterate through the column names
  for (col in col_names) {
    table = select(UREC_norm, all_of(col)) #extract only one column from UREC_norm
    for (n in 1:nrow(table)) {
      #Iterate through the rows
      if (is.na(table[n,col])) {
        #if the value at row n and column "col" is NA
        UREC_norm[n, col] = 0 #then give it a value of 0
      } else{
        UREC_norm[n, col] = -1*(UREC_norm[n, col]) + 1 #otherwise inverse the values
      }
    }
  }
  #Put geometry back into dataframe and covnert to sf object
  UREC_norm = st_set_geometry(UREC_norm, UREC_norm$ext_geometry)
  UREC_norm$geometry = UREC_norm$ext_geometry
  UREC_norm = st_drop_geometry(UREC_norm)
  UREC_norm = st_set_geometry(UREC_norm, UREC_norm$geometry)
  
  return(UREC_norm)
  
}




