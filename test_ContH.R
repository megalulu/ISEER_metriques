#####################         TO BE DELETED            #####################
#####################         TEST CONT Hab          #####################

#Load libraries
install.packages("landscapemetrics")
library(landscapemetrics)

#Create a new set of UREC_merge
UREC_merge_contH = UREC_merge %>% select(id, Id_UEA,Id_rive, contF, contG, contA, contU )

#Load Utilisation du territoire masks
#list_ESA files:
path_ESA_Forest = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Forest_UREC' #Forest
list_rast_ESA_forest = list.files(path = path_ESA_Forest, pattern = '*.tif', full.names = T) #Forest list of files

path_ESA_Grass = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Grass_UREC' #Grass
list_rast_ESA_Grass = list.files(path = path_ESA_Grass, pattern = '*.tif', full.names = T) #Grass of files

path_ESA_Agriculture = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Agriculture_UREC'
list_rast_ESA_Agriculture = list.files(path = path_ESA_Agriculture, pattern = '*.tif', full.names = T) #Agriculture list of files

path_ESA_Urban = 'C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/Urban_UREC'
list_rast_ESA_Urban = list.files(path= path_ESA_Urban, full.names = T)

i = 1
r = 1
#FRAGSTAT STATS
for (i in 1:nrow(UREC_merge_contH)){
  UREC = vect(UREC_merge_contH[i,])
  id = UREC$id
  
  for (r in 1:length(list_rast_ESA_Grass)){ #Needs to be changed
    tile = rast(list_rast_ESA_Grass[r])  #Needs to be changed
    tile_name = list_rast_ESA_Grass[r]   #Needs to be changed
    tile_name = sub('.*Grass_UREC/', '', tile_name) #Needs to be changed
    tile_name = sub('.tif.*', '', tile_name)
    
    if (id == tile_name){
      #Check that input data is properly formated (unit m)
      check = check_landscape(tile, verbose = T)
      #If the input data is not properly formatted, put NA for values in UREC table at primary key 
      #F is for Forest!! NEEDS TO BE CHANGED FOR DIFFERENT TILE TYPES !!!!
      if (check$OK == "✖"){
        
       
        area_mnG = 0   #Needs to be changed
        pdG = 0        #Needs to be changed
   
      }else {
        #Calculate landscape metrics

        area_mnG =  lsm_l_area_mn(tile) 
        area_mnG = area_mnG$value     #Mean area of patches
        pdG = lsm_l_pd(tile)
        pdG = pdG$value               #Patch Density
 
        #Add values to UREC table based on primary key
       
       # UREC$area_mnU = area_mn$value       #Needs to be changed
        #UREC$pdU= pd$value                 #Needs to be changed     
      }
      UREC_merge_contH[i, 'area_mnG'] = area_mnG
      UREC_merge_contH[i, 'pdG'] = pdG

    }
  }
}

UREC_merge_contH = st_cast(UREC_merge_contH, to = 'MULTIPOLYGON')
st_write(UREC_merge_contH, dsn = 'C:/Meghana/Belgique/traitements/results/Test_ContH/', layer = 'UREC_merge_contH.shp', driver = 'ESRI Shapefile', delete_dsn = T )
###############################################################################

#Normalizing script for UREC_merge
#ELse : Script to Initiate normalizing of metrics with min max 
UREC_norm_contH = st_read('C:/Meghana/Belgique/traitements/results/Test_ContH/UREC_merge_contH.shp') #create normalization table
UREC_norm_contH = st_cast(UREC_norm_contH, to = 'MULTIPOLYGON')
UREC_norm_contH$ext_geometry = st_geometry(UREC_norm_contH) #Extract geometry for  into a new column
UREC_norm_contH = st_drop_geometry(UREC_norm_contH) #Drop geometry (drops geometry column )
#UREC_norm = naniar::replace_with_na(data = UREC_norm_contH, replace = c(NA))
#Do normalization with min max values by iterating through sf dataframe columns 
f=11
for (f in colnames(UREC_norm_contH)) {
  feature_table = select(UREC_norm_contH, all_of(f)) #select one column
  colname = names(feature_table)
  print(colname)
  #Check if column name is 'Id_UEA' or 'Id_rive' or 'ext_geometry' or IndOmbrage (already has values between 0 and 1) : these columns should not be normalized
  if (colname != 'Id_UEA' & colname != 'id' &
      colname != 'Id_rive' & colname != 'ext_geometry') {
    print('Colename is not Id_UEA or Id_rive or geometry.')
    new_colname =   paste0(names(feature_table), '_nrm')#Create name for normalized column
    print(new_colname) #Print new column name to make sure it looks correct
    feature_table_clean = subset(feature_table, feature_table[,1]>-999)
    max_val = max(feature_table_clean, na.rm = T) #find maximum value in column and put it in a variable
    print(max_val)
    min_val = min(feature_table_clean, na.rm = T) #find minimum value in column and put it in a variable
    print(min_val)
    #iterate though each row of the column we are looking at (f)
    for (n in 1:nrow(UREC_norm_contH)) {
      if(UREC_norm_contH[n,colname] == -999){
        UREC_norm_contH[n,new_colname] = -999
      }
      else {
        normalisation = (feature_table[n,1] - min_val) / (max_val - min_val) #Min max normalisation equation
        #Create a new column with new column name and add normalisation value for the row. (This may not be optimal)
        UREC_norm_contH[n, new_colname] <- normalisation}
    }
  }
}
#Convoluted way of adding geometry back to sf dataframe with the label 'geometry' to the df 
UREC_norm_contH = st_set_geometry(UREC_norm_contH, UREC_norm_contH$ext_geometry)
UREC_norm_contH$geometry = UREC_norm_contH$ext_geometry
UREC_norm_contH = st_drop_geometry(UREC_norm_contH)
UREC_norm_contH = st_set_geometry(UREC_norm_contH, UREC_norm_contH$geometry)
UREC_norm_contH = vect(UREC_norm_contH)
writeVector(UREC_norm_contH, 'C:/Meghana/Belgique/traitements/results/Test_ContH/UREC_norm_contH.SQLite', filetype = 'SQLite',  overwrite = TRUE ) # Need to write in SQlite to keep names 
writeVector(UREC_norm_contH,  'C:/Meghana/Belgique/traitements/results/Test_ContH/UREC_norm_contH.shp',  overwrite = TRUE ) # Need this will change the col names (abbreviate them weirdly)

################################################################################
#Correlation matrics of varibles
source('Statistics_new.R')


UREC_norm_ContH_sub1 = vect('C:/Meghana/Belgique/traitements/results/Test_ContH/FUREC_norm_contH.SQLite' )
names(UREC_norm_ContH_sub1)
UREC_norm_ContH_sub1 = UREC_norm_ContH_sub1[,c('id', 'id_uea', 'id_rive', 'contf_nrm',    
                                                       'contg_nrm',   'contu_nrm' , 'conta_nrm',  'area_mnu_nrm', 'pdu_nrm', 'area_mna_nrm', 'pda_nrm', 'area_mnf_nrm', 
                                                       'pdf_nrm', 'area_mng_nrm', 'pdg_nrm')]


Correlation_matrix(UREC_norm_ContH_sub1, 'ContH metrics')



######DO PCA 
UREC_norm_ContH_sub2 = UREC_norm_ContH_sub1[,c( 'contf_nrm',    
                                               'contg_nrm',   'contu_nrm' ,  'area_mnu_nrm', 'pdu_nrm', 'area_mna_nrm', 'pda_nrm', 'area_mnf_nrm', 
                                                'area_mng_nrm', 'pdg_nrm')]

names(UREC_norm_ContH_sub2)

PCA_UREC_norm_ContH_sub2 = PCA_graph_function(UREC_norm_ContH_sub2, 'ContH metrics', axe = c(1, 2))



UREC_norm_ContH_sub3 = UREC_norm_ContH_sub1[,c( 'contg_nrm',   'contu_nrm' , 'pdu_nrm', 'area_mna_nrm')]

PCA_UREC_norm_ContH_sub3 =PCA_graph_function(UREC_norm_ContH_sub3, 'ContH metrics', axe = c(3, 4))


#############################################################################################
## Calculate indice de fonction ecologique 
UREC_norm_contH = st_read('C:/Meghana/Belgique/traitements/results/Test_ContH/UREC_norm_contH.SQLite')


UREC_norm_contH$indiceContH = (UREC_norm_contH$contg_nrm + UREC_norm_contH$contu_nrm  + UREC_norm_contH$pdu_nrm + UREC_norm_contH$area_mna_nrm
)* (1/4)
plot(UREC_norm_contH$GEOMETRY)
st_crs(UREC_norm_contH)
test = vect(UREC_norm_contH)
writeVector(test, filename = 'C:/Meghana/Belgique/traitements/results/Test_ContH/UREC_norm_indice_contH.shp', overwrite = T )
st_write(UREC_norm_contH, dsn =  'C:/Meghana/Belgique/traitements/results/Test_ContH', layer= 'UREC_norm_indice_contH.shp', driver = 'ESRI Shapefile', delete_dsn =  TRUE ) # Need this will change the col names (abbreviate them weirdly)


#Load PASL IQBR with indice IQBR calculated with PASL data
df_PASL_IQBR
#df_PASL_IQBR = read.csv('C:/Meghana/Belgique/traitements/data/IQBR/PASL_IQBR_tbl.csv', header = T) #This already exists or can be found as a csv here : 'C:/Meghana/Belgique/traitements/data/IQBR/PASL_IQBR_tbl.csv'
#Normalise df_PASL_IQBR 
r=1
max_PASL = max(df_PASL_IQBR$indice_PASL_IQBR, na.rm = T)
min_PASL = min(df_PASL_IQBR$indice_PASL_IQBR, na.rm = T)
for (r in 1:nrow(df_PASL_IQBR)){
  row = df_PASL_IQBR[r,]
  indice = (row$indice_PASL_IQBR - min_PASL)/(max_PASL -min_PASL)

  df_PASL_IQBR[r,'indice_PASL_IQBR_nrm'] = indice
}



df_contH = as.data.frame(UREC_norm_contH)

df_contH_PASL = left_join(df_contH,df_PASL_IQBR, by = c('id'='id'))
contH_PASL = df_contH_PASL[, c(  "indiceContH"   , "indice_PASL_IQBR",  "indice_PASL_IQBR_nrm")]
contH_PASL = na.omit(contH_PASL)
cor_contH_PASL =  round(cor(contH_PASL, method = c('spearman')), 2)
corrplot.mixed(
  cor_contH_PASL,
  lower = 'number',
  upper = 'square',
  tl.col = 'black',
  tl.pos = 'lt',
  tl.cex = 1,
  number.cex = 0.5,
  diag = 'n',
  title = paste0('Correlation plot for metrics in ', var2),
  mar=c(0,0,1,0)
)

#Plot the two indices to look at the general relationship
windows(10,5)
plot(contH_PASL$indice_PASL_IQBR_nrm, contH_PASL$indiceContH, xlab = 'IQBR', ylab = 'Cont Hab')
titre = "Indice de continuite de l'habitat vs IQBR normaliser"
windows(10,5)
plot(contH_PASL$indice_PASL_IQBR, contH_PASL$indiceContH, xlab = 'IQBR', ylab = 'Cont Hab')


shp_PASL = left_join(df_PASL_IQBR, UREC_norm_contH, by = c('id'='id') )


#################################################################################################################################
source('Statistics_new.R')
indice_contH = st_read('C:/Meghana/Belgique/traitements/results/Test_ContH/UREC_norm_indice_contH.shp')
join_contH_IQBR19 =  st_join(IQBR_19, indice_contH, by = c('id'='id'))
join_contH_IQBR19 = join_contH_IQBR19[,c('id', 'id_uea', 'id_rive','indiceCont', 'IQBR')]
join_contH_IQBR19 = vect(join_contH_IQBR19)
join_contH_IQBR19 = na.omit(join_contH_IQBR19)    
join_contH_IQBR19 = st_cast(join_contH_IQBR19, to = 'MULTIPOLYGON')
st_write(join_contH_IQBR19, dsn = 'C:/Meghana/Belgique/traitements/results/Test_ContH/', layer = 'UREC_norm_indContH_IQBR19.shp', driver = 'ESRI Shapefile', delete_layer = 
            T)
writeVector(join_contH_IQBR19, 'C:/Meghana/Belgique/traitements/results/Test_ContH/UREC_norm_indContH_IQBR19.shp', overwrite = T )
#Do correlation plot between IQBR UT19  and IndiceContH

Correlation_matrix(join_contH_IQBR19,'Indice de ContHab vs IQBR UT 2019')
join_contH_IQBR19 = as.data.frame(join_contH_IQBR19)
join_contH_IQBR19 = join_contH_IQBR19 %>% select(-c('id_uea', 'id_rive', 'id')) #subset dataframe to only select numeric data
res <-
  round(cor(join_contH_IQBR19, method = c('spearman')), 2) #Create Correlation matrix
res
