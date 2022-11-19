#Scripts of step to follow

#Source file with functions : metric_functions.R
source('metric_functions.R')
source('Normalization_functions.R')
source('Statistics_new.R')
#Set variables 
#Set variables for paths and files linked to UREC
#path_UREC_rive = 'C:/Meghana/Belgique/traitements/UREC_MaxCam/UREC_MaxCam_rive/' #path towards folder with individual UREC shapefiles
path_UREC_sampling = 'C:/Meghana/Belgique/decembre/data/sampling/' #path towards folder with individual UREC sampling shapefiles (made previously)
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp') #read UREC file with all URECs merged. This is where we will put the results of our metrics

#Set variables files and paths linked to raster files (land use/ Utilisation du territoire)
ut19_Veg = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_Veg.tif') #this raster file has been masked to only show Vegetation (Foret, MH, Agriculture)

ut19_Forest = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_Foret.tif') ##this raster file has been masked to only show Vegetation (Foret)
ut19_Agriculture =  raster('C:/Meghana/Belgique/decembre/data/raster/ut19_Agriculture.tif') ##this raster file has been masked to only show Vegetation (Agriculture)
ut19_MH = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_MH.tif')##this raster file has been masked to only show Vegetation (MH)
ut19_Urban = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_Urban.tif')

ut19_VegOpt = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_VegOptimale.tif')
ut19_full= raster('C:/Meghana/Belgique/decembre/data/raster/ut19_mask.tif')
ut19_classes <- read.csv("C:/Meghana/donnee_brutes/correspondance_indiceNat_ut19_10m.csv", sep=";")
ut19_classes$CODE_UT = as.numeric(ut19_classes$CODE_UT)
ut19_groups = ut19_classes %>% group_by(DESC_CAT)



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
#New set of results
#Connectivity and fragementation metric functions for UT Vegetation
results_new = continuity_metric(UREC_full = UREC_merge, path_sampling = path_UREC_sampling,  raster_UT = ut19_VegOpt, classe_UT = 'vegetation_optiamle' )
#st_write(results, 'C:/Meghana/Belgique/decembre/results/output1.shp')
results_new = PDV_fragementation_metric_function(UREC_full= results_new, raster_file = ut19_VegOpt, col_name = 'vegetation_optimale')
results_new = Indice_Naturalite_functions(UREC_merge = results_new, raster_file = ut19_full, csv_class_correspondence = ut19_groups)
st_write(results_new,'C:/Meghana/Belgique/decembre/results/results_new1.sqlite' )
st_write(results_new,'C:/Meghana/Belgique/decembre/results/results_new1.shp' )

#Normalization_functions
################################################################################
source('Normalization_functions.R')
sub_results_new = subset(results_new, select = -c(area_vegetation_optimale))
results_new = st_read('C:/Meghana/Belgique/decembre/results/results_new1.sqlite') #load UREC_merge with metrics s if not already loaded 
sub_results_new = subset(results_new, select = -c(area_vegetation_optimale))
sub_results_new_nrm = Normalization_function(UREC_merge = sub_results_new)
sub_results_new_nrm = New_inverse_fun(UREC_norm = sub_results_new_nrm, col_names = c("pd_vegetation_optimale_nrm"))

st_write(sub_results_new_nrm, 'C:/Meghana/Belgique/decembre/results/results_new1_norm.shp', delete_layer = T)
st_write(results_norm, 'C:/Meghana/Belgique/decembre/results/results_new1_norm.SQLite', driver = 'SQLITE')

results_new_Indice = sub_results_new_nrm
results_new_Indice$ConP = (results_new_Indice$pd_vegetation_optimale_nrm +
                             results_new_Indice$continuity_vegetation_optiamle_nrm +
                             results_new_Indice$indice_nat_nrm)/3
  
plot(results_new_Indice$ConP)
st_write(results_new_Indice,'C:/Meghana/Belgique/decembre/results/results_new1_indice.SQLite', driver = 'SQLITE' , delete_layer = T)
st_write(results_new_Indice,'C:/Meghana/Belgique/decembre/results/results_new1_indice.shp', delete_layer = T)



################################################################################
#Do statistics to find least correlated metrics
################################################################################
source('Statistics_new.R')

#Load UREC_norm as vect data 

ContH_vec =  vect('C:/Meghana/Belgique/decembre/results/output3_norm.SQLite')

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
stats_contH2 = ContH_vec[, c("id_uea", "rive",'id', 'pd_vegetation_nrm', 'pd_mh_nrm')] #here only two variables need to be tested 
#Do ACP on these pairs of variables that are correlated and select one
pca_contH2 = PCA_graph_function(df = stats_contH2, df_name = 'ContPaysageH2', axe = c(1,2))

#Create final dataset with only variables to be included in index of ecological function 
stats_contH3 = ContH_vec[,c("id_uea", "rive", "id",
                            "continuity_vegetation_nrm",
                            "continuity_forest_nrm",
                            "continuity_agriculture_nrm",
                            "continuity_mh_nrm",
                            "pd_forest_nrm" ,
                            "pd_mh_nrm",
                            "pd_agriculture_nrm",
                            "pd_vegetation_nrm")]
#Do PCA on the final set to make sure variability is explained in the entire dataset
pca_contH3 = PCA_graph_function(df = stats_contH3, df_name = 'ContPaysageH3', axe = c(3,4))

#############################################################################
#Create Indice de connectivite du paysage with metrics from previous step
############################################################################
#TODO create a generic function for this 
UREC_indice = ContH_vec[,c("id_uea", "rive", "id",
                           "continuity_vegetation_nrm",
                           "continuity_forest_nrm",
                           "continuity_agriculture_nrm",
                           "continuity_mh_nrm",
                           "pd_forest_nrm" ,
                           "pd_mh_nrm",
                           "pd_agriculture_nrm",
                           "pd_vegetation_nrm")]
n=1

#For loop for equal weights


UREC_indice$FEContP_poids_egale = round((
  UREC_indice$continuity_vegetation_nrm +
    UREC_indice$continuity_forest_nrm +
    UREC_indice$continuity_agriculture_nrm + 
    UREC_indice$continuity_mh_nrm +
    UREC_indice$pd_forest_nrm +
    UREC_indice$pd_agriculture_nrm +
    UREC_indice$pd_mh_nrm +
    UREC_indice$pd_vegetation_nrm 
)/8, 2)

#for loop for assigned weights ( here all agriculture is 1/5 the weight of other landuse)
UREC_indice$FEContP_poids  = round((
    UREC_indice$continuity_vegetation_nrm *(5/28) +
      UREC_indice$continuity_forest_nrm*(5/28)  +
      UREC_indice$continuity_agriculture_nrm*(1/28) + 
      UREC_indice$continuity_mh_nrm *(5/28)+
      UREC_indice$pd_forest_nrm*(5/28) +
      UREC_indice$pd_agriculture_nrm *(1/28)+
      UREC_indice$pd_mh_nrm*(5/28) +
      UREC_indice$pd_vegetation_nrm *(5/28)
  ), 2)

max_poids = max(UREC_indice$FEContP_poids )
min_poids = min(UREC_indice$FEContP_poids )
UREC_indice$FEContP_poids_nrm = (UREC_indice$FEContP_poids-min_poids)/(max_poids-min_poids)



UREC_indice$FEContP_Forest1 = round(
    (UREC_indice$continuity_forest_nrm + 
    UREC_indice$pd_forest_nrm)/2 
  ,
  2
)
UREC_indice$FEContP_Vegetation1 = round(
  (UREC_indice$continuity_vegetation_nrm + UREC_indice$area_vegetation_nrm)/2 
  ,
  2
)
UREC_indice$FEContP_Agriculture1 = round(
  (UREC_indice$continuity_agriculture_nrm + UREC_indice$area_agriculture_nrm+UREC_indice$pd_agriculture_nrm)/3
  ,
  2
)

UREC_indice$FEContP_MH1 = round(
  (UREC_indice$continuity_mh_nrm)
  ,
  2
)


#plot all datasets together 
par(mar = c(5, 4, 2, 8),                                  # Specify par parameters
    xpd = TRUE)

par(mar = c(5.1, 4.1, 4.1, 2.1),                                  # Specify par parameters
    xpd = TRUE)
plot(UREC_indice$FEContP_poids, xlab = 'Unite spatiale', ylab = "Valeur de l'indice entre 0 et 1", pch = 3)
points(UREC_indice$FEContP_poids_egale, pch = 3, col = 'red')
legend("topleft", legend=c("Indice avec poids", " Indice poids egales"),
       col=c("black", "red"), pch =3, cex=0.8)
titre = "Indice de connectivite du paysage"

plot(UREC_indice$FEContP_poids, xlab = 'Unite spatiale', ylab = "Valeur de l'indice entre 0 et 1", pch = 3)
plot(UREC_indice$FEContP_Vegetation1,  pch = 3, col = 'red', xlab = 'Unite spatiale', ylab = "Valeur de l'indice entre 0 et 1" )
points(UREC_indice$FEContP_poids, xlab = 'Spatial unit', ylab = "Valeur de l'indice entre 0 et 1", pch = 3)
points(UREC_indice$FEContP_Vegetation1,  pch = 3, col = 'red')
legend("right", inset = c(-0.5, 0),                   # Create legend outside of plot
       legend = c("Indice den\ Connectivit paysage", "metrique Vegetation"),
       col=c("black", "red"),
       pch = 3,
       cex=0.8)
       
legend("right", legend=c("Indice de Cont paysage", "metrique Vegetation"),
       col=c("black", "red"), lty=1:2, cex=0.8)

points(UREC_indice$FEContP_Forest1, col = 'green', pch = 3)
points(UREC_indice$FEContP_Agriculture1, col = 'yellow', pch = 3)
points(UREC_indice$FEContP_MH1, col = 'blue', pch = 3)

#Write out results
writeVector(UREC_indice, 'C:/Meghana/Belgique/decembre/results/Indice_ContPaysage.shp', overwrite = T)
writeVector(UREC_indice, 'C:/Meghana/Belgique/decembre/results/Indice_ContPaysage.sqlite',filetype = 'SQLite',   overwrite = T)

#Make plot of FE indice poids egal vs poids pas egal
windows(10,5)
plot(
  UREC_indice$FEContP_poids,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)
points(UREC_indice$FEContP_poids_egale, pch = 3, col = 'red')
legend("topleft", legend=c("Indice avec poids", " Indice poids egal"),
       col=c("black", "red"), pch =3, cex=0.8)

#Make plot of FE indice poids egal vs poids pas egal and vegetation
windows(10,5)
plot(
  UREC_indice$FEContP_poids,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)
points(UREC_indice$FEContP_poids_egale, pch = 3, col = 'red')
points(UREC_indice$FEContP_Vegetation1, pch = 3, col = 'blue')
legend("topleft", legend=c("Indice avec poids", " Indice poids egal", 'Metriques de vegetation'),
       col=c("black", "red", 'blue'), pch =3, cex=0.8)

#Make plot of FE indice poids egal vs poids pas egal and Forest
windows(10,5)
plot(
  UREC_indice$FEContP_poids,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)
points(UREC_indice$FEContP_poids_egale, pch = 3, col = 'red')
points(UREC_indice$FEContP_Forest1, pch = 3, col = 'blue')
legend("topleft", legend=c("Indice avec poids", " Indice poids egal", 'Metriques de Foret'),
       col=c("black", "red", 'blue'), pch =3, cex=0.8)

#Make plot of FE indice poids egal vs poids pas egal and Agriculture
windows(10,5)
plot(
  UREC_indice$FEContP_poids,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim=c(0,1)
)
points(UREC_indice$FEContP_poids_egale, pch = 3, col = 'red')
points(UREC_indice$FEContP_Agriculture1, pch = 3, col = 'blue')
legend("topleft", legend=c("Indice avec poids", " Indice poids egal", 'Metriques de Agriculture'),
       col=c("black", "red", 'blue'), pch =3, cex=0.8)

#Make plot of FE indice poids egal vs poids pas egal and MH
windows(10,5)
plot(
  UREC_indice$FEContP_poids,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)
points(UREC_indice$FEContP_poids_egale, pch = 3, col = 'red')
points(UREC_indice$FEContP_Agriculture1, pch = 3, col = 'blue')
legend("topleft", legend=c("Indice avec poids", " Indice poids egal", 'Metriques de MH'),
       col=c("black", "red", 'blue'), pch =3, cex=0.8)

#############################################################################################
#Indice Regulation de la temperature de l'eau
#############################################################################################
#Preprocessing

#############################################################################################

PtRef03 <- st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb", #this may need to change depending on the UDH
                   layer = "PtRef",
                   stringsAsFactors = F)
PtRef05 <- st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb", #this may need to change depending on the UDH
                   layer = "PtRef",
                   stringsAsFactors = F)
pt_ref_clip = st_read('C:/Meghana/Belgique/decembre/traitements/pt_ref_int_merge.shp')

UEA_MaxCam = st_read('C:/Meghana/Belgique//decembre/data/UEA_merge.shp')

#Int_PtRef03_UREC_merge = st_intersection(UREC_merge, PtRef03)
#Int_PtRef03_UREC_merge_group = Int_PtRef03_UREC_merge %>% group_by(Id_UEA)
PtRef_mod_lotique03 = st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb", 
                              layer = "PtRef_mod_lotique",
                              stringsAsFactors = F)
PtRef_mod_lotique05 = st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb", 
                              layer = "PtRef_mod_lotique",
                              stringsAsFactors = F)

PtRef03_mod_lotique_join = left_join(pt_ref_clip, PtRef_mod_lotique03, by = c('Id_PtRef' = 'Id_PtRef'))
PtRef05_mod_lotique_join = left_join(pt_ref_clip, PtRef_mod_lotique05, by = c('Id_PtRef' = 'Id_PtRef'))
PtRef_mod_lotique_join = rbind(PtRef03_mod_lotique_join,PtRef05_mod_lotique_join)

PtRef_mod_lotique_join = subset(PtRef_mod_lotique_join, select = c(Id_PtRef, Id_UEA,
                                                                           Largeur_mod))
PtRef_mod_lotique_join = subset(PtRef_mod_lotique_join, Largeur_mod != -999) #remove rows = -999 in column of interest (Largeur_mod)
PtRef_mod_lotique_join = subset(PtRef_mod_lotique_join, !is.na(Largeur_mod))
largeur_riviere= PtRef_mod_lotique_join %>% dplyr::group_by(Id_UEA) %>% dplyr::summarize(river_width = mean(Largeur_mod)) #group table by Id_UEA and summarize the column Largeur_mod with median function
st_write(largeur_riviere, 'C:/Meghana/Belgique/decembre/traitements/pt_ref_largeur_riviere.shp', delete_layer = T) #write the table containing (Id_UEA and median river_width)
largeur_riviere = st_drop_geometry(largeur_riviere)
#Join table of median width grouped-by Id_UEA with the UEA table
UEA_larg_mod = dplyr::left_join(UEA_MaxCam,largeur_riviere, by=c('Id_UEA'='Id_UEA')) #choose correct Primary Key!!

path_name_buffer_largeur_mod = 'C:/Meghana/Belgique/traitements/results/surface_eau_mod/surface_eau_mod_MaxCam/'
i = 63
for (i in 1:nrow(UEA_larg_mod)){
  river = UEA_larg_mod[i,]
  river_Id_UEA = river$Id_UEA
  dist = river$river_width
  if (!is.na(dist)){
    buffer = st_buffer(river, dist)
    name_buffer = paste0(path_name_buffer_largeur_mod, river_Id_UEA, '.shp')
    st_write(buffer, name_buffer, delete_layer = T) #write the buffer shapefiles to a folder
  }else {
    print(paste0('No available river width for ', river$id))
  }
 
}

list_file_water_surface = list.files('C:/Meghana/Belgique/traitements/results/surface_eau_mod/surface_eau_mod_MaxCam/', pattern = '*.shp', full.names = T)
i=1
n=140
for (i in 1:length(list_file_water_surface)) {
  water = vect(list_file_water_surface[i])
  water_id = water$Id_UEA
  for (n in 1:nrow(UREC_merge)) {
    shp = vect(UREC_merge[n,])
    Id_uea = shp$Id_UEA
    if (Id_uea == water_id) {
      Id= shp$id
      name = paste0(
        'C:/Meghana/Belgique/traitements/results/surface_eau_mod/UREC_water/UREC_water_MaxCam/',
        Id,
        '.shp'
      )
      UREC_water = terra::intersect(water, shp)
      terra::writeVector(UREC_water, name, overwrite = T)
    }
    
  }
}




##########################
#Ombrage sur la riviere
###################

#Load ombrage 
path_ombrage = 'C:/Meghana/Belgique/decembre/traitements/ombrage/'
#these all have different projections for some reason !
ombrage1 = raster(paste0(path_ombrage, 'ombrage1.vrt')) 
ombrage2 = raster(paste0(path_ombrage, 'ombrage2.vrt'))
ombrage3 = raster(paste0(path_ombrage, 'ombrage3.vrt'))

path_UREC_water = 'C:/Meghana/Belgique/traitements/results/surface_eau_mod/UREC_water/UREC_water_MaxCam/'
list_files_UREC_water = list.files(path_UREC_water, pattern = '*.shp', full.names = T)
l = 1
i = 1
#merge all UREC_water surfaces :
UREC_water_merge = st_read(list_files_UREC_water[1])
for (i in 2:length(list_files_UREC_water)){
  water_rive = st_read(list_files_UREC_water[i])
  UREC_water_merge =rbind(UREC_water_merge, water_rive)
}
st_write(UREC_water_merge,'C:/Meghana/Belgique/traitements/results/surface_eau_mod/UREC_water/UREC_water_MaxCam/merge/UREC_water_merge.shp' )

test_ombrage = UREC_water_merge
test_ombrage=st_transform(test_ombrage, st_crs(ombrage2))
test_ombrage$meanOmbr1 = exactextractr::exact_extract(ombrage1, test_ombrage, 'mean')
test_ombrage$meanOmbr2 = exactextractr::exact_extract(ombrage2, test_ombrage, 'mean')
test_ombrage$meanOmbr3 = exactextractr::exact_extract(ombrage3, test_ombrage, 'mean')
st_write(test_ombrage, 'C:/Meghana/Belgique/decembre/traitements/UREC_water_ombrage.shp', delete_layer  = T)

test_ombrage$meanOmbrage = NA
i = 1
for (i in 1:nrow(test_ombrage)){
  new_val = NA
  #row = test[i,]
  if (!is.na(test_ombrage[i,]$meanOmbr2)){
    new_val = test_ombrage[i,]$meanOmbr2
  }
  if (!is.na(test_ombrage[i,]$meanOmbr3)){
    new_val = test_ombrage[i,]$meanOmbr3
  }
  test_ombrage[i,]$meanOmbrage = new_val
}

test_ombrage = st_cast(test_ombrage, to ='MULTIPOLYGON')
st_write(test_ombrage, 'C:/Meghana/Belgique/decembre/traitements/UREC_water_ombrage.shp', delete_layer  = T)
st_write(test_ombrage, 'C:/Meghana/Belgique/decembre/traitements/UREC_water_ombrage.SQLite', delete_layer  = T)

UREC_merge_omrbage = UREC_merge
test = test_ombrage
test = st_drop_geometry(test)
test = subset(test, select = c(id, meanOmbrage))
UREC_merge_omrbage = left_join(UREC_merge_omrbage, test, by = c('id'='id'))
UREC_merge_omrbage$meanOmbrage_adj = UREC_merge_omrbage$meanOmbrage/255

st_write(UREC_merge_omrbage, 'C:/Meghana/Belgique/decembre/traitements/UREC_merge_ombrage.shp', delete_layer  = T)
st_write(UREC_merge_omrbage, 'C:/Meghana/Belgique/decembre/traitements/UREC_merge_ombrage.SQLite', delete_layer  = T)




#Canope en surplomb sur la riviere
################################################
#preprocessing
###############
mask_urbain = rast('C:/Meghana/Belgique/decembre/traitements/MHC_canope1.tif')
mask_urbain_vectoriel = vect('C:/Meghana/Belgique/decembre/traitements/ut_urbain_vect.shp')

vrt_mhc_mask7 = rast('C:/Meghana/Belgique/decembre/traitements/MHC_mask/mhc7_mask.vrt')
#vrt_mhc_mask7_proj = terra::project(vrt_mhc_mask7, mask_urbain)
#writeRaster(vrt_mhc_mask7_proj,'C:/Meghana/Belgique/decembre/traitements/MHC_mask/mhc7_proj_mask.tif' )
vrt_mhc_mask8 = rast('C:/Meghana/Belgique/decembre/traitements/MHC_mask/MTM8/mhc8_mask.vrt')
#vrt_mhc_mask8_proj = terra::project(vrt_mhc_mask8, mask_urbain)


#Get extents of SpatRaster mhc
etendu7 = ext(vrt_mhc_mask7)#MHC7
etendu8 = ext(vrt_mhc_mask8) #MHC8

UREC_merge = vect("C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp")
UREC_water_merge = st_read('C:/Meghana/Belgique/traitements/results/surface_eau_mod/UREC_water/UREC_water_MaxCam/merge/UREC_water_merge.shp')
UREC_water_merge_vect = vect(UREC_water_merge)
UREC_water_merge_v7 = terra::project(UREC_water_merge_vect, 'EPSG:2949')
UREC_water_merge_v8 = terra::project(UREC_water_merge_vect, 'EPSG:2950')
#UREC_mergev7 = terra::project(UREC_merge, 'EPSG:2949')
#UREC_mergev8 = terra::project(UREC_merge, 'EPSG:2950')
#Clip UREC_v 7 and 8 to extent of mhc7 and 8 
UREC_water_merge_v7 = terra::crop(UREC_water_merge_v7, etendu7) #Cropped UREC_v7 to only include UREC that overlay the extent of raster mhc7
#UREC_water_merge_v7$CanopyRatio =NA
UREC_water_merge_v8 = terra::crop(UREC_water_merge_v8, etendu8)
#UREC_water_merge_v8$CanopyRatio =NA


i = 1


#Run OverhangingCanopy function on water merged 
UREC_water_merge_v7 = OverhangingCanopy(UREC_water = UREC_water_merge_v7, raster_file = vrt_mhc_mask7, EPSG = 'EPSG:2949', urban_vector_mask = mask_urbain_vectoriel)
UREC_water_merge_v8 = OverhangingCanopy(UREC_water = UREC_water_merge_v8, raster_file = vrt_mhc_mask8, EPSG = 'EPSG:2950', urban_vector_mask = mask_urbain_vectoriel)

#join UREC_water_merge_v7 and v8 to UREC_merge 
UREC_water_merge_v7_sub = UREC_water_merge_v7
UREC_water_merge_v7_sub = st_as_sf(UREC_water_merge_v7_sub)
UREC_water_merge_v7_sub = st_drop_geometry(UREC_water_merge_v7_sub)
UREC_water_merge_v7_sub = subset(UREC_water_merge_v7_sub, select = c(id, canopyRatio))



UREC_merge_omrbage_test = UREC_merge_omrbage
UREC_merge_omrbage_test = left_join(UREC_merge_omrbage_test,UREC_water_merge_v7_sub, by = c('id'='id') )

UREC_water_merge_v8_sub = UREC_water_merge_v8
UREC_water_merge_v8_sub = st_as_sf(UREC_water_merge_v8_sub)
UREC_water_merge_v8_sub = st_drop_geometry(UREC_water_merge_v8_sub)
UREC_water_merge_v8_sub = subset(UREC_water_merge_v8_sub, select = c(id, canopyRatio))
UREC_water_merge_v8_sub$canopyRatioMTM8 = UREC_water_merge_v8_sub$canopyRatio 
UREC_water_merge_v8_sub = subset(UREC_water_merge_v8_sub, select = c(id, canopyRatioMTM8))
UREC_merge_omrbage_test = left_join(UREC_merge_omrbage_test,UREC_water_merge_v8_sub, by = c('id'='id') )

#set new column name of UREC_merge and set all values to NA
UREC_merge_omrbage_test$CanopyRatio_new = NA
#for loop to put values from both canopyRatio mtm7 and mtm8 into new column
for (i in 1:nrow(UREC_merge_omrbage_test)){
  new_val = NA #set empty variable
  #row = test[i,]
  if (!is.na(UREC_merge_omrbage_test[i,]$canopyRatio)){#check that value of row at column canopyRatio is not NA
    new_val = UREC_merge_omrbage_test[i,]$canopyRatio #set new variable to that value
  }
  if (!is.na(UREC_merge_omrbage_test[i,]$canopyRatioMTM8)){#otherwise check that value of row at column canopyRatioMTM8 is not NA
    new_val = UREC_merge_omrbage_test[i,]$canopyRatioMTM8 #set new variable to that value
  }
  UREC_merge_omrbage_test[i,]$CanopyRatio_new = new_val #set new value to the row at column created previoulsy
}

st_write(UREC_merge_omrbage_test,'C:/Meghana/Belgique/decembre/results/UREC_merge_ombrage1.shp' )
st_write(UREC_merge_omrbage_test,'C:/Meghana/Belgique/decembre/results/UREC_merge_ombrage1.SQLite' )

#################################################################################################
#Normalise metrics for indice d'ombrage
#####################
UREC_merge_ombrage_nrm = st_read('C:/Meghana/Belgique/decembre/results/UREC_merge_ombrage1.SQLite')

UREC_merge_ombrage_nrm = subset(UREC_merge_ombrage_nrm, select = c(id, rive, id_uea, meanombrage_adj, canopyratio_new))
UREC_merge_ombrage_nrm = Normalization_function(UREC_merge_ombrage_nrm)

####################################################################################
#Do statistics on metrics
stats_ombrage = UREC_merge_ombrage_nrm
stats_ombrage_na = na.omit(stats_ombrage)
stats_ombrage_sub = stats_ombrage_na[, c('id', 'rive', 'id_uea','meanombrage_adj_nrm','canopyratio_new_nrm')]
stats_ombrage_sub = vect(stats_ombrage_sub)

stats_ombrage$meanombrage_adj_nrm = as.numeric(stats_ombrage$meanombrage_adj_nrm)
stats_ombrage$canopyratio_new_nrm = as.numeric(stats_ombrage$canopyratio_new_nrm )

stats_ombrage_na = na.omit(stats_ombrage_sub)
stats_ombrage_na = st_drop_geometry(stats_ombrage_na)

cor_ombrage = Correlation_matrix(df = stats_ombrage_na, var2 = 'Canopy and hillshading')
pca_ombrage_df = stats_ombrage_na[stats_ombrage_na, c('meanombrage_adj_nrm', 'canopyratio_new_nrm')]
pca_ombrage = PCA_graph_function(df = stats_ombrage_sub, df_name = 'Canopy and hillshading', axe = c(1,2))
#######################################################################################
#Make ombrage index based on results of statstics (we keep both metrics because they explain different parts of the variability and are not very correlated)
UREC_merge_ombrage_nrm$Indice_ombrage = (UREC_merge_ombrage_nrm$canopyratio_new_nrm +
                                           UREC_merge_ombrage_nrm$meanombrage_adj_nrm)/2

#plot(UREC_merge_ombrage_nrm$Indice_ombrage)
UREC_merge_ombrage_nrm_sub = na.omit(UREC_merge_ombrage_nrm)
plot(UREC_merge_ombrage_nrm_sub$Indice_ombrage, ylim = c(0,1))
st_write(UREC_merge_ombrage_nrm_sub, 'C:/Meghana/Belgique/decembre/results/indiceOmbrage.shp')
st_write(UREC_merge_ombrage_nrm_sub, 'C:/Meghana/Belgique/decembre/results/indiceOmbrage.SQLite')
save.image()

#######################################################################################################
######################################################################################################
#MISE EN PLACE DE L'ISEER
###########################################################################################
#Load UREC_merge with indices for connectiivty and ombrage
UREC_contP = st_read('C:/Meghana/Belgique/decembre/results/results_new1_indice.SQLite')
UREC_contP = subset(UREC_contP, select = c( id, continuity_vegetation_optiamle_nrm , pd_vegetation_optimale_nrm ,indice_nat_nrm))
df_UREC_contP = st_drop_geometry(UREC_contP)
UREC_ombrage = st_read('C:/Meghana/Belgique/decembre/results/indiceOmbrage.SQLite')
UREC_ombrage = subset(UREC_ombrage, select = c(id_uea, rive, id,meanombrage_adj_nrm, canopyratio_new_nrm))
UREC_ISEER = left_join(UREC_ombrage,df_UREC_contP, by = c('id'='id'))
UREC_ISEER$meanombrage_adj_nrm = as.numeric(UREC_ISEER$meanombrage_adj_nrm)
UREC_ISEER$canopyratio_new_nrm = as.numeric(UREC_ISEER$canopyratio_new_nrm)
UREC_ISEER$continuity_vegetation_optiamle_nrm = as.numeric(UREC_ISEER$continuity_vegetation_optiamle_nrm )
UREC_ISEER$pd_vegetation_optimale_nrm = as.numeric(UREC_ISEER$pd_vegetation_optimale_nrm)
UREC_ISEER$indice_nat_nrm = as.numeric(UREC_ISEER$indice_nat_nrm)
UREC_ISEER = vect(UREC_ISEER)
#Do statistics on metrics from each function 

stats_UREC_ISEER = Correlation_matrix(df = UREC_ISEER, var2 = 'metrics ConP and Ombrage')
PCA_UREC_ISEER = PCA_graph_function(df = UREC_ISEER, df_name = 'ConnectP and Ombrage', axe = c(1,2))

UREC_ISEER$ISEER = (UREC_ISEER$meanombrage_adj_nrm +
                      UREC_ISEER$canopyratio_new_nrm+
                      UREC_ISEER$pd_vegetation_optimale_nrm+
                      UREC_ISEER$indice_nat_nrm+
                      UREC_ISEER$continuity_vegetation_optiamle_nrm)/5

UREC_ISEER_sf = st_as_sf(UREC_ISEER)
st_write(UREC_ISEER_sf,'C:/Meghana/Belgique/decembre/results/UREC_ISEER.SQLITE')
st_write(UREC_ISEER_sf,'C:/Meghana/Belgique/decembre/results/UREC_ISEER.shp')
#Plot ISEER
windows(10,5)

plot(UREC_ISEER$ISEER, ylim = c(0,1))
save.image()


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
  

                     





