#Maintient de l<habitat avec water set up 

#Load all the UREC files we need to complete the metrics 
#1. Tree stats : 
UREC_tree_stats = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/treeStats_full.shp')
#2. CanRatio : (THis has been rejected because it does not seem correlated with forests... so it must be wrong some how?)
UREC_canRatio = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/CanRatio_full.sqlite')
UREC_canRatio = st_drop_geometry(UREC_canRatio)
#3. Ratio des surfaces vegetale 
UREC_vegetation = st_read("C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_surfaces_UT_sans_eau.sqlite")
UREC_vegetation = st_drop_geometry(UREC_vegetation)
#4. Nombre de classes
UREC_nbr_class = st_read('C:/Meghana/Belgique/decembre/traitements/Nbr_class.sqlite')
drop = c('vegratio')
UREC_nbr_class = UREC_nbr_class[,!names(UREC_nbr_class)%in% drop] #drop old vegratio to avoir confusion with new veg ratio 
UREC_nbr_class = st_drop_geometry(UREC_nbr_class)
#5. unit width
UREC_width = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/UREC_lateral_width.shp')
UREC_width = st_drop_geometry(UREC_width)

#Put all these metrics together into one shapefile
UREC_habitat_water = UREC_tree_stats
UREC_habitat_water = cbind(UREC_habitat_water, UREC_vegetation, by = c('id'))
UREC_habitat_water = cbind(UREC_habitat_water, UREC_nbr_class, by = c('id'))
UREC_habitat_water = cbind(UREC_habitat_water, UREC_width, by = c('id'))
UREC_habitat_water = cbind(UREC_habitat_water, UREC_canRatio, by = c('id'))
names(UREC_habitat_water)
UREC_habitat_water[is.na(UREC_habitat_water)] <- 0
keep_hab_water = c( "id_uea","rive","id",
                    "meanHeight" ,"medianHeig","majorityHe","sdHeight",
                    "anthropique","forestier","agricole", "humide",
                    "vegopt", "nbr_class" ,  "meanwidth" ) #you could add "canopyratio"
UREC_habitat_water = UREC_habitat_water[,names(UREC_habitat_water)%in% keep_hab_water]
names(UREC_habitat_water)
st_write(UREC_habitat_water, 'C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/UREC_habitat_metrics_full.shp', overwrite = T)
st_write(UREC_habitat_water, 'C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/UREC_habitat_metrics_full.sqlite', delete_layer  = T)

#########Do statistiques
#normalise the data 
UREC_habitat_water_nrm = Normalization_function(UREC_merge = UREC_habitat_water)
st_write(UREC_habitat_water_nrm, 'C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/UREC_habitat_metrics_full_nrm.sqlite', delete_layer = T)

drop_hab_water1 = c("meanHeight" ,"medianHeig","majorityHe","sdHeight",
                    "anthropique","forestier","agricole", "humide",
                    "vegopt", "nbr_class" ,  "meanwidth" , "canopyratio")

UREC_habitat_water_nrm1 =  UREC_habitat_water_nrm[,!names(UREC_habitat_water_nrm)%in% drop_hab_water1]
names(UREC_habitat_water_nrm1)

#Convert UREC_habitat_water_nrm1 into spatVector
UREC_habitat_water_nrm1_v = vect(UREC_habitat_water_nrm1)
#Do correlation matrix 
cor_habt_water = Correlation_matrix(df = UREC_habitat_water_nrm1_v, var2 = 'water_all metrics')
write.table(cor_habt_water,
            file = 'C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/correlation_plot_allmetrics.csv' ,
            sep = ",",
            row.names = TRUE, )
# Do PCA on variables that are correlated related to tree stats
keep_stats = c("id_uea","rive","id","meanHeight_nrm" , "medianHeig_nrm" , "sdHeight_nrm" ,   "majorityHe_nrm")
pca_treeStats = UREC_habitat_water_nrm1_v[,names(UREC_habitat_water_nrm1_v) %in%keep_stats]
pca_treeStats1 = PCA_graph_function(df = pca_treeStats, df_name = 'Tree stats', axe = c(1,2))
            
# Do PCA on variables that are correlated related to Vegetation Optiamle
keep_vegOp = c("id_uea","rive","id","forestier_nrm",  "humide_nrm" ,"vegopt_nrm")
pca_vegOP = UREC_habitat_water_nrm1_v[,names(UREC_habitat_water_nrm1_v) %in%keep_vegOp]
pca_vegOP1 = PCA_graph_function(df = pca_vegOP, df_name = 'VegOpt', axe = c(1,2))


# Do PCA on variables that are left
drop_full = c("forestier_nrm",  "humide_nrm" ,"medianHeig_nrm" , "sdHeight_nrm" ,   "majorityHe_nrm")
pca_full = UREC_habitat_water_nrm1_v[,!names(UREC_habitat_water_nrm1_v) %in%drop_full]
pca_full = PCA_graph_function(df = pca_full, df_name = 'full metrics', axe = c(1,2))

#Create UREC with final metrics and calculate indice de FE
drop_urec_hab = c("agricole_nrm", "forestier_nrm",  "humide_nrm" ,"medianHeig_nrm" , "sdHeight_nrm" ,   "majorityHe_nrm")
UREC_hab_full = UREC_habitat_water_nrm1_v[, !names(UREC_habitat_water_nrm1_v)%in% drop_urec_hab]
names(UREC_hab_full)
UREC_hab_full$inv_anthro = -UREC_hab_full$anthropique_nrm +1
UREC_hab_full$FE = (
  UREC_hab_full$meanwidth_nrm + UREC_hab_full$nbr_class_nrm
  + UREC_hab_full$vegopt_nrm + UREC_hab_full$inv_anthro
  + UREC_hab_full$meanHeight_nrm
) / 5

writeVector(UREC_hab_full,'C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/UREC_metrics_indice_Habitat.shp' )
writeVector(UREC_hab_full,'C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/UREC_metrics_indice_Habitat.sqlite', filetype =  'SQLITE')

windows(10,5)
plot(UREC_hab_full$FE)