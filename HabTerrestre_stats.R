#New script to normalize and do stats on maintien de l'habitat terrestre

#Normalisation
source('normalization_functions.R')
librayr(dp)
UREC_maintienHabT3 <- read.csv2("C:/Meghana/Belgique/decembre/traitements/UREC_maintienHabT3.csv")
UREC_maintienHabT3[UREC_maintienHabT3 == ''] <- NA
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
df = left_join(UREC_merge, UREC_maintienHabT3, "id")
df$agricole = as.numeric(df$agricole)
df$anthropiqu = as.numeric(df$anthropiqu)
df$aquatique = as.numeric(df$aquatique)
df$forestier = as.numeric(df$forestier)
df$humide= as.numeric(df$humide)
df$non_classifie= as.numeric(df$non_classifie)
df$coupe_regen = as.numeric(df$coupe_regen)
df$sol_nu = as.numeric(df$sol_nu)
df$coupe_regen[is.na(df$coupe_regen)] <- 0

#Replace NA with 0 (because all the columns make more sense that way )
df <- replace(df, is.na(df), 0)

#Run normalisation function 
UREC_hab_nrm = Normalization_function(UREC_merge= df)
#Write files
st_write(UREC_hab_nrm, "C:/Meghana/Belgique/decembre/traitements/UREC_maintient_hab_normalise2.shp")
UREC_hab_nrm_v = vect(UREC_hab_nrm)
writeVector(UREC_hab_nrm_v, "C:/Meghana/Belgique/decembre/traitements/UREC_maintient_hab_normalise2.sqlite",filetype = 'SQLite', overwrite = T )
#Create a csv just in case 
t = as.data.frame(UREC_hab_nrm)
write.csv(t, 'C:/Meghana/Belgique/decembre/traitements/UREC_maintient_hab_normalise2.csv')
#########################
#Correlation and PCA
cor_hab = vect(UREC_hab_nrm)
cor_hab$rive = cor_hab$rive.x
drop <- c('Id_UEA',"ogc_fid", 'rive.x', 'rive.y',
          'vegratio',
          'nbr_class',
          'agricole',
          'anthropiqu',
          'aquatique',
          'forestier',
          'humide',
          'non_classifie',
          'coupe_regen',
          'sol_nu',
          'vegetation_ratio',
          'Nbr_class',
          'mean_height',
          'median_height',
          'sd_height',
          'majority_height',
          'CanRatio', 
          'Nbr_class_nrm',
          'vegetation_ratio_nrm')

cor_hab = cor_hab[,!(names(cor_hab) %in% drop)]

cor_matrix_hab = Correlation_matrix(df = cor_hab, var2 = 'Hab')

pca_cor_matrix_hab = PCA_graph_function(df = cor_hab, df_name = 'Habitat _all', axe = c(1,2))


#Check PCA o, 'f correlated metrix to choose one 
keep <- c("id","id_uea", 'rive', 'aquatique_nrm', 'vegratio_nrm' )
df_aqua_VegR =  cor_hab[,(names(cor_hab) %in% keep)]
pca_cor_aquatique_vegRatio = PCA_graph_function(df_aqua_VegR, df_name = 'aquatique vs Veg Ratio', axe = c(1,2))

keep1 <- c("id","id_uea", 'rive', 'mean_height_nrm', 'median_height_nrm', 'sd_height_nrm','majority_height_nrm' )
df_tree_stats =  cor_hab[,(names(cor_hab) %in% keep1)]

pca_tree_stats = PCA_graph_function(df = df_tree_stats, df_name = 'TreeStats', axe = c(1,2))


#Do PCA on final decision for metriques in round 1
drop1 <- c('aquatique_nrm', 'median_height_nrm', 'sd_height_nrm','majority_height_nrm' )
df_hab_metriques1 = cor_hab[,!(names(cor_hab) %in% drop1)]
PCA_hab_metriques1 = PCA_graph_function(df = df_hab_metriques1, df_name = 'Habitat metriques 1', axe = c(1,2))
PCA_hab_metriques2 = PCA_graph_function(df = df_hab_metriques1, df_name = 'Habitat metriques ', axe = c(1,3))
PCA_hab_metriques3 = PCA_graph_function(df = df_hab_metriques1, df_name = 'Habitat metriques ', axe = c(2,3))

#Do PCA on final selection of metriques to verify that they are not correlated
keep2 <- c("id","id_uea", 'rive', 'vegratio_nrm','nbr_class_nrm','agricole_nrm','forestier_nrm', 'humide_nrm',
           'mean_height_nrm', 'anthropiqu_nrm', 'CanRatio_nrm')
df_hab_metriques_f1 = cor_hab[,(names(cor_hab) %in% keep2)]
cor_df_hab_metriques_f1 = Correlation_matrix(df = df_hab_metriques_f1, var2 = 'Correlation of final metrics')
PCA_df_hab_metriques_f1 = PCA_graph_function(df = df_hab_metriques_f1, df_name = 'PCA final metrics', axe = c(1,2))
PCA_df_hab_metriques_f1_axe2 = PCA_graph_function(df = df_hab_metriques_f1, df_name = 'PCA final metrics', axe = c(2,3))
PCA_df_hab_metriques_f1_axe3 = PCA_graph_function(df = df_hab_metriques_f1, df_name = 'PCA final metrics', axe = c(1,3))

######################################################################
#Create first index
UREC_hab = df_hab_metriques_f1
UREC_hab$Fe1 = (UREC_hab$vegratio_nrm+UREC_hab$nbr_class_nrm+
                  UREC_hab$agricole_nrm+UREC_hab$anthropiqu_nrm+
                  UREC_hab$forestier_nrm+UREC_hab$humide_nrm+
                  UREC_hab$mean_height_nrm+UREC_hab$CanRatio_nrm)/8
UREC_hab$Fe2 = (UREC_hab$vegratio_nrm+UREC_hab$nbr_class_nrm+
                  UREC_hab$forestier_nrm+
                  UREC_hab$mean_height_nrm+UREC_hab$CanRatio_nrm)/6

old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
windows(10,5)
plot(UREC_hab$Fe2)



# UREC_hab = st_read('C:/Meghana/Belgique/decembre/traitements/UREC_maintienHabT2.shp')
# UREC_hab = st_as_sf(UREC_hab)
# UREC_hab$non_classifie = UREC_hab$"non classifi\xe3\x83\xe2\xa9"
# UREC_hab %>% 
#   rename( "non_classifie"= "non classifi\xe3\x83\xe2\xa9")
# 
# colnames(UREC_hab) <- c(
# #   'ogc_fid0',
# #   'id_uea',
# #   'rive',
# #   'id',
#   'vegratio',
#   'nbr_class',
#   'agricole',
#   'anthropique',
#   'aquatique',
#   'forestier',
#   'humide',
#   'non_classifie',
#   'coupe_regen',
#   'sol_nu_lande',
#   'vegratio',
#   'nbr_class',
#   'meanheight',
#   'medianheight',
#   'sdheight',
#   'majorityheight',
#   'canopyrati')
# 
# UREC_hab = as.data.frame(UREC_hab)
# UREC_hab = st_drop_geometry(UREC_hab)
# UREC_hab$
# 
# df = left_join(UREC_merge, UREC_hab, "id")
# 
# 
# UREC_hab = st_as_sf(UREC_hab) %>% st_cast(to = 'POLYGON')
# drop <- c("ogc_fid0")
# df = UREC_hab[,!(names(UREC_hab) %in% drop)]
# #df = st_as_sf(df)
# UREC_hab_norm = Normalization_function(UREC_merge = df)
