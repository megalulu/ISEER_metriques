#ISEER

#Load data 
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')

#Ombrage 
UREC_ombrage = st_read('C:/Meghana/Belgique/decembre/results/UREC_metric_canope_full.shp')
#normalise canRatio values 
UREC_ombrage$T_canopyRati_nrm = (UREC_ombrage$canopyRati - min(UREC_ombrage$canopyRati))/ (max(UREC_ombrage$canopyRati)-min(UREC_ombrage$canopyRati))
keep_ombrage = c('T_canopyRati_nrm', 'id', 'Id_UEA', 'rive')
UREC_ombrage_sub = UREC_ombrage[,(names(UREC_ombrage)%in% keep_ombrage)]
names(UREC_ombrage_sub)
#Connectivity Paysage
UREC_ConP = st_read('C:/Meghana/Belgique/decembre/results/results_new1_indice.SQlite')
keep_ConP = c('id_uea', 'rive', 'id', 'continuity_vegetation_optiamle_nrm', 'pd_vegetation_optimale_nrm','indice_nat_nrm')
UREC_ConP_sub = UREC_ConP[,(names(UREC_ConP)%in% keep_ConP)]
names(UREC_ConP )
UREC_ConP_sub = st_drop_geometry(UREC_ConP_sub)

#Maintien et creation d'habitat
UREC_hab = st_read("C:/Meghana/Belgique/decembre/traitements/FE_maintient_Hab_VegOptimal.shp")
names(UREC_hab)
keep_hab = c('id_uea', 'rive', 'id','VegRatio_o','inv_anthro','mean_heigh','CanRatio_n')
UREC_hab_sub = UREC_hab[UREC_hab, (names(UREC_hab)%in% keep_hab)]
head(UREC_hab_sub)
UREC_hab_sub = st_drop_geometry(UREC_hab_sub)
#Regulation de la productivity 
UREC_prod = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_urb_srf3_nrm.sqlite')
names(UREC_prod)
# UREC_prod$inv_agricole_nrm = -UREC_prod$agricole_nrm +1
# UREC_prod$inv_anthropique_nrm = -UREC_prod$anthropiqu_nrm +1
# UREC_prod$inv_avrslope_nrm = -UREC_prod$avrslope_nrm +1
names(UREC_prod)

keep_prod = c('id_uea', 'rive', 'id', "inv_avrslope_nrm", "hepb_nrm", "inv_anthropique_nrm", "inv_agrianthro_nrm")
UREC_prod_sub = UREC_prod[,(names(UREC_prod)%in% keep_prod)]
UREC_prod_sub = st_drop_geometry(UREC_prod_sub)
#combine the tables together into metrics potentielles for ISEER
UREC_ISEER1 = UREC_ombrage_sub
UREC_ISEER1 = left_join(UREC_ISEER1, UREC_ConP_sub, by = c('id'))
UREC_ISEER1 = left_join(UREC_ISEER1, UREC_hab_sub, by = c('id'))
UREC_ISEER1 = left_join(UREC_ISEER1, UREC_prod_sub, by = c('id'))
names(UREC_ISEER1)
UREC_ISEER1$rive = UREC_ISEER1$rive.x

#Get rid of unecessary columns 
drop_iseer1 = c("Id_UEA", "rive.x", "id_uea.x", "rive.y","id_uea.y", "rive.x.x" ,   "rive.y.y")
UREC_ISEER1 = UREC_ISEER1[,(!names(UREC_ISEER1)%in% drop_iseer1)]
names(UREC_ISEER1)
UREC_ISEER1_v = vect(UREC_ISEER1)
#DO statistics on ISEER 
cor_ISEER1 = Correlation_matrix(df = UREC_ISEER1_v, var2 = 'Initial metrics for first batch ISEER')
write.csv(cor_ISEER1,file = 'C:/Meghana/Belgique/decembre/traitements/tbl_correlation_ISEER1.csv', row.names = FALSE)

#Some sub PCA based on correlation plot. How to decide with correlated metrics to pick 
#Veg Metrics : VegRatio, pd_VegOpti, ContinuitVegOpt, inv_anthro
keep_veg = c(
  "id",
  "rive",
  "id_uea",
  "VegRatio_o",
  "continuity_vegetation_optiamle_nrm",
  "pd_vegetation_optimale_nrm",
  "inv_anthro"
)

ISEER1_veg = UREC_ISEER1_v[,(names(UREC_ISEER1_v)%in% keep_veg)]
pca_ISEER1_veg = PCA_graph_function(df = ISEER1_veg, df_name = 'PCA metrics veg', axe = c(1,2))

#Indice de naturalite vs mean ehight vs inv_slope 
keep_nat = c("id", "rive","id_uea","inv_avrslope_nrm","mean_heigh", "indice_nat_nrm")
ISEER1_nat = UREC_ISEER1_v[,(names(UREC_ISEER1_v)%in% keep_nat)]
pca_ISEER1_nat = PCA_graph_function(df = ISEER1_nat, df_name ='PCA metrics indice de nat', axe = c(1,2))


#Faire un deuxieme indice de ISEER avec les m/triques selectionner
drop_iseer2 = c("continuity_vegetation_optiamle_nrm", "pd_vegetation_optimale_nrm","indice_nat_nrm" , "inv_avrslope_nrm","inv_anthro")
UREC_ISEER2 = UREC_ISEER1_v[,(!names(UREC_ISEER1_v)%in% drop_iseer2)]
pca_ISEER2 = PCA_graph_function(df = UREC_ISEER2, df_name = 'Metriques initiales non correles', axe = c(1,2))

#Créer l'indice avec les métriques sélectionnées 
UREC_ISEER2$FE1 = (UREC_ISEER2$T_canopyRati_nrm  +UREC_ISEER2$inv_agrianthro_nrm+
                     UREC_ISEER2$mean_heigh + UREC_ISEER2$inv_anthropique_nrm+
                     UREC_ISEER2$VegRatio_o)/5
writeVector(UREC_ISEER2, 'C:/Meghana/Belgique/decembre/traitements/ISEER/indice_ISEER1.sqlite', filetype = 'SQLITE')
writeVector(UREC_ISEER2, 'C:/Meghana/Belgique/decembre/traitements/ISEER/indice_ISEER1.shp')
min(UREC_ISEER2$FE1)
max(UREC_ISEER2$FE1)

windows(10,5)
plot(UREC_ISEER2$FE1)


###Creer l'ISEER avec les m/triques selectionee
keep_ISEER3 =  c("id", "rive","id_uea",'inv_anthropique_nrm',"VegRatio_o",
                 
                 "T_canopyRati_nrm" ,"CanRatio_n" , "inv_agricole_nrm", 'indice_nat_nrm')
UREC_ISEER3 = UREC_ISEER1_v[,(names(UREC_ISEER1)%in% keep_ISEER3)]
UREC_ISEER3$ISEER1 = (UREC_ISEER3$VegRatio_o+UREC_ISEER3$T_canopyRati_nrm+
                        UREC_ISEER3$inv_anthropique_nrm + UREC_ISEER3$CanRatio_n +
                        UREC_ISEER3$inv_agricole_nrm)/5
plot(UREC_ISEER3$ISEER1)

#On attribue des poids selon le nombre de fois que la metriques revient dans les indices de FE 
#inv anthropique revient 2 fois dans les indices de FE alors elle compte pour 2x le poids des autres m/triques 

UREC_ISEER3$ISEER2 = UREC_ISEER3$VegRatio_o/6+ UREC_ISEER3$T_canopyRati_nrm/6+
                        UREC_ISEER3$inv_anthropique_nrm/3 + UREC_ISEER3$CanRatio_n/6 +
                        UREC_ISEER3$inv_agricole_nrm/6
plot(UREC_ISEER3$ISEER2)

minmax(UREC_ISEER3$ISEER2)