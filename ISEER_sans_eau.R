#ISEER sans eau

#Load data 
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water.shp')

#Data from FE reg temp 
UREC_temp = st_read('C:/Meghana/Belgique/decembre/traitements/Temperature/UREC_OverhangingCan_full_nrm.sqlite' )
names(UREC_temp)
keep_temp = c('rive', 'id', 'id_uea', 'canratio')
UREC_temp = UREC_temp[,names(UREC_temp)%in% keep_temp]
#UREC_temp = st_drop_geometry(UREC_temp)
UREC_temp$overhanging  = UREC_temp$canratio
drop_canR = c("canratio")
UREC_temp = UREC_temp[,!names(UREC_temp) %in% drop_canR]
names(UREC_temp)

#Data from FE contuinity du paysage
UREC_ConntP = st_read('C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_ContP_final.sqlite')
names(UREC_ConntP)
keep_conntP = c("id", "id_uea", "rive" ,"indice_nat_nrm" ,
                "inv_anthropique", "inv_pd_vegopt")
UREC_ConntP = UREC_ConntP[,names(UREC_ConntP) %in% keep_conntP]
UREC_ConntP= st_drop_geometry(UREC_ConntP)

#Data from FE Maintien Hab terrestre
UREC_hab = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/UREC_metrics_indice_Habitat.sqlite')
names(UREC_hab)
keep_hab = c("rive","id","id_uea","meanheight_nrm", 
             "vegopt_nrm","nbr_class_nrm","meanwidth_nrm",
             "inv_anthro")
UREC_hab = UREC_hab[,names(UREC_hab)%in% keep_hab]
UREC_hab = st_drop_geometry(UREC_hab)
#Data from FE Reg Productivity 
UREC_prod = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_prod_aqua_f.sqlite')
names(UREC_prod)
keep_prod = c( "id","rive","id_uea" ,"vegopt_nrm" ,
               "vegetation_nrm",  "hepb_nrm",
               "inv_pente"   ,   "inv_hydcon" )
UREC_prod = UREC_prod[,names(UREC_prod)%in% keep_prod]
UREC_prod = st_drop_geometry(UREC_prod)

#Combine all data 
ISEER1 = left_join(UREC_temp,UREC_prod, by = 'id')
ISEER1 = left_join(ISEER1,UREC_hab, by = 'id')
ISEER1 = left_join(ISEER1,UREC_ConntP, by = 'id')
#ISEER1 = left_join(ISEER1,UREC_temp, by = 'id')
names(ISEER1)
ISEER1$rive = ISEER1$rive.x
ISEER1$id_uea = ISEER1$id_uea.x
keep_ISEER1 = c( "id","id_uea", "rive" ,
                 "vegopt_nrm.x" ,   "vegetation_nrm",  "hepb_nrm" , 
                 "inv_pente","inv_hydcon","meanheight_nrm",
                 "nbr_class_nrm", "meanwidth_nrm","inv_anthro",
                 "indice_nat_nrm", "inv_pd_vegopt", "overhanging")
ISEER1 = ISEER1[, names(ISEER1) %in% keep_ISEER1]
names(ISEER1)
ISEER1 = ISEER1 %>% drop_na() #Drop rows with NA (because lack of data on size of river)
#st_write(ISEER1, 'C:/Meghana/Belgique/decembre/traitements/ISEER/ISEER_eau/metriques_potentielles_ISEER_nrm.shp', delete_layer = T)
#st_write(ISEER1, 'C:/Meghana/Belgique/decembre/traitements/ISEER/ISEER_eau/metriques_potentielles_ISEER_nrm.sqlite',delete_layer = T)

#DO correlation of metrics 
ISSER1 = st_read('C:/Meghana/Belgique/decembre/traitements/ISEER/ISEER_eau/metriques_potentielles_ISEER_nrm.sqlite')
ISEER1_v = vect(ISEER1) #Transform sf object into vect obj

cor_ISEER1_v = Correlation_matrix(df = ISEER1_v, var2 = 'Metriques potentielles ISEER')
pca_ISEER1_v = PCA_graph_function(df = ISEER1_v, df_name = 'métriques potentielles ISEER', axe = c(1,2))

#Premier jet pour l'ISEER

ISEER1_v$iseer1 = (ISEER1_v$overhanging + ISEER1_v$vegetation_nrm +
                     ISEER1_v$inv_pd_vegopt + ISEER1_v$inv_pente + 
                     ISEER1_v$nbr_class_nrm + ISEER1_v$hepb_nrm + 
                     ISEER1_v$meanwidth_nrm) /7
#Take into consideration double overhanging vegetation score
ISEER1_v$iseer2 = (ISEER1_v$overhanging + ISEER1_v$overhanging +ISEER1_v$vegetation_nrm +
                     ISEER1_v$inv_pd_vegopt + ISEER1_v$inv_pente + 
                     ISEER1_v$nbr_class_nrm + ISEER1_v$hepb_nrm + 
                     ISEER1_v$meanwidth_nrm ) /8
writeVector(ISEER1_v, 'C:/Meghana/Belgique/decembre/traitements/ISEER/ISEER_eau/ISSER1.sqlite', filetype = 'SQLITE', overwrite = T)

windows(10,5)
plot(ISEER1_v$iseer2)

###Try by removing indice de naturalite 
drop_ind_nat = c("indice_nat_nrm", 'iseer1')
ISEER2_v = ISEER1_v[,!names(ISEER1_v) %in% drop_ind_nat]
names(ISEER2_v)
pca_ISSER2_v= PCA_graph_function(df = ISEER2_v, df_name = 'metriques potentielles 2', axe = c(1,2))

ISEER2_v$Fe = (ISEER1_v$overhanging + ISEER1_v$vegetation_nrm +
                 ISEER1_v$inv_anthro
                 + ISEER1_v$inv_pente + 
                 ISEER1_v$nbr_class_nrm + ISEER1_v$hepb_nrm + 
                 ISEER1_v$meanwidth_nrm ) /7
#take into consideration double inv anthropique score 
ISEER2_v$Fe2 = (ISEER1_v$overhanging + ISEER1_v$vegetation_nrm +
                 ISEER1_v$inv_anthro + ISEER1_v$inv_anthro +
                  ISEER1_v$inv_pente + ISEER1_v$nbr_class_nrm +
                  ISEER1_v$hepb_nrm +ISEER1_v$meanwidth_nrm ) /8

ISSER2$Fe3 =

writeVector(ISEER2_v, 'C:/Meghana/Belgique/decembre/traitements/ISEER/ISEER_eau/ISSER2.sqlite', filetype = 'SQLITE', overwrite = T)

windows(10,5)
plot(ISEER2_v$Fe2)
