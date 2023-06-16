#Fonction de productivite des ER 
#Mise en place de la table d'attribue de UREC
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')

UREC_m1 = st_read('C:/Meghana/Belgique/decembre/traitements/SurfaceClass.sqlite')
names(UREC_m1)
#UREC_m1 = df
UREC_m1$rive = UREC_m1$rive.x
keep_m1 = c( 'id', 'agricole', 'anthropique')
UREC_m1 =  UREC_m1[,(names(UREC_m1) %in% keep_m1)]
UREC_m1 = st_drop_geometry(UREC_m1)
UREC_m1$agricole = as.numeric(UREC_m1$agricole)
UREC_m1$anthropique = as.numeric(UREC_m1$anthropique)
UREC_m1["anthropique"][is.na(UREC_m1["anthropique"])] <- 0
UREC_m1["agricole"][is.na(UREC_m1["agricole"])] <- 0
UREC_m1$AgriAnthro = UREC_m1$agricole+UREC_m1$anthropique
names(UREC_m1)
#Open Vegoptimale ratio (from FE_maintient Hab)
UREC_vegOpt = st_read('C:/Meghana/Belgique/decembre/traitements/FE_maintient_Hab_VegOptimal.shp')
keep_VegOpt = c('id', 'VegRatio_o')
UREC_vegOpt = UREC_vegOpt[,names(UREC_vegOpt)%in% keep_VegOpt]
UREC_vegOpt = st_drop_geometry(UREC_vegOpt)
names(UREC_vegOpt)
#Siigsol 
UREC_sol = st_read('C:/Meghana/Belgique/decembre/traitements/results_urec_hydrocond.shp')
UREC_sol = st_drop_geometry(UREC_sol)
#Siigsol urbain
UREC_sol_urb = st_read('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/results_urec_hydrocond_maskUrb.shp')
#UREC_sol_urb["HydCond"][is.na(UREC_sol_urb["HydCond"])] <-0
UREC_sol_urb = st_drop_geometry(UREC_sol_urb)
UREC_sol_urb["HydCond"][is.na(UREC_sol_urb["HydCond"])] <-0

#Pente
UREC_pente = st_read('C:/Meghana/Belgique/decembre/traitements/UREC_slope.shp')
UREC_pente = st_drop_geometry(UREC_pente)
#Hand 
UREC_hand = st_read('C:/Meghana/Belgique/decembre/traitements/Hauteur_emerge/UREC_hepb.shp')
UREC_hand = st_drop_geometry(UREC_hand)
#Hand urb 
UREC_hand_urb = st_read('C:/Meghana/Belgique/decembre/traitements/Hauteur_emerge/UREC_hepb_mask_urb.shp')
UREC_hand_urb= st_drop_geometry(UREC_hand_urb)
UREC_hand_urb["hepb"][is.na(UREC_hand_urb["hepb"])] <- 0

#################
UREC_prod = left_join(UREC_merge, UREC_m1, by = 'id', copy = FALSE)
UREC_prod = left_join(UREC_prod, UREC_vegOpt, by = 'id', copy = FALSE)
UREC_prod = left_join(UREC_prod, UREC_sol, by = 'id', copy = FALSE)
UREC_prod = left_join(UREC_prod, UREC_pente, by = 'id', copy = FALSE)
UREC_prod = left_join(UREC_prod, UREC_hand, by = 'id', copy = FALSE)

UREC_prod$rive = UREC_prod$rive.x
UREC_prod$id_uea = UREC_prod$Id_UEA.x

#drop_prod = c("rive.x", "Id_UEA.x", "rive.y", "Id_UEA.y", "rive.x.x", "Id_UEA.y.y", "rive.y.y", "Id_UEA.x.x" )
drop_prod = c("Id_UEA.x", "rive.x", "Id_UEA.y","rive.y", "Id_UEA.x.x" , "rive.x.x",
              "Id_UEA.y.y",  "rive.y.y" )
UREC_prod =  UREC_prod[,!(names(UREC_prod) %in% drop_prod)]
names(UREC_prod)
# st_write(UREC_prod, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod.shp')
# st_write(UREC_prod, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod.sqlite')



###########
#Do join for metrics masked for urban areas 
UREC_prod_urb = left_join(UREC_merge, UREC_m1, by = 'id', copy = FALSE)
UREC_prod_urb = left_join(UREC_prod_urb, UREC_vegOpt, by = 'id', copy = FALSE)
UREC_prod_urb = left_join(UREC_prod_urb, UREC_sol_urb, by = 'id', copy = FALSE)
UREC_prod_urb = left_join(UREC_prod_urb, UREC_pente, by = 'id', copy = FALSE)
UREC_prod_urb = left_join(UREC_prod_urb, UREC_hand_urb, by = 'id', copy = FALSE)
UREC_prod_urb$rive = UREC_prod_urb$rive.x
UREC_prod_urb$id_uea = UREC_prod_urb$Id_UEA.x
drop_prod_urb = c("Id_UEA.x", "rive.x","rive.y", "Id_UEA.y", "rive.x.x",  "Id_UEA","rive.y.y")
UREC_prod_urb =  UREC_prod_urb[,!(names(UREC_prod_urb) %in% drop_prod_urb)]#use same drop list as previously
names(UREC_prod_urb)


####Load data from new surface test 
UREC_surface_new = test_surface
UREC_surface_new$AgriAnthro = UREC_surface_new$Anthropique+UREC_surface_new$Agricole

#Creat new prod data set 
UREC_prod_urb2 = left_join(UREC_surface_new, UREC_vegOpt, by = 'id', copy = FALSE)
UREC_prod_urb2 = left_join(UREC_prod_urb2, UREC_sol_urb, by = 'id', copy = FALSE)
UREC_prod_urb2 = left_join(UREC_prod_urb2, UREC_pente, by = 'id', copy = FALSE)
UREC_prod_urb2 = left_join(UREC_prod_urb2, UREC_hand_urb, by = 'id', copy = FALSE)
UREC_prod_urb2$rive = UREC_prod_urb2$rive.x
UREC_prod_urb2$id_uea = UREC_prod_urb2$Id_UEA.x
drop_prod_urb2 = c("Id_UEA.x", "rive.x","rive.y", "Id_UEA.y", "rive.x.x",  "Id_UEA","rive.y.y")
UREC_prod_urb2 =  UREC_prod_urb2[,!(names(UREC_prod_urb2) %in% drop_prod_urb2)]#use same drop list as previously
names(UREC_prod_urb2)



################################################################
#Normalization of metriques productivite
#UREC_prod_v = vect(UREC_prod)
UREC_prod_nrm = Normalization_function(UREC_merge = UREC_prod)
# st_write(UREC_prod_nrm, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.shp')
# st_write(UREC_prod_nrm, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.sqlite')
prod_nrm = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.sqlite')
prod_nrm = UREC_prod_nrm
names(prod_nrm)#prod_nrm = UREC_prod_nrm
keep_nrm = c("id","id_uea",
  "rive",
  "agricole_nrm",
  "anthropique_nrm",
  "HydCond_nrm",
  "VegRatio_o_nrm",
  "avrSlope_nrm",
  "hepb_nrm"
)

keep_nrm = c("id","id_uea",
               "rive",
               "agricole_nrm",
               "anthropique_nrm",
               "HydCond_nrm",
               "avrSlope_nrm",
             "hepb_nrm",
             "VegRatio_o_nrm")

keep_nrm = c("id","id_uea",
             "rive",
             "agricole_nrm",
             "anthropique_nrm",
             "HydCond_nrm",
             "avrSlope_nrm",
             "hepb_nrm",
             "VegRatio_o_nrm",
             "AgriAnthro_nrm")

prod_nrm =prod_nrm[,(names(prod_nrm) %in% keep_nrm)]
names(prod_nrm)

prod_nrm_v = vect(prod_nrm)
cor_prod = Correlation_matrix(df = prod_nrm_v, var2 = 'productivite all')
pca_prod = PCA_graph_function(prod_nrm_v, df_name = 'Productivite all', axe = c(1,2))
pca_prod2 = PCA_graph_function(prod_nrm_v, df_name = 'Productivite all', axe = c(2,3))
pca_prod3 = PCA_graph_function(prod_nrm_v, df_name = 'Productivite all', axe = c(1,3))
pca_prod4 = PCA_graph_function(prod_nrm_v, df_name = 'Productivite all', axe = c(1,4))
#Check PCA on correlated metriques Forestier and average slope 
keep_nrm_forSlope = c("id","id_uea",
                      "rive","avrSlope_nrm", "forestier_nrm" )
prod_nrm_forestier_slope = prod_nrm_v[,(names(prod_nrm_v) %in% keep_nrm_forSlope)]
pca_forSlope = PCA_graph_function(prod_nrm_forestier_slope, df_name = 'Forestier vs Slope', axe = c(1,4))


###################
#Création d'indice préliminaire
ind1_prod = prod_nrm 
ind1_prod$inv_agricole = -ind1_prod$agricole_nrm +1
ind1_prod$inv_anthropique = -ind1_prod$anthropiqu_nrm +1

ind1_prod$Fe1 = (
  ind1_prod$agricole_nrm + ind1_prod$anthropiqu_nrm + ind1_prod$forestier_nrm +
    ind1_prod$humide_nrm + ind1_prod$HydCond_nrm + ind1_prod$hepb_nrm + ind1_prod$avrSlope_nrm
) / 7
 
ind1_prod$Fe2 = (
  ind1_prod$forestier_nrm +
    ind1_prod$humide_nrm + ind1_prod$HydCond_nrm + ind1_prod$hepb_nrm + ind1_prod$avrSlope_nrm
) / 5

ind1_prod$Fe3 = (ind1_prod$agricole_nrm+
                   ind1_prod$forestier_nrm+
                   ind1_prod$humide_nrm+
                   ind1_prod$avrSlope_nrm)/4

#######NEw indice de fonction ecologique avec inversion de l'agricole et l'urbain
ind1_prod$Fe4 = (ind1_prod$forestier_nrm+ind1_prod$humide_nrm +ind1_prod$hepb_nrm+
                   ind1_prod$inv_anthropique+ind1_prod$inv_agricole+ind1_prod$hydcond_nrm+
                   ind1_prod$avrslope_nrm)/7
plot(ind1_prod$Fe4)

ind1_prod$Fe5 = (ind1_prod$forestier_nrm +
                   ind1_prod$hydcond_nrm+
                   ind1_prod$hepb_nrm+
                     ind1_prod$avrslope_nrm)/4
        
plot(ind1_prod$Fe5)

ind1_prod$Fe6 = (ind1_prod$forestier_nrm +
                   ind1_prod$inv_agricole+
                   ind1_prod$humide_nrm+
                   ind1_prod$avrslope_nrm)/4
plot(ind1_prod$Fe6)

########################################################
#NEW indices avec Veg optimale et inversion d'anthropique, agricole et pente
ind1_prod = prod_nrm 
ind1_prod$inv_agricole = -ind1_prod$agricole_nrm +1
ind1_prod$inv_anthropique = -ind1_prod$anthropique_nrm +1
ind1_prod$inv_avgslope = -ind1_prod$avrSlope_nrm +1

ind1_prod$Fe7 = (ind1_prod$inv_agricole + ind1_prod$VegRatio_o_nrm+
                   ind1_prod$HydCond_nrm + ind1_prod$hepb_nrm +
                   ind1_prod$inv_anthropique + ind1_prod$inv_avgslope)/6

ind1_prod$Fe8 = (ind1_prod$inv_agricole +
                   ind1_prod$HydCond_nrm + ind1_prod$hepb_nrm +
                   ind1_prod$inv_anthropique + ind1_prod$inv_avgslope)/5

ind1_prod$Fe9 = (ind1_prod$inv_agricole +
                   ind1_prod$HydCond_nrm + 
                   ind1_prod$inv_anthropique + ind1_prod$inv_avgslope)/4
windows(10,5)
plot(ind1_prod$Fe9)
#NEW indices avec Veg optimale et inversion d'anthropique, agricole et pente and classe AgriAnthro
ind1_prod$inv_anthropique = -ind1_prod$anthropique_nrm +1
ind1_prod$inv_avgslope = -ind1_prod$avrSlope_nrm +1

ind1_prod$F10 = (ind1_prod$VegRatio_o_nrm + ind1_prod$inv_anthropique +
                   ind1_prod$hepb_nrm + ind1_prod$inv_avgslope)/4

windows(10,5)
plot(ind1_prod$F10)

#################NEW indices avec AgriAnthro 
ind1_prod = prod_nrm 
ind1_prod$inv_agricole = -ind1_prod$agricole_nrm +1
ind1_prod$inv_anthropique = -ind1_prod$anthropique_nrm +1
ind1_prod$inv_avgslope = -ind1_prod$avrSlope_nrm +1
ind1_prod$inv_agriAnthropique =  -ind1_prod$AgriAnthro_nrm +1

ind1_prod$FE11 = (ind1_prod$inv_agricole + ind1_prod$hepb_nrm +
                    ind1_prod$HydCond_nrm + ind1_prod$inv_avgslope +
                    ind1_prod$VegRatio_o_nrm)/5

windows(10,5)
plot(ind1_prod$FE11)

ind1_prod$FE12 = (ind1_prod$inv_anthropique+ ind1_prod$hepb_nrm +
                    ind1_prod$HydCond_nrm + ind1_prod$inv_avgslope +
                    ind1_prod$VegRatio_o_nrm)/5 

ind1_prod$FE13 = (ind1_prod$inv_agriAnthropique+ ind1_prod$hepb_nrm +
                    ind1_prod$HydCond_nrm + ind1_prod$inv_avgslope +
                    ind1_prod$VegRatio_o_nrm)/5



windows(10,5)
plot(ind1_prod$FE12)


min(ind1_prod$FE11)
max(ind1_prod$FE11)






#Some extra tests that no longer matter but we keep because the code may be usefull later
########################################################
#Tests la variabilite des metriques normalisee dans la fonction de Productivite des ER
prod_nrm = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.sqlite')
keep_nrm = c("id","id_uea",
             "rive",
             "agricole_nrm",
             "anthropiqu_nrm",
             "forestier_nrm"  ,
             "humide_nrm"    ,
             "hydcond_nrm",
             "avrslope_nrm",
             "hepb_nrm"
)

prod_nrm =prod_nrm[,(names(prod_nrm) %in% keep_nrm)]

tbl_stats = data.frame(nrow = 5,ncol = 7)
col_names =  c("agricole_nrm",
               "anthropiqu_nrm",
               "forestier_nrm"  ,
               "humide_nrm"    ,
               "hydcond_nrm",
               "avrslope_nrm",
               "hepb_nrm"
)
colnames(tbl_stats) <- col_names

tbl_stats<- setNames(data.frame(matrix(ncol = 7, nrow = 5)), col_names )
row_names = c('mean', 'standard deviation', 'median', 'min', 'max')
row.names(tbl_stats) <- row_names

col = 'agricole_nrm'
for (col in col_names){
  print(col)
  column1 = as.data.frame(st_drop_geometry(prod_nrm[,col]))[,1]
  tbl_stats[,col] = c(mean(column1), sd(column1), median(column1), min(column1), max(column1))
}

#transform rows into columns
tbl_stats_t = as.data.frame(t(tbl_stats))



library(ggplot2)

# create sample data
df = tbl_stats_t
#add col_names as a variable
df$row_name = col_names
# convert data from wide to long format
df_long <-
  tidyr::pivot_longer(
    df,
    cols = c( "mean", "standard deviation", "median", 'min', 'max'),
    names_to = "variable",
    values_to = "value"
  )

# create scatter plot
ggplot(df_long, aes(x = row_name, y = value, color = variable)) +
  geom_point(size = 3) +
  ylim(0, 1) +
  scale_color_manual(values = c("mean" = "red", "standard deviation" = "blue", "median"= "green", 'min' = 'black', 'max'= 'yellow')) +
  labs(color = "Variable")


####################################################################################
#New set of data with urb mask 
#Created the new set at top part 
#Normalization of metriques productivite urb
#UREC_prod_v = vect(UREC_prod)
UREC_prod_urb_nrm = Normalization_function(UREC_merge = UREC_prod_urb)
# st_write(UREC_prod_nrm, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.shp')
# st_write(UREC_prod_nrm, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.sqlite')
#prod_nrm = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.sqlite')
prod_urb_nrm = UREC_prod_urb_nrm
names(prod_urb_nrm)#prod_nrm

#Only keep columns we need to run stats
keep_prod_urb1 = c("id" ,"id_uea", "rive","agricole_nrm","anthropique_nrm",
                   "AgriAnthro_nrm" , "VegRatio_o_nrm",  "HydCond_nrm",
                   "avrSlope_nrm", "hepb_nrm")
prod_urb_nrm1 = prod_urb_nrm[,names(prod_urb_nrm) %in% keep_prod_urb1]
names(prod_urb_nrm1)
prod_urb_nrm1_v = vect(prod_urb_nrm1)
prod_urb_nrm1_cor= Correlation_matrix(df = prod_urb_nrm1_v, var2 = 'correlation metric urb')
########################
####################NEW SET OF DATA WITH URB MASK AND NEW SURFACE CLASSES

UREC_prod_urb2_nrm = Normalization_function(UREC_prod_urb2)
names(UREC_prod_urb2_nrm)
keep_urb2_nrm = c("id", "rive", "id_uea","Anthropique_nrm", "Forestier_nrm" ,"Agricole_nrm",
                  "Humide_nrm", "AgriAnthro_nrm",  "VegRatio_o_nrm",  "HydCond_nrm",
                  "avrSlope_nrm",  "hepb_nrm")
prod_urb2 = UREC_prod_urb2_nrm[,names(UREC_prod_urb2_nrm)%in% keep_urb2_nrm]
st_write(prod_urb2, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_urb_srf_nrm.sqlite')
prod_ubr2_v = vect(prod_urb2)
names(prod_urb2)

prod_urb2_cor = Correlation_matrix(df = prod_ubr2_v, var2 = 'Correlation urb/surf')
prod_urb2_pca = PCA_graph_function(df = prod_ubr2_v, df_name = 'Urb and surf', axe = c(1,2))

#Indices avec urb et nouvelle surface
#Make sure to inverse certain metrics
prod_ubr2_v$inv_AgriAnthro_nrm = -prod_ubr2_v$AgriAnthro_nrm +1
prod_ubr2_v$inv_Anthropique_nrm = -prod_ubr2_v$Anthropique_nrm +1
prod_ubr2_v$inv_avrSlope_nrm = -prod_ubr2_v$avrSlope_nrm +1

prod_ubr2_v$Fe1 = (prod_ubr2_v$inv_AgriAnthro_nrm + prod_ubr2_v$Humide +
                     prod_ubr2_v$inv_Anthropique_nrm + prod_ubr2_v$hepb_nrm +
                     prod_ubr2_v$inv_avrSlope_nrm)/5

windows(10,5)
plot(prod_ubr2_v$Fe1)


####Nouvelle statistiques sans forestier et humide
drop_urb2 = c("Forestier_nrm", 'Fe1',"Humide_nrm", "inv_AgriAnthro_nrm",'inv_Anthropique_nrm','inv_avrSlope_nrm' )
prod_ubr3_v = prod_ubr2_v[,!names(prod_ubr2_v)%in% drop_urb2]

prod_ubr3_v_cor = Correlation_matrix(df =prod_ubr3_v, var2 = 'correlation ' )
prod_ubr3_v_pca = PCA_graph_function(df = prod_ubr3_v, df_name = 'Urb and surf', axe = c(1,2))

#Indice de FE 
prod_ubr3_v$inv_AgriAnthro_nrm = -prod_ubr3_v$AgriAnthro_nrm +1
prod_ubr3_v$inv_avrSlope_nrm = - prod_ubr3_v$avrSlope_nrm +1
prod_ubr3_v$inv_Anthropique_nrm = - prod_ubr3_v$Anthropique_nrm +1

prod_ubr3_v$Fe1 = (prod_ubr3_v$inv_AgriAnthro_nrm +
                         prod_ubr3_v$inv_Anthropique_nrm +
                         prod_ubr3_v$inv_avrSlope_nrm +
                         prod_ubr3_v$hepb_nrm)/4
writeVector(prod_ubr3_v, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_urb_srf3_nrm.sqlite', filetype = 'SQLITE', overwrite = T)
windows(10,5)
plot(prod_ubr3_v$Fe1)
summary(prod_ubr3_v$Fe1)
c = d
################################################################

#Calcules des statistiques des metriques sans prendre en compte la zone eau
################################################################################
#Load new data 

#Surface relative 
test_surface_aqua = st_read( "C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_surfaces_UT_sans_eau.sqlite")
test_surface_aqua_n = test_surface_aqua 
drop_aqua_n = c("non.classifié",  "coupe.et.régénération", "sol.nu.et.lande" , "agrianthro" )
test_surface_aqua_n = test_surface_aqua_n[,!names(test_surface_aqua_n) %in% drop_aqua_n]
test_surface_aqua_n$vegetation = (test_surface_aqua_n$forestier + test_surface_aqua_n$agricole + test_surface_aqua_n$humide)
names(test_surface_aqua_n)

#pente 
UREC_pente = st_read('C:/Meghana/Belgique/decembre/traitements/UREC_slope.shp')
UREC_pente = st_drop_geometry(UREC_pente)

#Hand urb 
UREC_hand_urb = st_read('C:/Meghana/Belgique/decembre/traitements/Hauteur_emerge/UREC_hepb_mask_urb.shp')
UREC_hand_urb= st_drop_geometry(UREC_hand_urb)
UREC_hand_urb["hepb"][is.na(UREC_hand_urb["hepb"])] <- 0

#Siigsol urbain
UREC_sol_urb = st_read('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/results_urec_hydrocond_maskUrb.shp')
UREC_sol_urb = st_drop_geometry(UREC_sol_urb)
UREC_sol_urb["HydCond"][is.na(UREC_sol_urb["HydCond"])] <-0

UREC_prod_aqau = test_surface_aqua_n
UREC_prod_aqau = left_join(UREC_prod_aqau, UREC_pente, by = 'id', copy = FALSE)
UREC_prod_aqau = left_join(UREC_prod_aqau, UREC_sol_urb, by = 'id', copy = FALSE)
UREC_prod_aqau = left_join(UREC_prod_aqau, UREC_hand_urb, by = 'id', copy = FALSE)
UREC_prod_aqau$rive = UREC_prod_aqau$rive.x
UREC_prod_aqau$id_uea = UREC_prod_aqau$id_uea.x
drop_prod_aqau = c( "rive.x", "id_uea.x", "Id_UEA.x", "rive.y", "id_uea.y" ,"rive.x.x",
                    "Id_UEA.y" ,"rive.y.y" )
UREC_prod_aqau =UREC_prod_aqau[,!names(UREC_prod_aqau)%in% drop_prod_aqau]
names(UREC_prod_aqau)
UREC_prod_aqau[is.na(UREC_prod_aqau)] <- 0


#normalise data 
norm_UREC_prod_aqau = Normalization_function(UREC_merge = UREC_prod_aqau)
keep_prod_aqau_nrm = c('id', "rive" ,"id_uea","anthropique_nrm",
                       "forestier_nrm"  , "agricole_nrm"  ,  "humide_nrm",
                       "vegopt_nrm"  ,    "avrSlope_nrm"  ,  "HydCond_nrm",
                       "hepb_nrm",   "vegetation_nrm" )
norm_UREC_prod_aqau_1 = norm_UREC_prod_aqau[,names(norm_UREC_prod_aqau)%in% keep_prod_aqau_nrm ]
st_write(norm_UREC_prod_aqau_1, "C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_regulation_prod_nrm.sqlite", delete_layer = T )
norm_UREC_prod_aqau_1_v = vect(norm_UREC_prod_aqau_1) #dont forget to make this in spatvector to run correlation matrix
cor_UREC_prod_aqau =Correlation_matrix(df = norm_UREC_prod_aqau_1_v, var2 = 'correlation metrics without aqua')
#Faire une PCA avec toutes les métriques possible
pca_UREC_prod_aqau = PCA_graph_function(df =norm_UREC_prod_aqau_1_v, df_name = 'PCA metriques without aqua', axe = c(1,2) )

#Faire une PCA juste entre végétation opt, forestier et humide
keep_VegOpt1 = c('id', "rive" ,"id_uea", "forestier_nrm"  ,"humide_nrm",
                 "vegopt_nrm"  )
norm_UREC_prod_aqau_Vegopt = norm_UREC_prod_aqau_1_v[, names(norm_UREC_prod_aqau_1_v)%in%keep_VegOpt1]
pca_UREC_prod_aqau_Vegopt = PCA_graph_function(df =norm_UREC_prod_aqau_Vegopt, df_name = 'PCA metriques without aqua, vegopt, forest, humide', axe = c(1,2) )

#Faire une PCA avec juste végétation et anthropqieu 
keep_Vegetation_Anth = c('id', "rive" ,"id_uea","vegetation_nrm", "anthropique_nrm")
norm_UREC_prod_aqau_VegAnth =  norm_UREC_prod_aqau_1_v[, names(norm_UREC_prod_aqau_1_v)%in%keep_Vegetation_Anth]
pca_UREC_prod_aqau_VegAnth = PCA_graph_function(df =norm_UREC_prod_aqau_VegAnth, df_name = 'PCA metriques without aqua, Vegetation, anthro', axe = c(1,2) )

#Faire PCA avec toutes les metriques sauf forestier et humide, anthropique
drop_f_MH = c( "forestier_nrm"  ,"humide_nrm", "anthropique_nrm")
norm_UREC_prod_aqau_2 =norm_UREC_prod_aqau_1_v[,! names(norm_UREC_prod_aqau_1_v)%in%drop_f_MH] 
names(norm_UREC_prod_aqau_2)

pca_norm_UREC_prod_aqau_2 = PCA_graph_function(df = norm_UREC_prod_aqau_2, df_name = 'PCA metrics 2', axe = c(1,2))

#Faire PCA avec toutes les metriques sauf forestier, humide, anthro
drop_f_MH_Agr_Anth = c( "forestier_nrm"  ,"humide_nrm", "anthropique_nrm")
norm_UREC_prod_aqau_3 =norm_UREC_prod_aqau_1_v[,! names(norm_UREC_prod_aqau_1_v)%in%drop_f_MH_Agr_Anth] 
names(norm_UREC_prod_aqau_3)
pca_norm_UREC_prod_aqau_3 = PCA_graph_function(df = norm_UREC_prod_aqau_3, df_name = 'PCA metrics 3', axe = c(1,2))

####Make index with selected métriques 
norm_UREC_prod_aqau_3$inv_pente = -norm_UREC_prod_aqau_3$avrSlope_nrm +1
norm_UREC_prod_aqau_3$inv_HydCon = -norm_UREC_prod_aqau_3$HydCond_nrm +1
norm_UREC_prod_aqau_3$inv_anthro = - norm_UREC_prod_aqau$anthropique +1
norm_UREC_prod_aqau_3$Fe1 = (norm_UREC_prod_aqau_3$vegopt_nrm + norm_UREC_prod_aqau_3$vegetation_nrm +
                               norm_UREC_prod_aqau_3$inv_pente + norm_UREC_prod_aqau_3$hepb_nrm + 
                               norm_UREC_prod_aqau_3$inv_HydCon)/5


writeVector(norm_UREC_prod_aqau_3,'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_prod_aqua_f.shp', overwrite = T )
writeVector(norm_UREC_prod_aqau_3,'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_prod_aqua_f.sqlite', filetype = 'SQLITE' ,overwrite=T)



windows(10,5)
plot(norm_UREC_prod_aqau_3$Fe1 )
l=
  
  
  








































####################################################################################

#Test pente and other variables with non lineare UEA 
#load data
pts_ref00 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro00.gdb',
                    layer = 'PtRef', stringsAsFactors = F)

pt_ref00_bv_morpho = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro00.gdb',
                             layer = 'PtRef_bv_morpho', stringsAsFactors = F)
UEA_00_Desc_lotique_n2 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro00.gdb',
                                 layer = 'UEA_Desc_Lotique_N2', stringsAsFactors = F)

UEA_L_N2 =  st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro00.gdb',
                    layer = 'UEA_L_N2', stringsAsFactors = F)
#######
#Do this for CRHQ 01
pts_ref01 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro01.gdb',
                    layer = 'PtRef', stringsAsFactors = F)

pt_ref01_bv_morpho = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro01.gdb',
                             layer = 'PtRef_bv_morpho', stringsAsFactors = F)
UEA_01_Desc_lotique_n2 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro01.gdb',
                                 layer = 'UEA_Desc_Lotique_N2', stringsAsFactors = F)

UEA_01_L_N2 =  st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro01.gdb',
                    layer = 'UEA_L_N2', stringsAsFactors = F)
###02
pts_ref02 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro02.gdb',
                    layer = 'PtRef', stringsAsFactors = F)

pt_ref02_bv_morpho = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro02.gdb',
                             layer = 'PtRef_bv_morpho', stringsAsFactors = F)
UEA_02_Desc_lotique_n2 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro02.gdb',
                                 layer = 'UEA_Desc_Lotique_N2', stringsAsFactors = F)

UEA_02_L_N2 =  st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro02.gdb',
                       layer = 'UEA_L_N2', stringsAsFactors = F)
##03 
pts_ref03 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb',
                    layer = 'PtRef', stringsAsFactors = F)

pt_ref03_bv_morpho = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb',
                             layer = 'PtRef_bv_morpho', stringsAsFactors = F)
UEA_03_Desc_lotique_n2 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb',
                                 layer = 'UEA_Desc_Lotique_N2', stringsAsFactors = F)

UEA_03_L_N2 =  st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb',
                       layer = 'UEA_L_N2', stringsAsFactors = F)
##04
pts_ref04 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro04.gdb',
                    layer = 'PtRef', stringsAsFactors = F)

pt_ref04_bv_morpho = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro04.gdb',
                             layer = 'PtRef_bv_morpho', stringsAsFactors = F)
UEA_04_Desc_lotique_n2 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro04.gdb',
                                 layer = 'UEA_Desc_Lotique_N2', stringsAsFactors = F)

UEA_04_L_N2 =  st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro04.gdb',
                       layer = 'UEA_L_N2', stringsAsFactors = F)
##05
pts_ref05 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb',
                    layer = 'PtRef', stringsAsFactors = F)

pt_ref05_bv_morpho = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb',
                             layer = 'PtRef_bv_morpho', stringsAsFactors = F)
UEA_05_Desc_lotique_n2 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb',
                                 layer = 'UEA_Desc_Lotique_N2', stringsAsFactors = F)

UEA_05_L_N2 =  st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb',
                       layer = 'UEA_L_N2', stringsAsFactors = F)
##06
pts_ref06 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro06.gdb',
                    layer = 'PtRef', stringsAsFactors = F)

pt_ref06_bv_morpho = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro06.gdb',
                             layer = 'PtRef_bv_morpho', stringsAsFactors = F)
UEA_06_Desc_lotique_n2 = st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro06.gdb',
                                 layer = 'UEA_Desc_Lotique_N2', stringsAsFactors = F)

UEA_06_L_N2 =  st_read('C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro06.gdb',
                       layer = 'UEA_L_N2', stringsAsFactors = F)
##05

#Create a function 
Slope_stats <- function(pt_ref, pt_ref_bv_morpho, UEA_desc_lotique_N2, style_fluvial){
  #create lineare query to filter out lineaire and n.d. style fluviale
  query_lin = UEA_desc_lotique_N2$Style_fluv == style_fluvial
  desc_lotique_meandre = dplyr::filter(UEA_desc_lotique_N2, query_lin) #filter out style fluviale lin/aire
  #query_nd = desc_lotique_meandre$Style_fluv != 'n.d.' #create new query to filter out n.d.
  #desc_lotique_meandre = dplyr::filter(desc_lotique_meandre, query_nd) #filter out style fluvial n.d.
  
  #Join pt_ref and table with slope data 
  pts_ref_pentes = left_join(pt_ref, pt_ref_bv_morpho, by = 'Id_PtRef')
  keep_slope = c('Id_PtRef', 'Id_UEA', 'S_moy') #make liste of column to keep
  pts_ref_pentes = pts_ref_pentes[,(names(pts_ref_pentes) %in% keep_slope)] #Subset data based on keep columns
  
  #Get rid of values = -999 in order to later do average of slope
  query_2 = pts_ref_pentes$S_moy != -999
  pts_ref_pentes_1 = dplyr::filter(pts_ref_pentes, query_2)
  #Group this new table by Id_UEA
  pts_ref_pentes_grp = pts_ref_pentes_1 %>% group_by(by = Id_UEA) 
  join_meandre_pentes = left_join(desc_lotique_meandre, pts_ref_pentes_grp, by = 'Id_UEA') %>% group_by(by = Id_UEA)%>% summarise(avr_slope = mean(S_moy))
  
  }
#Caculate average slope 

avr_pente_00 = Slope_stats(pt_ref = pts_ref00, pt_ref_bv_morpho = pt_ref00_bv_morpho, UEA_desc_lotique_N2 = UEA_00_Desc_lotique_n2, style_fluvial = 'Méandre')

avr_pente_01 = Slope_stats(pt_ref = pts_ref01, pt_ref_bv_morpho = pt_ref01_bv_morpho, UEA_desc_lotique_N2 = UEA_01_Desc_lotique_n2, style_fluvial = 'Méandre')

avr_pente_02 = Slope_stats(pt_ref = pts_ref02, pt_ref_bv_morpho = pt_ref02_bv_morpho, UEA_desc_lotique_N2 = UEA_02_Desc_lotique_n2, style_fluvial = 'Méandre')

avr_pente_03 = Slope_stats(pt_ref = pts_ref03, pt_ref_bv_morpho = pt_ref03_bv_morpho, UEA_desc_lotique_N2 = UEA_03_Desc_lotique_n2, style_fluvial = 'Méandre')

avr_pente_04 = Slope_stats(pt_ref = pts_ref04, pt_ref_bv_morpho = pt_ref04_bv_morpho, UEA_desc_lotique_N2 = UEA_04_Desc_lotique_n2, style_fluvial = 'Méandre')

avr_pente_05 = Slope_stats(pt_ref = pts_ref05, pt_ref_bv_morpho = pt_ref05_bv_morpho, UEA_desc_lotique_N2 = UEA_05_Desc_lotique_n2, style_fluvial = 'Méandre')

avr_pente_06 = Slope_stats(pt_ref = pts_ref06, pt_ref_bv_morpho = pt_ref06_bv_morpho, UEA_desc_lotique_N2 = UEA_06_Desc_lotique_n2, style_fluvial = 'Méandre')

plot(avr_pente_00$avr_slope, col = 'blue')
plot(avr_pente_02$avr_slope, col = 'red')


slope_meandres = rbind(avr_pente_00, avr_pente_02)
slope_meandres = rbind(slope_meandres, avr_pente_03)
slope_meandres = rbind(slope_meandres, avr_pente_04)
slope_meandres = rbind(slope_meandres, avr_pente_05)
slope_meandres = rbind(slope_meandres, avr_pente_06)
slope_meandres = na.omit(slope_meandres)
plot(slope_meandres$avr_slope)
slope_meandres$nrm_slope = (slope_meandres$avr_slope - min(slope_meandres$avr_slope)) / (max(slope_meandres$avr_slope) - min(slope_meandres$avr_slope))
plot(slope_meandres$nrm_slope)
##Do the same but only for lineaire 
lin_avr_pente_00 = Slope_stats(pt_ref = pts_ref00, pt_ref_bv_morpho = pt_ref00_bv_morpho, UEA_desc_lotique_N2 = UEA_00_Desc_lotique_n2, style_fluvial = 'Linéaire')

lin_avr_pente_01 = Slope_stats(pt_ref = pts_ref01, pt_ref_bv_morpho = pt_ref01_bv_morpho, UEA_desc_lotique_N2 = UEA_01_Desc_lotique_n2, style_fluvial = 'Linéaire')

lin_avr_pente_02 = Slope_stats(pt_ref = pts_ref02, pt_ref_bv_morpho = pt_ref02_bv_morpho, UEA_desc_lotique_N2 = UEA_02_Desc_lotique_n2, style_fluvial = 'Linéaire')

lin_avr_pente_03 = Slope_stats(pt_ref = pts_ref03, pt_ref_bv_morpho = pt_ref03_bv_morpho, UEA_desc_lotique_N2 = UEA_03_Desc_lotique_n2, style_fluvial = 'Linéaire')

lin_avr_pente_04 = Slope_stats(pt_ref = pts_ref04, pt_ref_bv_morpho = pt_ref04_bv_morpho, UEA_desc_lotique_N2 = UEA_04_Desc_lotique_n2, style_fluvial = 'Linéaire')

lin_avr_pente_05 = Slope_stats(pt_ref = pts_ref05, pt_ref_bv_morpho = pt_ref05_bv_morpho, UEA_desc_lotique_N2 = UEA_05_Desc_lotique_n2, style_fluvial = 'Linéaire')

lin_avr_pente_06 = Slope_stats(pt_ref = pts_ref06, pt_ref_bv_morpho = pt_ref06_bv_morpho, UEA_desc_lotique_N2 = UEA_06_Desc_lotique_n2, style_fluvial = 'Linéaire')

slope_lin = rbind(lin_avr_pente_00, lin_avr_pente_01)
slope_lin = rbind(slope_lin, lin_avr_pente_02)
slope_lin = rbind(slope_lin, lin_avr_pente_03)
slope_lin = rbind(slope_lin, lin_avr_pente_04)
slope_lin = rbind(slope_lin, lin_avr_pente_05)
slope_lin = rbind(slope_lin, lin_avr_pente_06)
slope_lin = na.omit(slope_lin)
slope_lin$nrm_slope = (slope_lin$avr_slope - min(slope_lin$avr_slope)) / (max(slope_lin$avr_slope) - min(slope_lin$avr_slope))
plot(slope_lin$avr_slope)


