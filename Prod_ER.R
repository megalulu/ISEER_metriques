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
#Pente
UREC_pente = st_read('C:/Meghana/Belgique/decembre/traitements/UREC_slope.shp')
UREC_pente = st_drop_geometry(UREC_pente)
#Hand 
UREC_hand = st_read('C:/Meghana/Belgique/decembre/traitements/Hauteur_emerge/UREC_hepb.shp')
UREC_hand = st_drop_geometry(UREC_hand)

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
















################################################################
#Normalization of metriques productivite
#UREC_prod_v = vect(UREC_prod)
UREC_prod_nrm = Normalization_function(UREC_merge = UREC_prod)
# st_write(UREC_prod_nrm, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.shp')
# st_write(UREC_prod_nrm, 'C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.sqlite')
#prod_nrm = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_metriques_prod_nrm.sqlite')
prod_nrm = UREC_prod_nrm
names(prod_nrm)#prod_nrm = UREC_prod_nrm
# keep_nrm = c("id","id_uea",
#   "rive",
#   "agricole_nrm",
#   "anthropiqu_nrm",
#   "forestier_nrm"  ,
#   "humide_nrm"    ,
#   "hydcond_nrm",
#   "avrslope_nrm",
#   "hepb_nrm"
# )
keep_nrm = c("id","id_uea",
               "rive",
               "agricole_nrm",
               "anthropique_nrm",
               "HydCond_nrm",
               "avrSlope_nrm",
             "hepb_nrm",
             "VegRatio_o_nrm")

# keep_nrm = c("id","id_uea",
#              "rive",
#              "anthropique_nrm",
#              "HydCond_nrm",
#              "avrSlope_nrm",
#              "hepb_nrm",
#              "VegRatio_o_nrm", 
#              "AgriAnthro_nrm")

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

#NEW indices avec Veg optimale et inversion d'anthropique, agricole et pente and classe AgriAnthro
ind1_prod$inv_anthropique = -ind1_prod$anthropique_nrm +1
ind1_prod$inv_avgslope = -ind1_prod$avrSlope_nrm +1

ind1_prod$F10 = (ind1_prod$VegRatio_o_nrm + ind1_prod$inv_anthropique +
                   ind1_prod$hepb_nrm + ind1_prod$inv_avgslope)/4
plot(ind1_prod$F10)


windows(10,5)
plot(ind1_prod$Fe10)



min(ind1_prod$F10)
max(ind1_prod$F10)






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


