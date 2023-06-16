#Surface class tests
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
raster_file = rast('C:/Meghana/Belgique/decembre/data/raster/ut19_mask.tif')
csv__class_correspondence = read.csv2("C:/Meghana/donnee_brutes/UT_2019_10m.csv")
#csv__class_correspondence %>% group_by(CODE_UT)
#UREC_merge = UREC_merge[1:5,]

test_surface = SurfaceClass(UREC_merge = UREC_merge, csv_class_correspondence = csv__class_correspondence,
                            raster_file = raster_file)

test_surface_v = vect(test_surface)
test_surface_v$id_uea = test_surface_v$Id_UEA
test_surface_v$VegOpt = test_surface_v$Forestier + test_surface_v$Humide
test_surface_v$AgriAnthro = test_surface_v$Anthropique + test_surface_v$Agricole

drop = c('Id_UEA')
test_surface_v = test_surface_v[,!names(test_surface_v)%in%drop]
test_surface_v
test_surface_v_nrm = Normalization_function(UREC_merge = test_surface_v)
keep = c("rive","id", "id_uea","Anthropique_nrm","Forestier_nrm","Agricole_nrm",
         "Humide_nrm", 'AgriAnthro', 'VegOpt')
test_surface_v_nrm_keep = test_surface_v_nrm[, names(test_surface_v_nrm)%in% keep]

test_surface_v_nrm_keep$Anthropique_nrm = as.numeric(test_surface_v_nrm_keep$Anthropique_nrm)
test_surface_v_nrm_keep$Forestier_nrm= as.numeric(test_surface_v_nrm_keep$Forestier_nrm)
test_surface_v_nrm_keep$Agricole_nrm = as.numeric(test_surface_v_nrm_keep$Agricole_nrm)
test_surface_v_nrm_keep$Humide_nrm = as.numeric(test_surface_v_nrm_keep$Humide_nrm)
test_surface_v_nrm_keep = vect(test_surface_v_nrm_keep)

cor_test_surface = Correlation_matrix(df = test_surface_v_nrm_keep, var2 = 'surface matrice')  

#####Do correlation with original surface data 
UREC_merge_surface= st_read('C:/Meghana/Belgique/decembre/traitements/SurfaceClass.sqlite')
UREC_merge_surface[is.na(UREC_merge_surface)] <- 0 #replace all values that are NA with 0
UREC_merge_surface$agricole = as.numeric(UREC_merge_surface$agricole)
UREC_merge_surface$anthropique = as.numeric(UREC_merge_surface$anthropique)
UREC_merge_surface$forestier = as.numeric(UREC_merge_surface$forestier)
UREC_merge_surface$aquatique = as.numeric(UREC_merge_surface$aquatique)
UREC_merge_surface$humide = as.numeric(UREC_merge_surface$humide)
UREC_merge_surface$non.classifié = as.numeric(UREC_merge_surface$non.classifié)
UREC_merge_surface$coupe.et.régénération = as.numeric(UREC_merge_surface$coupe.et.régénération)
UREC_merge_surface$sol.nu.et.lande = as.numeric(UREC_merge_surface$sol.nu.et.lande)
UREC_merge_surface$VegOpt = UREC_merge_surface$forestier +UREC_merge_surface$humide
UREC_merge_surface$AgriAnthro = UREC_merge_surface$agricole + UREC_merge_surface$anthropique
drop_surface = c('vegratio', 'nbr_class' )
UREC_merge_surface = UREC_merge_surface[, !names(UREC_merge_surface)%in% drop_surface]
UREC_merge_surface_nrm = Normalization_function(UREC_merge = UREC_merge_surface)


UREC_merge_surface_v = vect(UREC_merge_surface)
cor_UREC_merge_surface_v_ = Correlation_matrix(df = UREC_merge_surface_v, var2 = 'All landuse ratios total')


####Some statistiques on landuses in the UREC 
UREC_surface_stats = UREC_merge_surface
drop_stats = c('vegratio', 'nbr_class','AgriAnthro', 'VegOpt', 'id','id_uea','rive')
UREC_surface_stats = UREC_surface_stats[, !names(UREC_surface_stats) %in% drop_stats]
names(UREC_surface_stats)
summary(UREC_surface_stats)

tbl_stats = data.frame(nrow = 5,ncol = 8)
col_names =  c("agricole",
               "anthropique",
               "aquatique",
               "forestier"  ,
               "humide"    ,
               "non.classifié" ,
               "coupe.et.régénération",
               "sol.nu.et.lande")

tbl_stats<- setNames(data.frame(matrix(ncol = 8, nrow = 5)), col_names )
row_names = c('mean', 'standard deviation', 'median', 'min', 'max')
row.names(tbl_stats) <- row_names

               
#col = 'agricole'
for (col in colnames(UREC_surface_stats)){
  print(col)
  column1 = as.data.frame(st_drop_geometry(UREC_surface_stats[,col]))[,1]
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
  labs(color = "Variable")+
  theme(text = element_text(size=20),
  axis.text.x = element_text(angle=45, hjust=1))
#####################################################

#Test des correlation entre les surface sans prendre en compte l'aquatique 
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water.shp')
raster_file = rast('C:/Meghana/Belgique/decembre/data/raster/ut19_mask.tif')
csv__class_correspondence = read.csv2("C:/Meghana/donnee_brutes/UT_2019_10m.csv")
#csv__class_correspondence %>% group_by(CODE_UT)
#UREC_merge = UREC_merge[1:5,]

test_surface_aqua = SurfaceClass(UREC_merge = UREC_merge, csv_class_correspondence = csv__class_correspondence,
                            raster_file = raster_file)

test_surface_aqua$VegOpt = test_surface_aqua$Forestier + test_surface_aqua$Humide
test_surface_aqua$vegetation = test_surface_aqua$Forestier + test_surface_aqua$Humide +test_surface_aqua$Agricole

test_surface_aqua$AgriAnthro = test_surface_aqua$Anthropique + test_surface_aqua$Agricole
test_surface_aqua[is.na(test_surface_aqua)] <- 0
test_surface_aqua$id_uea = test_surface_aqua$Id_UEA
test_surface_aqua= test_surface_aqua[,-1]#remove column that is Id_UEA
st_write(test_surface_aqua, "C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_surfaces_UT_sans_eau.sqlite", delete_layer = T)
test_surface_aqua_v = vect(test_surface_aqua)
cor_test_surface_aqua = Correlation_matrix(df =test_surface_aqua_v, var2 = 'test surface aqua' )