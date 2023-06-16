#Connectivite du paysage _water update

#load UREC_rive sampling data and UREC_rive_water data 
path_UREC_sampling = 'C:/Meghana/Belgique/decembre/data/sampling/' #path towards folder with individual UREC sampling shapefiles (made previously)
list_file_sampling = list.files(path = path_UREC_sampling, full.names = T, pattern = ".shp")

UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water.shp')
i=5
# for (i in 1:length(list_file_sampling)){
#   sampling = st_read(list_file_sampling[i])
#   sampling_id = sampling[1,]$id
#   #Create a query to find the appropraite UEA 
#   query = UREC_merge$id == sampling_id
#   rive = filter(UREC_merge, query) #Find the correct UREC based on id
#   sampling_clip = st_intersection(sampling, rive)
#   st_write(sampling_clip, paste0('C:/Meghana/Belgique/decembre/data/sampling/sampling_without_water/', sampling_id, '.shp'))
# 
# }

####Calculate metrics
path_UREC_sampling = 'C:/Meghana/Belgique/decembre/data/sampling/sampling_without_water/'
ut19_VegOpt = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_VegOptimale.tif')

# #Continuity vegopt metric
# UREC_contVegOpt = continuity_metric(UREC_full = UREC_merge, path_sampling = path_UREC_sampling,  raster_UT = ut19_VegOpt, classe_UT = 'VegetationOpt' )
# st_write(UREC_contVegOpt, 'C:/Meghana/Belgique/decembre/traitements/ContP_eau/connectivite_VegOpt.shp')
# st_write(UREC_contVegOpt, 'C:/Meghana/Belgique/decembre/traitements/ContP_eau/connectivite_VegOpt.sqlite')
# 
# #patch density index Veg optimal
#FRAGSTAT STATS
# occ_sol = 'VegOpt'
# raster_file = ut19_VegOpt
# 
# i=1
# 
# UREC_fragstat = fragstat_function(raster_file = raster_file, UREC_merge = UREC_merge, occ_sol =  'VegOpt')
# st_write(UREC_fragstat,'C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_fragstat.shp' )
# st_write(UREC_fragstat,'C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_fragstat.sqlite' )
# 
# # #indice de naturalite 

#load csv of land use classes with weights 
ut19_classes <- read.csv("C:/Meghana/donnee_brutes/correspondance_indiceNat_ut19_10mV2.csv", sep=";")
ut19_classes$CODE_UT = as.numeric(ut19_classes$CODE_UT)
ut19_groups = ut19_classes %>% group_by(DESC_CAT)
#load raster file of landuse clipped to UREC
raster_file = rast('C:/Meghana/Belgique/decembre/data/raster/ut19_mask.tif')
#Load UREC_merge 
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water.shp')
UREC_merge = UREC_merge[1:2,]

UREC_IndNat = Indice_Naturalite_functions(UREC_merge = UREC_merge, raster_file = raster_file, csv_class_correspondence = ut19_groups)
st_write(UREC_IndNat, 'C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_indiceNat.shp')
st_write(UREC_IndNat, 'C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_indiceNat.sqlite')

#######DO statistiques and create indexe 

#Put all the data together 
#1. indice de naturalite 
UREC_IndNat = st_read('C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_indiceNat.sqlite')

#2. Continuite de la veg Opt 
UREC_contVegOpt =st_read('C:/Meghana/Belgique/decembre/traitements/ContP_eau/connectivite_VegOpt.sqlite')
UREC_contVegOpt = st_drop_geometry(UREC_contVegOpt)
#3. Indice de fragmentation 
UREC_fragstat =st_read('C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_fragstat.sqlite')
UREC_fragstat = st_drop_geometry(UREC_fragstat)
#4. Elements de discontinuite 
UREC_surface = st_read("C:/Meghana/Belgique/decembre/traitements/fonction_productivite/UREC_surfaces_UT_sans_eau.sqlite")
keep = c('id', 'anthropique', 'vegopt')
UREC_surface = UREC_surface[, names(UREC_surface)%in% keep]
names(UREC_surface)
UREC_surface = st_drop_geometry(UREC_surface)

#Create a new shapefile with all of these metrics joined together
UREC_ConnP = UREC_IndNat
UREC_ConnP = left_join(UREC_ConnP,UREC_contVegOpt, by = c('id') )
UREC_ConnP = left_join(UREC_ConnP,UREC_fragstat, by = c('id') )
UREC_ConnP = left_join(UREC_ConnP,UREC_surface, by = c('id') )

drop = c("id_uea.x" , "rive.x", "id_uea.y","rive.y" )
UREC_ConnP = UREC_ConnP[,!names(UREC_ConnP)%in% drop]
UREC_ConnP[is.na(UREC_ConnP)] <- 0
st_write(UREC_ConnP, 'C:/Meghana/Belgique/decembre/traitements/ContP_eau/metriques_ConnP.shp')
st_write(UREC_ConnP, 'C:/Meghana/Belgique/decembre/traitements/ContP_eau/metriques_ConnP.sqlite')


#Do normalisation of data 
UREC_ConnP_nrm = Normalization_function(UREC_merge = UREC_ConnP)
st_write(UREC_ConnP_nrm, 'C:/Meghana/Belgique/decembre/traitements/ContP_eau/metriques_ConnP_nrm.shp')
st_write(UREC_ConnP_nrm, 'C:/Meghana/Belgique/decembre/traitements/ContP_eau/metriques_ConnP_nrm.sqlite')

drop_nrm = c( "indice_nat","continuity_vegetationopt", 'pd_vegopt',
              "area_vegopt", "anthropique", "vegopt")

UREC_ConnP_nrm1 = UREC_ConnP_nrm[,!names(UREC_ConnP_nrm)%in% drop_nrm]
names(UREC_ConnP_nrm1)

#transform from sf to Vect object to do statistics 
UREC_ConnP_nrm1_v = vect(UREC_ConnP_nrm1)

cor_UREC_ConnP_nrm1_v = Correlation_matrix(UREC_ConnP_nrm1_v, var2 = 'metrics in Connectivity paysage')

#Do PCA on all metrics 
pca_UREC_ConnP_nrm1_v = PCA_graph_function(df =UREC_ConnP_nrm1_v, df_name = 'ConnP metrics', axe = c(1,2) )

#Do pca on subset of data linked to VegOpt
drop_vegOp = c('pd_vegopt_nrm', "area_vegopt_nrm",'anthropique_nrm' )
UREC_VegOpt = UREC_ConnP_nrm1_v[,!names(UREC_ConnP_nrm1_v)%in%drop_vegOp]

pca_UREC_VegOpt = PCA_graph_function(df = UREC_VegOpt, df_name = 'VegOpt', axe = c(1,2))

#Do pca on finally sected metrics
keep_ContP = c("id","id_uea","rive","indice_nat_nrm",
               "pd_vegopt_nrm" ,"anthropique_nrm")
UREC_ConnP_nrm2_v = UREC_ConnP_nrm1_v[,names(UREC_ConnP_nrm1_v)%in%keep_ContP]


pca_UREC_ConnP_nrm2_v = PCA_graph_function(df = UREC_ConnP_nrm2_v, df_name = 'final selection ContP', axe = c(1,2))

#Creat index based on results of PCA
UREC_ConnP_nrm2_v$inv_anthropique = -UREC_ConnP_nrm2_v$anthropique_nrm +1
UREC_ConnP_nrm2_v$inv_pd_vegopt = -UREC_ConnP_nrm2_v$pd_vegopt_nrm +1
UREC_ConnP_nrm2_v$FE = (UREC_ConnP_nrm2_v$inv_anthropique + 
                          UREC_ConnP_nrm2_v$inv_pd_vegopt+
                          UREC_ConnP_nrm2_v$indice_nat_nrm)/3
writeVector(UREC_ConnP_nrm2_v,'C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_ContP_final.shp' )
writeVector(UREC_ConnP_nrm2_v,'C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_ContP_final.sqlite', filetype = 'SQLITE' )

windows(10,5)
plot(UREC_ConnP_nrm2_v$FE)

summary(UREC_ConnP_nrm2_v$FE)
summary(UREC_ConnP_nrm2_v$FE2)


####Do some check on metric liee a connectivity du paysage with fragstat 

pd1 = st_read('C:/Meghana/Belgique/decembre/traitements/ContP_eau/UREC_ContP_final.sqlite')
