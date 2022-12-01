####SCRIPTS DE TRAVAIL MELCC####

source('metric_functions.R')
source('Normalization_functions.R')
source('Statistics_new.R')

setwd = 'C:/Meghana/TUTO/'

#Load data : 
path_UREC_merge = 'C:/Meghana/TUTO/Donnees/UREC.shp'
UREC_merge = st_read(path_UREC_merge)

path_UREC_sampling = 'C:/Meghana/TUTO/Donnees/UREC_sampling/'

path_axe = 'C:/Meghana/TUTO/Traitements/axes.shp'
axe = st_read(path_axe)

path_axe_folder = 'C:/Meghana/TUTO/Traitements/axes/'


i =1
#Create empty list in case some UREC cannot be subdivided
missing_sampling <- list()

#Create sampling units for UREC 
for (i in 1:nrow(UREC_merge)){
  rive = UREC_merge[i,]
  print(i)
  name = paste0(path_axe_folder, rive$id, '.shp')
  axe_intersection = st_intersection(axe, rive)
  axe_intersection = st_as_sfc(axe_intersection)
  axe_intersection = st_cast(axe_intersection, to = 'LINESTRING')
  semis = st_line_sample(axe_intersection, density = 0.02, type = 'regular')
  rive_sfc = st_as_sfc(rive)
  voronoi = st_voronoi(semis)
  #voronoi = do.call(st_voronoi(), st_geometry(semis))
  voronoi = st_collection_extract(st_voronoi(do.call("c", st_geometry(semis)))) #this is some weird thing off the internet. Only works if c is in quotation marks. 
  x = class(voronoi)
  if (x[1] == "sfc_POLYGON"){
    #voronoi = st_as_sfc(voronoi)
    voronoi_crs = st_set_crs(voronoi,crs(rive))
    voronoi_crs = st_as_sf(voronoi_crs)
    rive_sampling = st_intersection(rive,voronoi_crs)
    print(paste0('writing rive sampling at poistion ', i, ' with Id : ', rive_sampling[1,]$id))
    st_write(rive_sampling,paste0(path_UREC_sampling, rive$id, '.shp'), delete_layer = T)
  }else {
    missing_sampling <- append(missing_sampling, i)
    
  }
  
}

##########################################################
#Indice de connectivite du paysage
###############

#Load Data related to 

#Set variables files and paths linked to raster files (land use/ Utilisation du territoire)
ut19_full= raster('C:/Meghana/TUTO/Donnees/Utilisation_territoire/UT2019_clip1.tif') #Utilisation du territoire
ut19_VegOpt = raster('C:/Meghana/TUTO/Donnees/Utilisation_territoire/UT2019_clip1_VegOpt.tif') #Mask vegetation optimale 
ut19_Urban = raster('C:/Meghana/TUTO/Donnees/Utilisation_territoire/UT2019_clip1_Urbain.tif') #Mask Urbain
ut19_classes <- read.csv("C:/Meghana/TUTO/Donnees/Utilisation_territoire/correspondance_indiceNat_ut19_10m.csv", sep=";") #Table de correspondance entre les classes d'utilisation du territoire et l'indice de naturalie

#Rouler les scripts pour calculer les metriques liees a l'indice de connectivite du paysage
UREC_merge = continuity_metric(UREC_full= UREC_merge,path_sampling = path_UREC_sampling, raster_UT = ut19_VegOpt, classe_UT = 'vegetation_optimale') #metrique de continuite de la vegettation optimale
UREC_merge = PDV_fragementation_metric_function(UREC_full = UREC_merge, raster_file = ut19_VegOpt, col_name = 'vegetation_optimale') #metrique de fragemntation de la vegettation optimale
UREC_merge = Indice_Naturalite_functions(UREC_merge = UREC_merge, raster_file = ut19_full, csv_class_correspondence = ut19_classes) #metrique d'indice de naturalite 
#Ecriture du fichier UREC_merge metrique 
st_write(UREC_merge, 'C:/Meghana/TUTO/Resultats/resultats_merique_IndiceConnectivitePaysage.sqlite')

#Normalization et inversion
UREC_merge_norm = st_read('C:/Meghana/TUTO/Resultats/resultats_merique_IndiceConnectivitePaysage.sqlite') #Read the file with the calculated metrics
UREC_merge_norm = subset(UREC_merge_norm, select = -c(area_vegetation_optimale))
UREC_merge_norm = Normalization_function(UREC_merge = UREC_merge_norm) #Normalisation 
UREC_merge_norm = New_inverse_fun(UREC_norm = UREC_merge_norm, col_names = c('pd_vegetation_optimale_nrm')) #Inversion
st_write(UREC_merge_norm, 'C:/Meghana/TUTO/Resultats/resultats_metrique_Norm_IndiceConnectivitePaysage.sqlite',delete_layer = T)
#Calcul de l'indice de fonction ecologique
UREC_indice_ConP = st_read('C:/Meghana/TUTO/Resultats/resultats_metrique_Norm_IndiceConnectivitePaysage.sqlite')
UREC_indice_ConP$ConP = (UREC_indice_ConP$continuity_vegetation_optimale_nrm +
                           UREC_indice_ConP$pd_vegetation_optimale_nrm+
                           UREC_indice_ConP$pd_vegetation_optimale_nrm)/3
st_write(UREC_indice_ConP, 'C:/Meghana/TUTO/Resultats/resultats_indice_Norm_IndiceConnectivitePaysage.sqlite')


##########################################################
#Indice de regulation de la temperature de l'eau par l'ombrage
###############
#Pretraitements :creation des polygones de riviere
###############
UEA = st_read('C:/Meghana/TUTO/Donnees/UEA.shp') #charger les UEA
pt_ref_clip = st_read('C:/Meghana/TUTO/Donnees/ptsRef.shp') #charger le shapefile points de reference
PtRef_mod_lotique05 = st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb", 
                              layer = "PtRef_mod_lotique",
                              stringsAsFactors = F) #charger les donnees liees aux points de reference du CRHQ
PtRef05_mod_lotique_join = left_join(pt_ref_clip, PtRef_mod_lotique05, by = c('Id_PtRef' = 'Id_PtRef')) #joindre les deux tables selon leur identifiant commun (Id_PtRef)
PtRef05_mod_lotique_join = subset(PtRef05_mod_lotique_join, select = c(Id_PtRef, Id_UEA,
                                                                   Largeur_mod)) #Selection les colonnes qui nous interressent (Id_PtRef, Id_UEA, Largeur_mod)

PtRef_mod_lotique_join = subset(PtRef_mod_lotique_join, Largeur_mod != -999) #Enlever les lignes sans valeurs donnees pour la colonne largeur_mod (largeur modelisee de la riviere)
PtRef_mod_lotique_join = subset(PtRef_mod_lotique_join, !is.na(Largeur_mod))#Enlever les lignes sans valeurs donnees pour la colonne largeur_mod (largeur modelisee de la riviere)

largeur_riviere= PtRef_mod_lotique_join %>% dplyr::group_by(Id_UEA)%>% dplyr::summarize(river_width = mean(Largeur_mod)) # Regrouper la table par l'identifiant 'Id_UEA' et resume la largeur de la riviere avec la fonction moyenne. 
st_write(largeur_riviere, 'C:/Meghana/TUTO/Traitements/pt_ref_largeur_riviere.sqlite', delete_layer = T) #Ecrire la table avec l'information sur la largeur de la riviere par UEA 

#Joindre la largeur de la riviere et l'objet sf UEA 
largeur_riviere = st_drop_geometry(largeur_riviere)# convertir l'objet sf en dataframe
UEA_larg_mod = dplyr::left_join(UEA,largeur_riviere, by=c('Id_UEA'='Id_UEA')) #Faire la jointure entre le shp des UEA et la table avec la largeur de la riviere, selon la cle primaire 'Id_UEA'

#Creation des polygone de la riviere 
path_name_buffer_largeur_mod = 'C:/Meghana/TUTO/Donnees/riviere/' #Chemin d'access vers le dossier pour les polygones de rivieres

for (i in 1:nrow(UEA_larg_mod)) {
  river = UEA_larg_mod[i, ]
  river_Id_UEA = river$Id_UEA
  dist = river$river_width
  if (!is.na(dist)) {
    buffer = st_buffer(river, dist)
    name_buffer = paste0(path_name_buffer_largeur_mod, river_Id_UEA, '.shp')
    st_write(buffer, name_buffer, delete_layer = T) #write the buffer shapefiles to a folder
  } else {
    print(paste0('No available river width for ', river$id))
  }
  
}

#Creation des polygones de la riviere par UREC (couper le long des limites du polygone UREC)

list_file_water_surface = list.files(path_name_buffer_largeur_mod, pattern = '*.shp', full.names = T) #creer une liste des fichier riviere
path_Riviere_UREC =  'C:/Meghana/TUTO/Donnees/UREC_riviere/' #le chemin d'access vers le dossier ou mettre le polygone UREC_riviere

#Boucle for pour creer les polygones UREC_riviere
for (i in 1:length(list_file_water_surface)) {
  water = vect(list_file_water_surface[i])
  water_id = water$Id_UEA
  for (n in 1:nrow(UREC_merge)) {
    shp = vect(UREC_merge[n, ])
    Id_uea = shp$Id_UEA
    if (Id_uea == water_id) {
      Id = shp$id
      name = paste0(Id,'.shp')
      UREC_water = terra::intersect(water, shp)
      terra::writeVector(UREC_water, filename = path_Riviere_UREC ,layer = name, overwrite = T)
    }
  }
}


#Canope en surplomb sur la riviere
################################################
#preprocessing
###############

#Fusionner toutes les UREC_riviere en un fichier:
list_files_UREC_water = list.files(path_Riviere_UREC, pattern = "*.shp$", full.names = T)
UREC_water_merge = st_read(list_files_UREC_water[1]) #initierla variabe UREC_water_merge pour faire la combinaison future des UREC_rivere
for (i in 2:length(list_files_UREC_water)){
  water_rive = st_read(list_files_UREC_water[i])
  UREC_water_merge =rbind(UREC_water_merge, water_rive)
}
st_write(UREC_water_merge,'C:/Meghana/TUTO/Donnees/UREC_water_merge.shp') #Ecrire le fichier contenant toutes les UREC_water_merge dans donnees



#Canope en surplomb sur la riviere
################################################
#preprocessing
###############
mask_urbain_vectoriel = vect('C:/Meghana/TUTO/Donnees/utilisation_territoire/urbain_vecteur.shp')

#Charger le raster MHC couper a l'etendue de nos unites spatiales
path_mhc = 'C:/Meghana/TUTO/Traitements/MHC/'
raster_mhc = raster(paste0(path_mhc, 'mask_MHC_21L13SE.tif'))

#Creer le vrt du mask MHC pour reduire le temps de computation des analyses. !Attention il faut creer un vrt par groupe de tuile avec la meme projection!
#Cette etape n'est que necessaire une fois. 
mhc_list_file = list.files(path_mhc,pattern="*.tif$",full.names=TRUE) #creer une liste des fichiers dans le dossier ombrage (dans le tutoriel ce n'est qu'un fichier)
mhc_file_vrt = paste0(path_mhc, 'mhc.vrt')
terra::vrt(mhc_list_file, filename=mhc_file_vrt, options=NULL, overwrite=FALSE) #creation du vrt !!! Il faut travailler avec le vrt quand le raster est tres grand, sinon la computation ne sera pas possible !!!

#charger le VRT
mhc_vrt = raster::raster(paste0(path_mhc, 'mhc.vrt'))

#Trouver l'etendu du mhc_vrt
etendu_mhc = ext(mhc_vrt)#MHC7

#ouvrire le fichier des UREC_riviere fussioner par unites spatiales 
UREC_water_merge_vect = vect(UREC_water_merge) #transformer l'objet sf UREC_water_merge) en objet spatVector
UREC_water_merge_proj = terra::project(UREC_water_merge_vect, 'EPSG:2949') #Reprojeter le vecteur UREC_water_merge a la meme projection que le raster virtuel mhc. On peut utiliser la fonction crs(mhc_vrt) pour trouver le code ESPG

#Couper le vecteur UREC_water a l'entedu du raster 
UREC_water_merge_proj = terra::crop(UREC_water_merge_proj, etendu_mhc) #Couper le vecteur des unites spatiales a l'etendu du raster mhc. (Il faudra faire cette etapes pour chaque groupe de projection des tuilles MHC). 

#Run OverhangingCanopy function on water merged 
UREC_water_merge_proj = OverhangingCanopy(UREC_water = UREC_water_merge_proj, raster_file = mhc_vrt, EPSG = 'EPSG:2949',urban_vector_mask = mask_urbain_vectoriel)
UREC_water_merge_proj$Id_UEA = UREC_water_merge_proj$Id_UEA_1
UREC_water_merge_proj = subset(UREC_water_merge_proj, selec = c(Id_UEA,rive, id, canopyRatio))
st_write(UREC_water_merge_proj, 'C:/Meghana/TUTO/Traitements/UREC_water_Canopee.sqlite', delete_layer = T) #ecrire les resultats de la canopee en surplomb


###########################################################################
#CREATION DE L'INDICE DE REGULATION DE LA TEMPERATURE DE L'EAU
######################################################################
#Normalisation des metriques d'ombrage (il n'a pas besoin de faire l'verse ici puisqu'aucune des metriques n'est le taux de dispersions)
UREC_merge_IndiceRegTemp_norm = Normalization_function(UREC_merge_IndiceRegTemp_norm)

UREC_merge_IndiceRegTemp_norm = st_read( 'C:/Meghana/TUTO/Traitements/UREC_water_Canopee.sqlite')
UREC_merge_IndiceRegTemp_norm = Normalization_function(UREC_merge_IndiceRegTemp_norm)


#Creation de l'indice de la fonction de la regulation de la temperature de l'eau. 
UREC_merge_IndiceRegTemp_norm$Indice_regulation_temp = UREC_merge_IndiceRegTemp_norm$canopyratio_nrm
                                                  
st_write(UREC_merge_IndiceRegTemp_norm, 'C:/Meghana/TUTO/Resultats/resultats_metrique_Norm_IndiceOmbrage.sqlite', delete_layer = T)


################################################################################
#Creation de l'Indice ISEER preliminaire
#################################
IndiceRegTemp = st_read('C:/Meghana/TUTO/Resultats/resultats_metrique_Norm_IndiceOmbrage.sqlite') #lire les fichier avec les resultats de l'indice de regulation de la temperature de l'eau
IndiceRegTemp = subset(IndiceRegTemp, select = c(id, canopyratio_nrm                       ))
IndiceConP = st_read('C:/Meghana/TUTO/Resultats/resultats_indice_Norm_IndiceConnectivitePaysage.sqlite') #Lire les fichiers avec les resultats de l'indice de connectibite du paysage
IndiceConP = subset(IndiceConP, select = c(id,pd_vegetation_optimale_nrm,continuity_vegetation_optimale_nrm                     ))
IndiceConP_df = st_drop_geometry(IndiceConP) #transformer objet sf en dataframe

ISEER = left_join(IndiceRegTemp, IndiceConP_df, by = c('id' = 'id')) #joindre les deux indice de fonction ecologique pour faciliter le calcul de l'ISER
ISEER$ISEER = (ISEER$continuity_vegetation_optimale_nrm +
                 ISEER$pd_vegetation_optimale_nrm+
                 ISEER$canopyratio_nrm)/3
st_write(ISEER,'C:/Meghana/TUTO/Resultats/ISEER_preliminaire.sqlite', delete_layer = T)





