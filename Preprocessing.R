#This script is to  pre-processing data to create spatial units (rive gauche, rive droite)
#and create sampling plots un spatial units with a disaggregation loop


#Install and import packages 
#Install and import libraries 
pkgs = c(
  "sf",
  "sp",
  "raster",
  "rgdal",
  "dplyr",            
  "units",
  "purrr",
  "lwgeom",
  "gdalUtils",
  "fasterize",
  "xlsx",
  "nngeo")
to_install = !pkgs %in% installed.packages()
if(any(to_install)) {
  install.packages(pkgs[to_install])
}

# load libraries
library(sp)
library(sf)
library(raster)
library(gdalUtils)
library(dplyr)
library(units) 
library(purrr) 
library(lwgeom) 
library(xlsx)
library (nngeo)

setwd = 'C:/Meghana/Belgique'

#############################################
#Preprocessing of tables 
path0 = 'C:/Meghana/Belgique/traitements/data/UREC_full'
file_list = list.files(path0, pattern = '*.shp', full.names = TRUE)
UREC_bul = st_read('C:/Meghana/Belgique/traitements/data/UREC_full/UREC.shp', stringsAsFactors = F)
UREC_bul =st_zm(UREC_bul, drop = TRUE, what = 'ZM')
UREC_bulstrode = st_read('C:/Meghana/Belgique/traitements/data/UREC_full/UREClin_merge_valid_Bulstrode.shp', stringsAsFactors = F)
UREC_bulstrode =st_zm(UREC_bulstrode, drop = TRUE, what = 'ZM')
UREC_saults = st_read('C:/Meghana/Belgique/traitements/data/UREC_full/UREC_saults.shp', stringsAsFactors = F)
UREC_saults =st_zm(UREC_saults, drop = TRUE, what = 'ZM')

UREC_bul$ID_UEA = UREC_bul$Id_UEA
UREC_bul = UREC_bul[,'ID_UEA']
UREC_saults = UREC_saults[,'ID_UEA']

#####PREPROCESSING #######
######for Riviere Jacques Cartier
jacques_full = st_read('C:/Meghana/Belgique/donnees_brutes/JacquesCartier/JacquesCartier_UREC_full.shp', stringsAsFactors = F)
jacques_full = select(jacques_full, 'Id_UEA_1')
jacques_full$ID_UEA = jacques_full$Id_UEA_1
jacques_full = select(jacques_full, 'ID_UEA')
st_write(jacques_full, 'C:/Meghana/Belgique/donnees_brutes/sampling_full/JacquesCartier/jacques_full.shp', overwrite = T)

jacques_rives = st_read('C:/Meghana/Belgique/donnees_brutes/JacquesCartier/JacquesCartier_UREC_rives.shp', stringsAsFactors = F)
jacques_rives = select(jacques_rives, 'ID_UEA')
jacques_rives$Id_rive = seq.int(nrow(jacques_rives))
st_write(jacques_rives, 'C:/Meghana/Belgique/donnees_brutes/sampling_rives/JacquesCartier/jacques_rives.shp', delete_layer = T)

for (i in 1:nrow(jacques_rives)){
  rive = jacques_rives[i,]
  rive_Id_UEA = rive$ID_UEA
  rive_Id_rive = rive$Id_rive
  
  name = paste0('C:/Meghana/Belgique/donnees_brutes/sampling_rives/JacquesCartier/',rive_Id_UEA, '_rive', rive_Id_rive, '.shp')
  st_write(rive,name, delete_layer =  T)
  
  
}

######for Riviere Castor
Castor_full = st_read('C:/Meghana/Belgique/donnees_brutes/Castor/Castor_UREC_full.shp', stringsAsFactors = F)
Castor_full = select(Castor_full , 'Id_UEA_1')
Castor_full$ID_UEA = Castor_full$Id_UEA_1
Castor_full = select(Castor_full, 'ID_UEA')
st_write(Castor_rives, 'C:/Meghana/Belgique/donnees_brutes/sampling_full/Castor/castor_full.shp', delete_layer = T)

Castor_rives = st_read('C:/Meghana/Belgique/donnees_brutes/Castor/Castor_UREC_rives.shp', stringsAsFactors = F)
Castor_rives$ID_UEA = Castor_rives$Id_UEA
Castor_rives = select(Castor_rives, 'ID_UEA')
Castor_rives$Id_rive = seq.int(nrow(Castor_rives))
st_write(Castor_rives, 'C:/Meghana/Belgique/donnees_brutes/sampling_rives/Castor/castor_rives.shp', delete_layer = T)

for (i in 1:nrow(Castor_rives)){
  rive = Castor_rives[i,]
  rive_Id_UEA = rive$ID_UEA
  rive_Id_rive = rive$Id_rive
 
  name = paste0('C:/Meghana/Belgique/donnees_brutes/sampling_rives/Castor/',rive_Id_UEA, '_rive', rive_Id_rive, '.shp')
  st_write(rive,name, delete_layer =  T)
}

#####for Riviere Wallbridge
Wallbridge_full = st_read('C:/Meghana/Belgique/donnees_brutes/Wallbridge/Wallbridge_UREC_full.shp', stringsAsFactors = F)
Wallbridge_full$ID_UEA = Wallbridge_full$Id_UEA
Wallbridge_full= select(Wallbridge_full, 'ID_UEA')
st_write(Wallbridge_full, 'C:/Meghana/Belgique/donnees_brutes/sampling_full/Wallbridge/Wallbridge_full.shp', delete_layer = T)

Wallbridge_rives = st_read('C:/Meghana/Belgique/donnees_brutes/Wallbridge/Wallbridge_UREC_rives.shp', stringsAsFactors = F)

UREC_merge = rbind(UREC_bul, UREC_bulstrode, UREC_saults, jacques_full, Castor_full)
UREC_merge = st_zm(UREC_merge, drop = TRUE, what = 'ZM')

st_write(UREC_bul,'C:/Meghana/Belgique/traitements/data/UREC_full_n/UREC_Bulstrode1.shp', overwrite = T)
st_write(UREC_bulstrode,'C:/Meghana/Belgique/traitements/data/UREC_full_n/UREC_Bulstrode2.shp', overwrite = T)
st_write(UREC_saults,'C:/Meghana/Belgique/traitements/data/UREC_full_n/UREC_Saults.shp', overwrite = T)
st_write(UREC_merge,'C:/Meghana/Belgique/traitements/data/UREC_full_merge/UREC_merge.shp', delete_layer  = T)

##### import UEA data and merge 
UEA = st_read('C:/Meghana/Belgique/traitements/data/UEA/UEA_newId.shp', stringsAsFactors = F)
UEA$ID_UEA = UEA$Id_UEA_2
UEA = select(UEA, 'ID_UEA')
UEA_J = st_read('C:/Meghana/Belgique/donnees_brutes/JacquesCartier/UEA_JacquesCartier.shp', stringsAsFactors = F)
UEA_J = select(UEA_J, 'ID_UEA')
UEA_C = st_read('C:/Meghana/Belgique/donnees_brutes/Castor/UEA_lin_Castor.shp', stringsAsFactors = F)
UEA_C$ID_UEA = UEA_C$Id_UEA_1
UEA_C = select(UEA_C, 'ID_UEA')
UEA = rbind(UEA, UEA_C, UEA_J)

path0 = 'C:/Meghana/Belgique/traitements'
#Step 1: Import UEAf_merge data
file_in = paste0(path0, '/data/UEAf_merge/UEAf_merge.shp')
UEA = st_read(file_in, stringsAsFactors = F)
UEA = st_zm(UEA, drop = TRUE, what = 'ZM')#Remove z dimension 
#UEA = st_cast(UEA, to = 'LINESTRING')

#Import UREC_merge if not already done
file_in = 'C:/Meghana/Belgique/traitements/data/UREC_full_merge/UREC_merge.shp'
UREC_merge = st_read(file_in  , stringsAsFactors = F)


#######################################################################
#Can be used if data is properly formatted to at the start (need to make for loop for that)
#Put the UREC shapefiles in a list
path0 = paste0(path0, 'data/UREC/UREC_valid')
file_list = list.files(path0, pattern = '*.shp', full.names = TRUE)
#read all the shapefiles and store them in a  list
shapefile_list = lapply(file_list, FUN = st_read)
#merge all the shapefiles into one new variable that can be written out
UREC <- do.call(rbind, shapefile_list)
st_write(UREC, 'C:/Meghana/Belgique/data/UREC_merge.shp', overwrite = TRUE)
#######################################################################

#Step2 : Clip UEAf_merge with UREC_merge and get rid of weird geometry 
UEA_sub =  st_read('C:/Meghana/Belgique/donnees_brutes/UEA.shp', stringsAsFactors = F)
#step3 : Create zone tampon around UEA
tampon_file_path = 'C:/Meghana/Belgique/traitements/results/tampon/'

for (i in 1:nrow(UEA)){
  segment = UEA[i,]
  segment_buff = st_buffer(segment, dist = 100)
  name = segment$ID_UEA
  file_name = paste0(tampon_file_path, name , '.shp')
  st_write(segment_buff, file_name, delete_layer = TRUE)
  
}


#import data
#Step to seperate rive gauche et rive droite
path0 = 'C:/Meghana/Belgique/traitements/data/UREC_rives'
file_list = list.files(path0, pattern = '*.shp', full.names = TRUE)
UREC_rive_bul = st_read('C:/Meghana/Belgique/donnees_brutes/UREC_rives/UREClin_Bul.shp', stringsAsFactors = F)
UREC_rive_bul =st_zm(UREC_rive_bul, drop = TRUE, what = 'ZM')
UREC_rive_bulstrode = st_read('C:/Meghana/Belgique/donnees_brutes/UREC_rives/UREC_Bulstrode.shp', stringsAsFactors = F)
UREC_rive_bulstrode =st_zm(UREC_rive_bulstrode, drop = TRUE, what = 'ZM')
UREC_rive_saults = st_read('C:/Meghana/Belgique/donnees_brutes/UREC_rives/UREClin_Saults.shp', stringsAsFactors = F)
UREC_rive_saults =st_zm(UREC_rive_saults, drop = TRUE, what = 'ZM')

UREC_rive_bul= UREC_rive_bul[,'ID_UEA']
UREC_rive_bulstrode$ID_UEA = UREC_rive_bulstrode$Id_UEA
UREC_rive_bulstrode = UREC_rive_bulstrode[,'ID_UEA']
UREC_rive_saults = UREC_rive_saults[,'ID_UEA']

UREC_rive_merge = rbind(UREC_rive_bul, UREC_rive_bulstrode, UREC_rive_saults)
UREC_rive_merge = st_zm(UREC_rive_merge, drop = TRUE, what = 'ZM')

UREC_rive_merge$Id_rive = seq.int(nrow(UREC_rive_merge))
UREC_rive_merge = st_make_valid(UREC_rive_merge)
st_write(UREC_rive_merge, 'C:/Meghana/Belgique/donnees_brutes/UREC_rive_merge.shp', delete_layer = TRUE)


#merge UREC_rive with rives from riviere Jacques Cartier et Castor
UREC_rive_merge = st_read('C:/Meghana/Belgique/donnees_brutes/UREC_rive_merge.shp', stringsAsFactors = F)


UREC_rive_jacques = st_read( "C:/Meghana/Belgique/donnees_brutes/sampling_rives/JacquesCartier/jacques_rives.shp", stringsAsFactors = F)
UREC_rive_jacques$ID_UEA = UREC_rive_jacques$Id_UEA
UREC_rive_jacques = select(UREC_rive_jacques, 'ID_UEA')
UREC_rive_castor = st_read( "C:/Meghana/Belgique/donnees_brutes/sampling_rives/Castor/castor_rives.shp", stringsAsFactors = F)
UREC_rive_castor$ID_UEA = UREC_rive_castor$Id_UEA
UREC_rive_castor = select(UREC_rive_castor, 'ID_UEA')
UREC_rive_merge = rbind(UREC_rive_merge, jacques_rives,Castor_rives )
st_write(UREC_rive_merge, 'C:/Meghana/Belgique/donnees_brutes/UREC_rive_merge_new.shp', delete_layer = T)
################################################################################
#Step 4 : make zone d'echantillonnage
###############################################################################

##For loop for sampling plot based on UREC_full
path0 = 'C:/Meghana/Belgique/traitements/results/Centerline_dissolved/'
file_list = list.files(path0, pattern = '*.shp', full.names = TRUE)

#read all the shapefiles and store them in a  list
sampling_file_path = 'C:/Meghana/Belgique/traitements/results/sampling_full/'
i = 1
#iterate through each feature
for (i in 1:length(file_list)){
  axe = st_read(file_list[i], stringsAsFactors = F) #read each centerline
  axe_name = file_list[i]
  Id =  sub(".*Centerline_dissolved/", "", axe_name)
  Id = sub(".shp.*", "", Id)
  semis = st_line_sample(axe, density = 0.02, type = 'regular') #sample points along line every 50m
  #create polygon of voronoi at each point and cast it to have the same geometry
  poly_voronoi = st_voronoi(semis) %>% st_cast() #create polygon of voronoi
  #filter UREC_merge par Id_UEA:                  # fin the associate UREC to the axe based on Id_UEA key
  query = UREC_merge$ID_UEA == Id
  UREC_sub = dplyr::filter(UREC_merge, query)
  #cross the UREC data with the polygon of voronoi to obtain sampling plots
  sampling = st_intersection(UREC_sub, poly_voronoi) 
  #Write these new sampling shapefile to folder
  file_name = paste0(sampling_file_path, Id , '.shp')
  st_write(sampling, file_name, delete_layer = TRUE)
  #plot(st_geometry(sampling))
}


##For loop for sampling plot based on UREC_rive (seperate rive gauche/droite) 
file_list_sampling_full <- list.files("C:/Meghana/Belgique/traitements/results/sampling_full", pattern = "*.shp", full.names = TRUE)
sampling_rive_path = 'C:/Meghana/Belgique/traitements/results/sampling_rives/'

for (i in 1:length(file_list_sampling_full)){
  sampling_full = st_read(file_list_sampling_full[i])
  ID_UEA_sampling = sampling_full$ID_UEA[1] #set key to compare sampling_full shapefiles with UREC_rive_merge shapefile
  for (l in 1:nrow((UREC_rive_merge))){
    shp = UREC_rive_merge[l,]
    Id_rive = shp$Id_rive
    ID_UEA_rive = shp$ID_UEA # set key for UREC_rive_merge
    
    if (ID_UEA_rive != ID_UEA_sampling){ #compare UREC_rive_merge at key 'Id_UEA' and sampling_full at key 'Id_UEA'
      next
    }
  sampling_rive = st_intersection(sampling_full,shp ) #if the key are the same do their intersection
  sampling_rive = st_cast(sampling_rive, to = 'MULTIPOLYGON') #fix geometry
  sampling_rive = select(sampling_rive, -c('ID_UEA.1')) #get rid of extra key column
  sampling_rive$ID_sampling = seq.int(nrow(sampling_rive))
  sampling_rive = st_make_valid(sampling_rive)
  file_name = paste0(sampling_rive_path,ID_UEA_rive, '_rive', Id_rive, '.shp') 
  st_write(sampling_rive, file_name, delete_layer = TRUE) #write the rive shapefile to a folder
  }
}

#For loop to get unsegmented rive gauche / droite UREC # this i where we will store the different results of metrics
UREC_rives_list = list.files("C:/Meghana/Belgique/traitements/results/sampling_rives", pattern = '*.shp', full.names = T)
path_name_UREC_rives = "C:/Meghana/Belgique/traitements/results/UREC_rives_new/"

for (i in 1:length(UREC_rives_list)){
  shp = st_read(UREC_rives_list[i])
  Id_rive = shp$Id_rive[1]
  Id_UEA = shp$ID_UEA[1]
  file_name = paste0(path_name_UREC_rives,Id_UEA,'_rive', Id_rive, '.shp')
  shp_union = st_union(shp)
  shp_union = st_sf(shp_union)
  shp_union$Id_UEA = Id_UEA
  shp_union$Id_rive = Id_rive
  st_write(shp_union,file_name, delete_layer = TRUE)
}


###############Add Id_UEA column to centerline dissolve 
centerline_D_list = list.files('C:/Meghana/Belgique/traitements/results/Centerline_dissolved/', pattern = '*.shp', full.names = T)
centerline_new_path = 'C:/Meghana/Belgique/traitements/results/Centerline_dissolved_new/'
i = 1

for (i in 1:length(centerline_D_list)){
  centerline = st_read(centerline_D_list[i], stringsAsFactors = F)
  axe_name = centerline_D_list[i]
  Id =  sub(".*Centerline_dissolved/", "", axe_name)
  Id = sub(".shp.*", "", Id)
  centerline_file_name = paste0(centerline_new_path,Id, '.shp')
  centerline$ID_UEA = Id
  centerline = select(centerline, 'ID_UEA')
  st_write(centerline, centerline_file_name, delete_layer = T )
}

################################################################################
#Check if this is done in Metrics folder!!!
################################################################################
#Create MNS data from MNT and MHC
#import file_list mnt and mhc
path_MNS_folder = 'C:/Meghana/Belgique/traitements/data/MNS'
list_files_mns = list.files(path_MNS_folder, pattern = '*.tif', full.names = T)
folder_list = list.dirs(path_MNS_folder,  full.names = TRUE, recursive = F)

for (f in 1:length(folder_list)){
  folder = folder_list[f]
  folder_name = sub(".*MNS/", "", folder)
  mnt = raster(paste0(folder, '/', 'MNT_', folder_name, '.tif'))
  crs(mnt )<-CRS("+init=epsg:2949")
  mhc = raster(paste0(folder, '/', 'MHC_', folder_name, '.tif'))
  crs(mhc )<-CRS("+init=epsg:2949")
  mns = mnt + mhc 
  crs(mns )<-CRS("+init=epsg:2949")
  MNS_file_name = paste0(path_MNS_folder, '/MNS_', folder_name, '.tif')
  writeRaster(mns,MNS_file_name, overwrite = T )
  
}

path_mnt = 'C:/Meghana/Belgique/traitements/data/MNT/'
file_list_mnt = list.files(path_mnt, pattern = '*.tif', full.names = T)
path_mhc = 'C:/Meghana/Belgique/donnees_brutes/LiDAR/MHC/'
file_list_mhc = list.files(path_mhc, pattern = '*.tif', full.names = T)


path_mnt #path towards the MNT folder in traitements
path_mhc #path towards the mhc folder 
list_file_mnt
list_file_mhc = list.files(path_mhc, pattern = '*.tif', full.names = T)

f = 2
l = 2

test_mnt = raster::raster(list_file_mnt[1])
ext_mnt = raster::extent(test_mnt)
test_mhc = raster::raster(list_file_mhc[1])
test_mns <- raster(ext_mnt, resolution = 1, crs = crs(mnt))

typeof(test_mns)

for (x in 1:10){
  print(x)
}

for (x in 1:ncol(test_mnt)){
  for (y in 1:nrow(test_mnt)){
    print(paste0(x,',',y))
    test_mns[x,y] = test_mnt[x,y] + test_mhc[x,y]
  }
}

test_mns[1, 1] = test_mnt[1,1] + test_mhc[1,1]
print(test_mns[1,1])

print(test_mnt[1,1])
print(test_mhc[1,1])




  full_name = list_file_mnt[f]
  mnt_id = sub("C:/Meghana/Belgique/traitements/data/MNT/MNT_", "", full_name)
  for (l in 1:length(list_file_mhc)){
    mhc = raster(list_file_mhc[l])
    full_name_mhc = list_file_mhc[l]
    mhc_id =  sub("C:/Meghana/Belgique/traitements/data/MHC/MHC_", "", full_name_mhc)
    if (mnt_id == mhc_id){
      mns = mnt + mhc
      outputRaster <- raster::overlay(mnt, mhc, fun=function(r1, r2){return(r1+r2)})
      mns_id = paste0('/MNS_', mhc_id)
      writeRaster(mns, paste0(path_MNS_folder, mns_id), overwrite = T)
    }
  }
}

#Extract the extents of the raster files as a shp to then get the centroid
i = 1
for (i in 1:length(list_files_mns)){
  mns = rast(list_files_mns[i])
  ext = terra::ext(mns)
  ext = as.vector(ext)
  ext_sf = vect(ext)
  mns_name = list_files_mns[i]
  mns_name = sub("C:/Meghana/Belgique/traitements/data/MNS/MNS_", "", mns_name)
  ext_name = paste0("extent_",sub("\\|*.tif", "", mns_name), '.shp')
  writeVector(ext, paste0(path_MNS_folder,'/emprise/',ext_name))
}




############################################################
#############################################################
#pieace of code that could be usefull in the future
UEA_clip = st_intersection(UEA,UREC_merge)
 
for (i in 1:nrow(UEA_clip)){
  row = UEA_clip[i,]
  type = st_geometry_type(row)
  if (type == 'POINT'|type == 'MULTILINESTRING'){
    UEA_clip = UEA_clip[-i,]
  }
}

#Some UEA have the same Id_UEA field, we need to sort them and group them
UEA_sort = UEA_clip %>% group_by(Id_UEA) %>% summarize() %>% st_cast('MULTILINESTRING')
st_write(UEA_sort, 'C:/Meghana/Belgique/traitements/data/UEA_sort.shp')
#Check that primary 'Id_UEA' is unique
Id = UEA_sort[,'Id_UEA']
isUnique <- function(vector){
  return(!any(duplicated(vector)))
}
isUnique(test) #if TRUE, then all the Id_UEA are unique
  
#Step 3 : For each feature in UEA_clip create a buffer zone and write it to a result file
tampon_file_path = 'C:/Meghana/Belgique/traitements/results/tampon/'
for (i in 1:nrow(UEA_sort)){
  segment = UEA_sort[i,]
  segment_buff = st_buffer(segment, dist = 100)
  name = segment$Id_UEA
  file_name = paste0(tampon_file_path, name , '.shp')
  st_write(segment_buff, file_name, delete_layer = TRUE)
  
}

#check that 50 file tampon were created 
tampon_file_list = list.files(tampon_file_path, pattern = '*.shp', full.names = TRUE)


name = sub(".*tampon/", "", tampon_file_list)
name = sub("\\|*.shp", "", name)

check = list()
for (i in 1:length(tampon_file_list)){
  name = tampon_file_list[i]
  name = sub(".*tampon/", "", name)
  name = sub("\\|*.shp", "", name)
  check = append(check, name)
}
df = do.call(rbind.data.frame, check)
missing_id = Id$Id_UEA[!Id$Id_UEA %in%
                         df$c..03AG_B2_100....03AG_B2_12....03AG_B2_32....03AG_B2_35....03AG_B2_43...]


