#This file is to calculate metrics in the spatial units that are disaggregated (see preprocessing r document)

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
  "nngeo",
  'installr',
  'RSAGA',
  `leaisgreat`
)
to_install = !pkgs %in% installed.packages()
if (any(to_install)) {
  install.packages(pkgs[to_install])
}

# load libraries and set working directory
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
library(installr)
library(RSAGA)
library(terra)
library(exactextractr)
library(whitebox)
setwd = 'C:/Meghana/Belgique'




#Put UREC_rive files in list

path_UREC_rive = 'C:/Meghana/Belgique/traitements/results/sampling_rives'
file_list = list.files(path_UREC_rive, pattern = '*.shp', full.names = T)

#Physical parameters of riparian ecosystems

#import DATA and path files for for loops

#import list of files of UREC_rive
path_name_UREC_rives = 'C:/Meghana/Belgique/traitements/results/UREC_rives_new/'
list_files_UREC_rives = list.files(
  'C:/Meghana/Belgique/traitements/results/UREC_rives_new',
  pattern = '*.shp',
  full.names = T
)
#import list of files for sampling plots on UREC_rive
list_files_sampling_rive = list.files(
  'C:/Meghana/Belgique/traitements/results/sampling_rives',
  pattern = '*.shp',
  full.names = T
)
#Import list of files for central axes of valley
path_centerline_dissolve = 'C:/Meghana/Belgique/traitements/results/Centerline_dissolved_new'
file_list_centerline_dissolve = list.files(path_centerline_dissolve,
                                           pattern = '*.shp',
                                           full.names = T)
#import list of files for UEA
#path_UEA = 'C:/Meghana/Belgique/traitements/data/UEA'
#file_list_UEA = list.files(path_UEA, pattern = '*.shp', full.names = T)
UEA = st_read('C:/Meghana/Belgique/traitements/data/UEA/UEA_new_complete.shp', stringsAsFactors = F)
UEA_old = st_read('C:/Meghana/Belgique/traitements/data/UEA/UEA.shp', stringsAsFactors = F)

#import shp of PtRef and PtRef_mod_lotique
PtRef03 <- st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb", #this may need to change depending on the UDH
                    layer = "PtRef",
                    stringsAsFactors = F)
PtRef_mod_lotique03 = st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb", 
                            layer = "PtRef_mod_lotique",
                            stringsAsFactors = F)
PtRef05 = st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb", #this may need to change depending on the UDH
                  layer = "PtRef",
                  stringsAsFactors = F)
PtRef_mod_lotique05 = st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb", 
                              layer = "PtRef_mod_lotique",
                              stringsAsFactors = F)
#I made in ArcGIS a clip version of PtRef with only the points we needed (that intersect with UEA)
PtRef_clip = st_read('C:/Meghana/Belgique/traitements/data/PointRef/PtRef_clip_complete.shp')
PtRef_clip = st_cast(PtRef_clip, to = 'POINT')

UEA_join = left_join(UEA, PtRef03, by = c('ID_UEA' = 'Id_UEA'))


##############################################################################################
#CALCULATING METRICS IN FOR LOOP
#############################################################################################
#Metric : Lateral width of the spatial unit
################################################################################
for (r in 1:length(list_files_UREC_rives)) {
  rive = st_read(list_files_UREC_rives[r])
  Identifiant_UEA_gd = rive$Id_UEA
  Identifiant_rive_gd = rive$Id_rive
  rive_name = paste0(Identifiant_UEA_gd, '_rive', Identifiant_rive_gd)
  
  for (l in 1:length(file_list_centerline_dissolve)) {
    axe = st_read(file_list_centerline_dissolve[l])
    identifiant_UEA = axe$ID_UEA
    
    if (Identifiant_UEA_gd == identifiant_UEA) {
      st_cast(axe, to = 'LINESTRING')
      semis <-
        st_line_sample(axe, density = 0.02 , type = 'regular')
      voronoi <- st_voronoi(semis , bOnlyEdges = T)
      # convertir en lignes simples
      voronoi <- st_cast(voronoi , to = 'LINESTRING')
      
      voronoi_lines <- st_intersection(voronoi , rive)
      width_UREC = st_length(voronoi_lines)
      median_width = median(width_UREC)
      rive$width = median_width
      st_write(rive,
               paste0(path_name_UREC_rives, rive_name, '.shp'),
               delete_layer = TRUE)
    }
  }
}

################################################################################
#Metric : Sinuosity
################################################################################
for (r in 1:length(list_files_UREC_rives)) {
  spt_unit_rive = st_read(list_files_UREC_rives[r])
  spt_unit_Id_UEA = spt_unit_rive$Id_UEA
  spt_unit_Id_rive = spt_unit_rive$Id_rive
  spt_unit_name = paste0(spt_unit_Id_UEA, '_rive', spt_unit_Id_rive)
  for (i in 1:nrow(UEA)) {
    sgmt = UEA[i, ]
    sgmt_Id = sgmt$ID_UEA
    if (spt_unit_Id_UEA == sgmt_Id) {
      for (a in 1:length(file_list_centerline_dissolve)) {
        sgmt_length = as.numeric(st_length(sgmt))
        axe = st_read(file_list_centerline_dissolve[a])
        identifiant_UEA = axe$ID_UEA
        if (spt_unit_Id_UEA == identifiant_UEA) {
          sinuosity = as.numeric(st_length(axe)) / sgmt_length
          spt_unit_rive$sinuosity = sinuosity
          st_write(spt_unit_rive,
                   paste0(path_name_UREC_rives, spt_unit_name, '.shp'),
                   delete_layer = TRUE)
        }
      }
    }
  }
}

################################################################################
#Metric : Forest conitnuity and grassland continuity and surface of anthropigenic areas (urbain and agricultur)
##################################################################
#import new qgis worldcover data (was not able to do this in R)

#Make raster stack of worldmap ESA raster layers
path_esa = paste0(getwd(),"/donnees_brutes/ESA_WORLDCOVER/data")
list_file=list.files(path_esa,pattern="*.tif$",full.names=TRUE)
file_vrt=paste0(path_esa,"/esa.vrt")
gdalbuildvrt(gdalfile=list_file, output.vrt= file_vrt,overwrite=TRUE)

#esa = rast(file_vrt) # this is esa with projection WGS84 (EPSG:4326)
esa= rast('C:/Meghana/Belgique/donnees_brutes/ESA_WORLDCOVER/data/esa_Qc_lamb.tif')

r = 3
i = 3

for (r in 1:length(list_files_UREC_rives)) {
  shp = st_read(list_files_UREC_rives[r])
  shp_file_name = paste0(path_name_UREC_rives,
                         shp$Id_UEA,
                         '_rive',
                         shp$Id_rive,
                         '.shp')
  for (i in 1:length(list_files_sampling_rive)) {
      UREC_vect = st_read(list_files_sampling_rive[i])
      UREC = vect(list_files_sampling_rive[i])
      if ((shp$Id_UEA == UREC_vect$ID_UEA[1]) &
          (shp$Id_rive == UREC_vect$Id_rive[1])) {
      UREC = terra::project(UREC, esa) # project vector UREC file to same projection as ESA raster file (WGS84)
      esa_clip = terra::crop(esa, UREC) #Reduce the extent of the ESA raster to the extent of UREC
      UREC_raster = terra::rasterize(UREC, esa_clip) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
      esa_mask = terra::mask(esa_clip, UREC_raster)  #create mask on raster with UREC shapefile
      #Create a forest mask
      esa_F = esa_mask
      esa_F[esa_F != 10] <- NA # replace what is not forest by NA
      esa_F[esa_F == 10] <- 1 # replace 10 (forest) by 1
      #create grassland mask
      esa_G = esa_mask
      esa_G[esa_G != 30] <- NA # replace what is not greassland by NA
      esa_G[esa_G == 30] <- 1 # replace 30 (grassland) by 1
      #Create an urban mask 
      esa_U = esa_mask
      esa_U[esa_U != 50] <- NA # replace what is not urban (built-up) by NA
      esa_U[esa_U == 50] <- 1 # replace 50 (built-up) by 1
      #Create an agriculture mask 
      esa_A = esa_mask
      esa_A[esa_A != 40] <- NA # replace what is not agriculture (cropland) by NA
      esa_A[esa_A == 40] <- 1 # replace 40 (cropland) by 1
      #Extract the surface of forested/ grassland/urban/agriculture pixels in each sub-sampling area and calculate continuity of forests
      UREC$surface_F = exactextractr::exact_extract(esa_F, UREC_vect, 'sum') *10 #multiply sum by area of each pixel (meed to do this with sf object!)
      UREC$surface_G = exactextractr::exact_extract(esa_G, UREC_vect, 'sum') *10
      UREC$surface_U = exactextractr::exact_extract(esa_U, UREC_vect, 'sum') *10
      UREC$surface_A = exactextractr::exact_extract(esa_A, UREC_vect, 'sum') *10
      
      UREC$area = as.numeric(st_area(UREC_vect)) #calculate the area of each sampling unit in the UREC
      UREC$propF = UREC$surface_F / UREC$area # get the ratio of forested area in each sampling unit by the area of the sampling unit
      UREC$propG = UREC$surface_G / UREC$area # get the ratio of grassland area in each sampling unit by the area of the sampling unit
      UREC$propU = UREC$surface_U / UREC$area # get the ratio of urban area in each sampling unit by the area of the sampling unit
      UREC$propA = UREC$surface_A / UREC$area # get the ratio of urban area in each sampling unit by the area of the sampling unit
      
      continuityF = median(UREC$propF) #create a variable of the median for continuity of forested areas
      continuityG = median(UREC$propG) #create a variable of the median for continuity of grassland areas
      continuityU = median(UREC$propU) #create a variable of the median for continuity of urban areas
      continuityA = median(UREC$propA) #create a variable of the median for continuity of urban areas
      
      #Put results in the UREC_rive_New files
      shp$contF = continuityF
      shp$contG = continuityG
      shp$contU = continuityU
      shp$contA = continuityA
      st_write(shp, shp_file_name, delete_layer = TRUE) #write shapefiles with new data
      
    }
  }
}

################################################
#Lets try with the new clipped to poly extent UT raster
#########################################################

file_esa_clip_poly = paste0(getwd(),"/donnees_brutes/ESA_WORLDCOVER/esa_clip_poly1.tif")
esa_clip_poly = raster(file_esa_clip_poly)
 # Mask all the pixels that are not forest and attribute a value of 1 to all pixels that are of class forest
mask_foret <- esa_clip_poly
mask_foret[mask_foret == 10] <- 1 # replace 10 (forest) by 1
mask_foret[mask_foret != 1] <- NA # replace what is not a 1 par NA
writeRaster(
  mask_foret,
  paste0(getwd(), '/donnees_brutes/ESA_WORLDCOVER/mask_forest_clip_poly.tif'), overwrite = T
)

i = 1
for (i in 1:length(list_files_sampling_rive)) {
  shp = st_read(list_files_sampling_rive[i])
  Id_UEA = shp$ID_UEA[1]
  Id_rive = shp$Id_rive[1]
  file_name = paste0(path_name_UREC_rives, '/',Id_UEA, '_rive', Id_rive)
  shp$s_forest = extract(mask_foret, shp, fun = sum, na.rm = T) * 10
  shp$AREA = st_area(shp) #calculate the area of each sampling unit in the UREC
  shp$propF = shp$s_forest / shp$AREA # get the ratio of forested area in each sampling unit by the area of the sampling unit
  continuityF = median(shp$prop)
  for (f in 1:length(list_files_UREC_rives)) {
    rive = st_read(list_files_UREC_rives[f])
    rive_Id_UEA = rive$Id_UEA
    rive_Id_rive = rive$Id_rive
    rive_name = paste0(rive_Id_UEA, '_rive', rive_Id_rive)
    if (rive_name != file_name) {
      next
    }
    rive$ContinuityF_percentage = continuityF * 100 #continuity in percentage
    st_write(rive,
             paste0(path_name_UREC_rives, rive_name, '.shp'),
             delete_layer = TRUE)
  }
}



#################################################################################






#Indice de continuite de l'habitat
#Still needs work
#################################################################################
for (i in 1:length(list_files_UREC_rives)){
  rive = st_read(list_files_UREC_rives[i])
  rive_Id_UEA = rive$Id_UEA
  rive_Id_rive = rive$Id_rive
  rive_name = paste0(rive_Id_UEA, '_rive', rive_Id_rive)
  Continuity = rive$CntntF_ +rive$CntntG_
  rive$Continuity = Continuity
  st_write(rive,
           paste0(path_name_UREC_rives, rive_name, '.shp'),
           delete_layer = TRUE)
  
  
}

#Create a new merge shapefile of all the UREC-rive shape files 
path_UREC_rive_brute = 'C:/Meghana/Belgique/donnees_brutes/UREC_rives_new'
list_file_UREC_rive_brute = list.files(path_UREC_rive_brute, pattern = '*.shp', full.names = T)
i = 1
for (i in 1:length(list_file_UREC_rive_brute)){
  rive = st_read(list_file_UREC_rive_brute[i])
  if (i == 1){
    rive_merge = rive
  }else{
    rive_merge = rbind(rive_merge, rive)
  }
}


#################################################################################

################################################################################
#Metric hauteur emerge au pied de berge THIS DOESN'T WORK YET!!!!
###############################################################################
path_mnt_mtm7 = paste0(getwd(), '/traitements/data/MNT/mtm7')
list_file_mnt_mtm7 = list.files(path_mnt_mtm7, patter = '*.tif', full.names = T)
vrt_mnt_mtm7 = paste0(path_mnt_mtm7, '/mnt_mtm7.vrt')
gdalbuildvrt(gdalfile=list_file_mnt_mtm7, output.vrt= vrt_mnt_mtm7,overwrite=TRUE)


mnt_mtm7 = terra::rast(vrt_mnt_mtm7)
i = 1
crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84"
for (i in 1:list_file_mnt){
  mnt_file = rast(list_file_mnt[1])
  name_path = paste0(path_mnt, 'mnt_wgs84/',names(mnt_file), '.tif')
  mnt_file = project(mnt_file,crs)
  writeRaster(mnt_file,name_path, overwrite = T)
  
}

test = rast('C:/Meghana/Belgique/traitements/data/MNT/MNT_31H16NO.tif')



#######################################################################################
#Metric shadow of trees over the water (indice d'ombrage):
#Steps A1 through A7
################################################################################
#A) Create water surface data from UEA data 
#Step A1 : Extract from PtRef data only rows that have UEA that we are interested in

PtRef03_join = st_join(UREC_merge,PtRef03, by = c('Id_UEA' = 'Id_UEA')) #join shapefile PtRef with shapefile of UREC_merge use primary key (Id_UEA) --> create Join table
PtRef03_mod_lotique_join = dplyr::left_join(PtRef03_join,PtRef_mod_lotique03, by = c('Id_PtRef' = 'Id_PtRef')) #Join table PtRef_mod_lotique with Join table 
#do the same with the other gdb concerned (05)
PtRef05_join = st_join(UREC_merge,PtRef05, by = c('Id_UEA' = 'Id_UEA'))
PtRef05_mod_lotique_join = dplyr::left_join(PtRef05_join,PtRef_mod_lotique05, by = c('Id_PtRef' = 'Id_PtRef'))
#subset tables for columns we are interested in
PtRef03_mod_lotique_join = select(PtRef03_mod_lotique_join, c("Id_UEA.x","Id_rive", "width", "contF", "contG","sinuosity","Id_PtRef", "Largeur_mod"))
PtRef05_mod_lotique_join = select(PtRef05_mod_lotique_join, c("Id_UEA.x","Id_rive", "width", "contF", "contG","sinuosity","Id_PtRef", "Largeur_mod"))
PtRef_mod_lotique_join = rbind(PtRef03_mod_lotique_join,PtRef05_mod_lotique_join) #combine both Join tables (03 and 05) along the rows
PtRef_mod_lotique_join$Id_UEA = PtRef_mod_lotique_join$Id_UEA.x #rename primary key to Id_UEA
PtRef_mod_lotique_join = select(PtRef_mod_lotique_join, -c("Id_UEA.x")) #remove old column of primary key Id_UEA.x
PtRef_mod_lotique_join = subset(PtRef_mod_lotique_join, Largeur_mod != -999) #remove rows = -999 in column of interest (Largeur_mod)
PtRef_mod_lotique_join = subset(PtRef_mod_lotique_join, !is.na(Largeur_mod)) # remove rows with NA in column of interest (Largeur_mod)
largeur_riviere= PtRef_mod_lotique_join %>% dplyr::group_by(Id_UEA) %>% dplyr::summarize(river_width = mean(Largeur_mod)) #group table by Id_UEA and summarize the column Largeur_mod with median function
st_write(largeur_riviere, 'C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_largeur_riviere.shp', delete_layer = T) #write the table containing (Id_UEA and median river_width)
largeur_riviere = st_drop_geometry(largeur_riviere)
#Join table of median width grouped-by Id_UEA with the UEA table
UEA_larg_mod = dplyr::left_join(UEA,largeur_riviere, by=c('ID_UEA'='Id_UEA')) #choose correct Primary Key!!

#Step A2: Create buffer zone around each UEA with distance = median of largeur_mod_lotique.
#        This represents the surface of the water!
#For loop to create buffer around each unit which represents the surface of the water
path_name_buffer_largeur_mod = 'C:/Meghana/Belgique/traitements/results/surface_eau_mod/'
i = 1
for (i in 1:nrow(UEA_larg_mod)){
  river = UEA_larg_mod[i,]
  river_Id_UEA = river$ID_UEA
  dist = river$river_width
  buffer = st_buffer(river, dist)
  name_buffer = paste0(path_name_buffer_largeur_mod, river_Id_UEA, '.shp')
  st_write(buffer, name_buffer, delete_layer = T) #write the buffer shapefiles to a folder
}

#Step B : Create Hillshade DEMs in QGIS
#Load Hillshade in R
mns_shade7 = rast('C:/Meghana/Belgique/traitements/results/hillshading/hillshade_MNS/hillshade_mns7.sdat')
crs(mns_shade7) <- 'EPSG:2949'
mns_shade8 = rast('C:/Meghana/Belgique/traitements/results/hillshading/hillshade_MNS/hillshade_mns8.sdat')
crs(mns_shade8) <- 'EPSG:2950'
#Step C: read water surface polygones

path_name_buffer_largeur_mod #variable should already exist and leads to shp of calculate water surface
list_file_water_surface =  list.files(path_name_buffer_largeur_mod, pattern = '*.shp', full.names = T)
#Create water surface polygones + write out water surface intersect with UREC
i = 1
r = 2
for (i in 1:length(list_files_UREC_rives)){
  shp = vect(list_files_UREC_rives[i])
  Id_uea = shp$Id_UEA
  Id_uea
  Id_rive = shp$Id_rive
  Id_rive
  name = paste0(path_name_buffer_largeur_mod, 'UREC_water/', Id_uea, '_rive', Id_rive, '.shp' )
  name
  for (r in 1:length(list_file_water_surface)){
    water = vect(list_file_water_surface[r])
    water$ID_UEA
    if (shp$Id_UEA == water$ID_UEA){
      UREC_water = terra::intersect(water, shp)
      terra::writeVector(UREC_water, name, overwrite = T)
    }
  }
}

path_water = 'C:/Meghana/Belgique/traitements/results/surface_eau_mod/UREC_water/'
list_files_water = list.files(path_water, pattern = '*.shp', full.names = T)

l = 2
i =1
#load virtual mhc rasters per given projection
UREC_merge #should already exist from past creation
UREC_merge_v = vect(UREC_merge) #convert sf object into SpatVect object
#Convert UREC_merge_v to CRS of MHC7 and MHC8
UREC_v7 = terra::project(UREC_merge_v, 'epsg:2949') #MTM7
UREC_v8 = terra::project(UREC_merge_v,'epsg:2950' )

#Get extents of rasters in both projections
etendu_shade7 = ext(mns_shade7)#MHC7
etendu_shade8 = ext(mns_shade8) #MHC8
#Clip UREC_v 7 and 8 to extent of mhc7 and 8 
UREC_v7 = terra::crop(UREC_v7, etendu_shade7) #Cropped UREC_v7 to only include UREC that overlay the extent of raster mns_shade7
UREC_v8 = terra::crop(UREC_v8, etendu_shade8)
a = 3
l = 1
i=3
#for loop test on mns_shade7 EPSG : 2949
for (l in 1:nrow(UREC_v8)) {#change this depending on projection needed
  rive = UREC_v8[l] #changed this depending on projection needed
  Id_uea = rive$Id_UEA
  Id_uea
  Id_rive = rive$Id_rive
  Id_rive
  for (a in 1:length(list_files_UREC_rives)) {
    seg = vect(list_files_UREC_rives[a])
    Id_seg = seg$Id_UEA
    Id_seg
    Id_seg_rive = seg$Id_rive
    Id_seg_rive
    
    if ((Id_uea == Id_seg) & (Id_seg_rive == Id_rive)) {
      for (i in 1:length(list_files_water)) {
        water = vect(list_files_water[i])
        water_uea = water$ID_UEA
        water_uea
        water_rive = water$Id_rive
        water_rive
        water = terra::project(water, rive) #project water to same projection as rive (either MTM7 --> espg : 2949 OR MTM8 --> espg: 2950)
        if ((water_uea == Id_uea) & (water_rive == Id_rive)) {
          name = paste0(
            'C:/Meghana/Belgique/traitements/results/UREC_rives_new/',
            Id_uea,
            '_rive',
            Id_rive,
            '.shp'
          )
          name
          ombre = mns_shade8 #Needs to be changed depending on UREC projection (UREC_v7 or UREC_v8)
          ombre = terra::crop(ombre, water)
          water_raster = terra::rasterize(water,ombre)
          ombre <-
            terra::mask(ombre, water_raster)# masquer le raster ombre avec la surface de l'eau
          # normaliser et inverser :
          #ombre <- ombre / 90
          #ombre <- abs(ombre - 1)
          IndOmbrage = median(values(ombre), na.rm = T)
          seg$IndOmbrage = IndOmbrage
          terra::writeVector(seg, name, overwrite = T)
          print(paste0('writing file UEA ', Id_seg, ' rive ',Id_seg_rive) )
        }
      }
    }
  }
}

################################################################################
  #Metric Surface de la canopee en surplomb sur la riviere
  ################################################################################
  #three methods shoudl be tests : 1. MHC, 2. WorldCover, 3. MHC + WorldCover (forest mask)
  


#Method 1 : MHC
################################################################################
#Intersect UREC with water surface 
#Create list of files for water surface per UREC



#load virtual mhc rasters per given projection
UREC_merge #should already exist from past creation
UREC_merge_v = vect(UREC_merge) #convert sf object into SpatVect object
mhc7 = rast('C:/Meghana/Belgique/traitements/data/MHC/mtm7/mhc_mtm7.vrt') 
mhc8 = rast('C:/Meghana/Belgique/traitements/data/MHC/mtm8/mhc_mtm8.vrt')

#Convert UREC_merge_v to CRS of MHC7 and MHC8
UREC_v7 = terra::project(UREC_merge_v, 'epsg:2949') #MTM7
UREC_v8 = terra::project(UREC_merge_v,'epsg:2950' ) #MTM8

#Get extents of SpatRaster mhc
etendu7 = ext(mhc7)#MHC7
etendu8 = ext(mhc8) #MHC8

#Clip UREC_v 7 and 8 to extent of mhc7 and 8 
UREC_v7 = terra::crop(UREC_v7, etendu7) #Cropped UREC_v7 to only include UREC that overlay the extent of raster mhc7
UREC_v8 = terra::crop(UREC_v8, etendu8)

#Extract surface of canopee with mhc_v7 and mhc_v8
###For loop to extract the area of canopy over the river for each UREC. #######
#Some variables need to be changed!!!!!
################################################################################
l = 1
i = 1
r =1
#iterate through features in UREC_merge (iterate through every UREC)
for (l in 1:nrow(UREC_v7)) { #Needs to be changed depending on which mhc you are looking at!
  rive = UREC_v7[l] #Read UREC AND needs to be changed 
  Id_seg = rive$Id_UEA
  Id_seg
  Id_side = rive$Id_rive
  Id_side
  name = paste0(
    path_name_UREC_rives,
    Id_seg,
    '_rive',
    Id_side,
    '.shp'
  )
  name
#Iterate through sampling files for each UREC
  for (i in 1:length(list_files_sampling_rive)) {
    shp = vect(list_files_sampling_rive[i]) #Read sampling shp for UREC
    shp = terra::project(shp, rive) #needs to be changed depending on projection
    shp_sf = st_read(list_files_sampling_rive[i]) #read vector file as sf object
    shp_sf = st_transform(shp_sf, crs = st_crs(shp)) #convert sf vector ro MTM7 coordinate system
    Id_uea = shp$ID_UEA[1]
    Id_uea
    Id_rive = shp$Id_rive[1]
    Id_rive
    #Match UREC feature with sampling feature
    if (Id_seg == Id_uea & Id_side == Id_rive) {
      #Iterate through water surface files for each UREC
      for (r in 1:length(list_file_water_surface)) {
        water = vect(list_file_water_surface[r]) #Read water surface polygon 
        Id_water = water$ID_UEA
        Id_water
        water = terra::project(water, rive) #needs to be changed depending on projection
        water_sf = st_read(list_file_water_surface[r]) #read water surface polygon as sf object
        water_sf = st_transform(water_sf, crs = st_crs(water)) #transform vector sf object to MTM7 projection
        print(paste0('working on file ', rive$Id_UEA, ' rive ', rive$Id_rive))
        print(Id_uea == Id_water)
         #match water surface to sampling shp       
         if (Id_uea == Id_water) {
          sampling_water = terra::intersect(water, shp)# extract polygon intersection between sampling and water surface (sf object)
          sampling_water_sf = st_intersection(water_sf, shp_sf)#convert SpatVect to sf object
          
          #Reduce the extent of the MHC raster to the extent of UREC
          mhc_clip7 = terra::crop(mhc7, sampling_water) #Needs to be changed depending on which extent we are using
          sampling_water_raster = terra::rasterize(sampling_water, mhc_clip7, 'ID_UEA' ) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
          mhc_mask = terra::mask(mhc_clip7, sampling_water_raster)
          mhc_surf = mhc_mask
          mhc_surf[mhc_surf<3] <- 0 # replace what is not forest by 0 (a tree has to be taller than 3m height)
          mhc_surf[mhc_surf>= 3] <- 1 #replace all values >= 3 to 1 (all cells with trees are given the same value)
          
          sampling_water$srfCan =  exactextractr::exact_extract(mhc_surf, sampling_water_sf, 'sum')#extract the area of canopy per sampling unit
          median_srfCan = median(sampling_water$srfCan, na.rm = T) #Take the median of the canopy surface over the sampling units 
          rive$srfCan_m2 = median_srfCan #add a column to the UREC feature (from UREC_merge)
          rive = terra::project(rive, 'epsg:32198') #change to coordinate system back to NAD83 Qc Lambert
          rive$Id_UEA 
          rive$Id_rive
          crs(rive)
          print(paste0('writting file ', rive$Id_UEA, ' rive ', rive$Id_rive))
          print(names(rive))
          writeVector(rive, name, overwrite = T) #Write the new UREC_files seperately into UREC_rive_new folder
          
        }
      }
    }
  }
  
}

################################################################################













###############################################################################
#code to access SAGA hillsahding directly from R. It doesn't work yet. 
###############################################################################
dem = 'C:/Meghana/Belgique/traitements/data/MNC/MHC_merge.tif'
output = 'C:/Meghana/Belgique/traitements/data/MNC/shademap.sdat'
unit = 1
method = 1
date = 2019 - 06 - 21
time = 12
position = 1

system(paste("C:/Fichiers de programme (x86)/SAGA-GIS/saga_cmd.exe -f=p","ta_lighting 0","-ELEVATION",dem,"-SHADE",output,"-METHOD",method,"-POSITION", position,"-UNIT",unit,
             "-DATE",date,"-TIME",time))

rsaga.hillshade(in.dem, out.grid, method = method, azimuth = 315,
                declination = 45, exaggeration = 4)
###############################################################################################
#Step 4: Create hillshade in SAGA (possible to do this from R)

#To do crop from Qgis do raster:crop _ raster:: mask
#work with sqlite! raster virtuelle ! do with Gdal outils for vrt function
#raster = terra = stars
################################################
################################################
################################################
# # pr?paration param?tre ? mettre dans SAGA

dem = raster('C:/Meghana/Belgique/traitements/data/MNT/MNT_21E13NE.tif')
output = 'C:/Meghana/Belgique/traitements/data/MNT/MNT_21E13NE_hillshade.sdat'
unit = 1
method = 1
date = 2019 - 06 - 21
time = 12
position = 1
 




# # cmd saga
#cat() for debugging system string
system(paste("C:/Program Files (x86)/SAGA-GISsaga_cmd.exe -f=p","ta_lighting 0","-ELEVATION",dem,"-SHADE",output,"-METHOD",method,"-POSITION", position,"-UNIT",unit,
              "-DATE",date,"-TIME",time))

shade_map = raster('C:/Meghana/Belgique/traitements/results/hillshading/MHC_merge_hillshading.sdat')

eau= st_read('C:/Meghana/Belgique/traitements/results/surface_eau_mod/dbd4da519b274d9f98b1159e68bcb78c.shp')
eau = st_transform(eau, crs  = 2949)
UREC_rive1 = st_read('C:/Meghana/Belgique/traitements/results/UREC_rives/27917246329c4218a2cf6562ac89d22e_rive13.shp')
UREC_rive1 = st_transform(UREC_rive1,crs  = 2949)
UREC_rive2 = st_read('C:/Meghana/Belgique/traitements/results/UREC_rives/27917246329c4218a2cf6562ac89d22e_rive14.shp')
UREC_rive2 = st_transform(UREC_rive2,crs  = 2949)

eau_rive1 = st_intersection(eau, UREC_rive1)
eau_rive2 = st_intersection(eau, UREC_rive2)


shade_map_clip = raster('C:/Meghana/Belgique/traitements/results/hillshading/hillshade_clip.sdat')
projectRaster(shade_map_clip, crs = 2949)
shade_map_mask = mask(shade_map_clip, UREC)
shade_map_mask = shade_map_mask/90
shade_map_mask = abs(shade_map_mask - 1)


shade_eau_rive1 = mask(shade_map_mask, eau_rive1)
shade_eau_rive2 = mask(shade_map_mask, eau_rive2)

shade1 = median(values(shade_eau_rive1), na.rm = T)
shade2 = median(values(shade_eau_rive2), na.rm = T)

UREC_rive1$shade = shade1
UREC_rive2$shade = shade2
st_write(UREC_rive2,'C:/Meghana/Belgique/traitements/results/hillshading/tests/UREC_rive2.shp', overwrite = T)




















########################################################################################
# STUFF THAT IS NOT FINISHED YET
########################################################################################

#Metric : Hauteur emerge au pied de berge
mnt1 = raster('C:/Meghana/Belgique/donnees_brutes/LiDAR/MNT_21E13NE.tif')

path_mnt = paste0(getwd(), '/donnees_brutes/LiDAR')
list_file = list.files(path_mnt, pattern = '*.tif', full.names = T)
mnt = stack(list_file)
