#Script to calculate IQBR 
###############################################################################
#load libraries
library(terra)
library(raster)
#load correspondance entre IQBR et classes de UT 2018 10m
IQBR_UT_2018_correspondence <- read.csv("C:/Meghana/donnee_brutes/IQBR_UT_2018_correspondence.csv", sep=";")
#Do a test with 2019 10m resolution Utilisation du territoire
IQBR_UT_2019_10m_Correspondence <- read.csv("C:/Meghana/donnee_brutes/IQBR_UT_2019_10m_Correspondence.csv", sep=";")
IQBR_UT_2019_10m_Correspondence$CODE_UT = as.numeric(IQBR_UT_2019_10m_Correspondence$CODE_UT)
#load Utilisation du territoire raster data
ut = terra::rast('C:/Meghana/donnee_brutes/UTILISATION_TERRITOIRE/utilisation_territoire_2019/utilisation_territoire_2019/utilisation_territoire_2019.tif')
#load UREC_merge data (to get extent)
UREC_merge = st_read('C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_new.shp')
IQBR = UREC_merge[,c('Id_UEA', 'Id_rive')]
IQBR_19 = UREC_merge[,c('Id_UEA', 'Id_rive')]

#UT clip should already be savec in your files. Otherwise create this file
#ut_clip = terra::crop(ut, st_ext(UREC_merge))
#Load UT_clip with raster
path_ut_clip = 'C:/Meghana/Belgique/traitement/data/UTILISATION_TERRITOIRE_2018_10m/test.tif'
ut_clip_r = raster::raster('C:/Meghana/Belgique/traitements/data/UTILISATION_TERRITOIRE_2018_10m/Utilisation_territoire_2018_10m_clip.tif')

path_ut19_clip = 'C:/Meghana/Belgique/traitement/data/UTILISATION_TERRITOIRE_2018_10m/test.tif'
ut19_clip_r = raster::raster('C:/Meghana/Belgique/traitements/data/UTILISATION_TERRITOIRE_2019_10m/utilisation_territoire_2019_10m_clip.tif')

#Crop Utilisation du territoire to the extent of UREC_merge
#IQBR = project(IQBR, ut)
IQBR = st_transform(IQBR, st_crs(ut_clip_r))






#Do for loop to get UT data per UREC_rive file, save this in a folder
r = 5

for (r in 1:nrow(IQBR_19)) {
  shp = IQBR_19[r,]

  shp = st_transform(shp, crs = st_crs(ut19_clip_r)) # project vector UREC file to same projection as ESA raster file (WGS84)
  ut_shp = terra::crop(ut19_clip_r, shp) #Reduce the extent of the ESA raster to the extent of UREC
  shp_raster = terra::rasterize(shp, ut_shp) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
  ut_mask = terra::mask(ut_shp, shp_raster)
      
   #Calculate IQBR for each masked raster area
  #Get frequency of each class in the raster 
  freq_dist = freq(ut_mask)
  freq_dist= as.data.frame(freq_dist)
  #freq_dist$value = as.data.frame.integer(freq_dist$value) 
 # join_tbl = left_join(freq_dist, IQBR_UT_2018_correspondence, c('value'='Value')) #raster needs to be loaded with raster library to work 
  join_tbl = left_join(freq_dist, IQBR_UT_2019_10m_Correspondence, c('value'= 'CODE_UT'))
  join_tbl= na.omit(join_tbl) #remove NA from table
  total_pix = sum(join_tbl$count) #Count the number of pixels to get proportional coverage
  join_tbl$perc_coverage = (join_tbl$count/ total_pix)*100
  join_tbl$IQBR = join_tbl$perc_coverage*join_tbl$Poids_IQBR
  indice_IQBR = sum(join_tbl$IQBR)/10
  
  #shp$IQBR = indice_IQBR
  IQBR_19[r,'IQBR'] = indice_IQBR

  

}

IQBR_19 = st_make_valid(IQBR_19)
IQBR_19 = st_cast(IQBR_19, to ='POLYGON')
st_is_valid(IQBR_19)
IQBR_19$id = paste0(IQBR_19$Id_UEA, '_rive', IQBR_19$Id_rive)
st_write(IQBR_19, dsn = 'C:/Meghana/Belgique/traitements/results/UREC_IQBR/', layer = 'UREC_merge_IQBR_19_new.shp',driver = 'ESRI Shapefile', delete_dsn = T)

#Compare IQBR 2018 and IQBR_20019 
plot(IQBR$IQBR, IQBR_19$IQBR)

#################################################################################################################################

# CREATING IQBR WITH PASL DATA 
#Load vector file of PASL cut to UREC_merge_IQBR (done in ArcGIS)
PASL = st_read('C:/Meghana/Belgique/traitements/data/IQBR/PASL_merge.shp')
PASL = as.data.frame(PASL)
#Load table of correspondence between IQBR and PASL classiication
correspondance_PASL_IQBR <- read.csv2("C:/Meghana/Belgique/traitements/data/IQBR/correspondance_PASL_IQBR.csv")
correspondance_PASL_IQBR = correspondance_PASL_IQBR %>% select("IQBR",  "Poids_IQBR", "PASL_ID" )
j_PASL_IQBR = left_join(PASL, correspondance_PASL_IQBR, by =c('PASL_ID' = 'PASL_ID'))
spat_j = st_join(IQBR, PASL)
IQBR_PASL = as.data.frame(spat_j)


IQBR_PASL = IQBR_PASL %>% select("Id_UEA","Id_rive", "IQBR","id", "PASL_ID",'Shape_Area')
poids_PASL_IQBR = j_PASL_IQBR %>% select("PASL_ID", "Poids_IQBR")

df_IQBR_PALS = left_join(poids_PASL_IQBR, IQBR_PASL, by = c('PASL_ID' = 'PASL_ID') )
surf_area = df_IQBR_PALS %>% group_by(id) %>% summarise(surface_area = sum(Shape_Area))

r = 62
l=1
for (r in 1:nrow(df_IQBR_PALS)){
  row = df_IQBR_PALS[r,]
  for(l in 1:nrow(surf_area)){
    line = surf_area[l,]
    
    if (row$id == line$id){
      
      surf_total = line$surface_area
      df_IQBR_PALS[r,'surf_total'] = surf_total
      next
    }
  }
}
df_IQBR_PALS$surf_prop = (df_IQBR_PALS$Shape_Area/df_IQBR_PALS$surf_total)*100
df_IQBR_PALS$IQBR_PASL = df_IQBR_PALS$surf_prop*df_IQBR_PALS$Poids_IQBR
df_IQBR_PALS = na.omit(df_IQBR_PALS)
df_PASL_IQBR = df_IQBR_PALS %>% group_by(id) %>% summarise(indice_PASL_IQBR = sum(IQBR_PASL/10)) 

write.table(df_PASL_IQBR, 'C:/Meghana/Belgique/traitements/data/IQBR/PASL_IQBR_tbl.csv')

##############################################################################
# Writing df_PASL to shapefile in folder
df_UREC_norm_contH = vect(UREC_norm_contH)
df_UREC_norm_contH$geoX = geom(df_UREC_norm_contH)[,3]
df_UREC_norm_contH$geoY = geom(df_UREC_norm_contH)[,4]
df_UREC_norm_contH= as.data.frame(df_UREC_norm_contH)
df_UREC_norm_contH = left_join(df_UREC_norm_contH, df_PASL_IQBR, by =c('id'='id'))
df_UREC_norm_contH = df_UREC_norm_contH %>% select(id, id_uea, id_rive,indiceContH, geoX, geoY, indice_PASL_IQBR, indice_PASL_IQBR_nrm )
test = st_as_sf(df_UREC_norm_contH, coords = c(df_UREC_norm_contH$geoX,df_UREC_norm_contH$geoY))
PASL_shp_vect = terra::geom(df_UREC_norm_contH)
PASL_shp = st_set_geometry(df_UREC_norm_contH, df_UREC_norm_contH$geo)
PASL_shp = st_cast(PASL_shp, to = 'MULTIPOLYGON')
st_write(PASL_shp, dsn = 'C:/Meghana/Belgique/traitements/data/IQBR', layer = 'PASL_IQBR_test1.shp', driver = 'ESRI Shapefile', delete_dsn = T )
st_write(PASL_shp, 'C:/Meghana/Belgique/traitements/data/IQBR/PASL_IQBR_test.shp')


st_write(PASL_shp, dsn = 'C:/Meghana/Belgique/traitements/data/IQBR/', layer = 'PASL_IQBR_tbl.SQLite', driver = 'SQLite', delete_dsn = T )

#Check difference between IQBR calculated with 2019 UT 10m and PASL 
IQBR_19$id = paste0(IQBR_19$Id_UEA, '_rive', IQBR_19$Id_rive)
IQBR_19_PASL = st_join(IQBR_19, PASL_shp, by = c('id'='id') )
IQBR_19_PASL$diff_indIQBR = IQBR_19_PASL$indice_PASL_IQBR-IQBR_19_PASL$IQBR
