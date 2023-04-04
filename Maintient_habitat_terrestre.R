############# Metrics linked to 'Maintain de l'habitat et creation d'habitat####################

#Statistics des arbres#
ut19_Urbain_vec = vect('C:/Meghana/Belgique/decembre/data/polygon_UT_urban.shp')
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
UREC_merge = vect(UREC_merge)
vrt_mhc_mask7 = rast('C:/Meghana/Belgique/decembre/traitements/MHC_mask/mhc7_mask.vrt')
vrt_mhc_mask8 = rast('C:/Meghana/Belgique/decembre/traitements/MHC_mask/MTM8/MTM8.vrt')
raster_file = vrt_mhc_mask8

EPSG= 'EPSG:2950'
urban_vector_mask = ut19_Urbain_vec
exent7 = ext(raster_file)
UREC_merge_v7 = UREC_merge
UREC_merge_v7 = terra::project(UREC_merge, crs(raster_file))
UREC_merge_v7 = raster::crop(UREC_merge_v7, exent7)
UREC_merge = UREC_merge_v7



######################################
i = 1

test = TreeStats(UREC_merge = UREC_merge_v7, raster_file = raster_file, EPSG = EPSG, urban_vector_mask = urban_vector_mask)
writeVector(test, 'C:/Meghana/Belgique/decembre/traitements/treeStats2.sqlite', filetype = 'SQLITE')

#NEW runs of treeStats
UREC_merge_mtm7 = vect('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_MTM7.shp')
tree_stats_mtm7 = TreeStats(UREC_merge = UREC_merge_mtm7, raster_file = vrt_mhc_mask7, EPSG = EPSG, urban_vector_mask = urban_vector_mask)

#NEW runs of treeStats
UREC_merge_mtm8 = vect('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_MTM8.shp')
mhc_list_files = list.files('C:/Meghana/Belgique/decembre/traitements/MHC_mask/MTM8', pattern = '*.tif')
mhc_output = 'C:/Meghana/Belgique/decembre/traitements/MHC_mask/MTM8/mhc_mtm8_new.vrt'

terra::vrt(mhc_list_files, mhc_output)

gdal.BuildVRT(mhc_output, mhc_list_files, options=vrt_options)


vrt_mhc_mask8
UREC_merge_mtm8= UREC_merge_mtm8[1,]
tree_stats_mtm8 = TreeStats(UREC_merge = UREC_merge_mtm8, raster_file = vrt_mhc_mask8, EPSG = EPSG, urban_vector_mask = urban_vector_mask)

tree_stats = rbind(tree_stats_mtm7,tree_stats_mtm8)
writeVector(tree_stats,'C:/Meghana/Belgique/decembre/traitements/treeStats_full.sqlite', filetype = 'SQLITE' )

c = tree_stats[11:12,]



###################################
#testing st_crop because terra_crop doesn't work 
UREC_merge = st_read('C:/Meghana/Belgique/decembre/traitements/UREC_maintienHabT1.shp')
UREC_merge = st_transform(UREC_merge, crs(raster_file))
UREC_merge = st_crop(UREC_merge, exent7)
UREC_merge = vect(UREC_merge)#re transform into spatvector
####Ratio de la surface de la canopee et des surfaces vegetale sur l'aire totale de l'UREC####
###Canopee ratio
i = 134
CanopyRatio <- function(UREC_merge, raster_file, EPSG,  urban_vector_mask){
  #UREC_merge : spatVector file of feature surface per UREC that has been reprojected to match raster file projetion and clipped to extent of raster_file (see main)
  #raster_file : raster file (or vrt) of MHC mosaic split based on projection (either MTM7 or MTM8)
  #EPSG : Character string of projection. example : "EPSG: 2949"
  #Urban_vector_mask : Spactvector file of vecotirized urban areas at same resolution as raster_file
  UREC_merge$CanopyRatio = NA


  for (i in 1:nrow(UREC_merge)){
    feature =  UREC_merge[i,]
    print(paste0('processing UREC : ', feature$id))
    features_sf = st_as_sf(feature) #load polygon as sf object to be used in exact extract
    features_sf = st_transform(features_sf, st_crs(raster_file))
    vrt_mhc_mask7_clip = terra::crop(raster_file, features_sf)
    vrt_mhc_mask7_clip[vrt_mhc_mask7_clip<1.5]<-NA
    feature_raster = terra::rasterize(features_sf, vrt_mhc_mask7_clip)
    vrt_mhc_mask7_feature = terra::mask(vrt_mhc_mask7_clip, feature_raster)
    porj_urbain = terra::project(urban_vector_mask, EPSG)
    proj_urbain_sf = st_as_sf(porj_urbain)
    vrt_mhc_mask7_feature_urbain = terra::mask(vrt_mhc_mask7_feature, proj_urbain_sf, inverse = T)
    
    feature$CanopySurface =as.numeric(exactextractr::exact_extract(vrt_mhc_mask7_feature_urbain, features_sf, 'count'))
    feature$area = as.numeric(st_area(features_sf))
    feature$CanopyRatio = feature$CanopySurface/feature$area 
    
    UREC_merge[i,]$CanopyRatio = feature$CanopyRatio

    
    
  }
  return(UREC_merge)
}

testCanRatio8 = CanopyRatio(UREC_merge = UREC_merge, raster_file = raster_file, EPSG = EPSG, urban_vector_mask= urban_vector_mask)
writeVector(testCanRatio8, 'C:/Meghana/Belgique/decembre/traitements/CanRatio8.sqlite', filetype = 'SQLITE', overwrite = TRUE)

testCanRatio7 = CanopyRatio(UREC_merge = UREC_merge, raster_file = raster_file, EPSG = EPSG, urban_vector_mask= urban_vector_mask)
#Select columns we are interested in 
testCanRatio7 = testCanRatio7[,c("id_uea","rive","id", "meanheight", "medianheig","sdheight","majorityhe","CanopyRatio")]
writeVector(testCanRatio7, 'C:/Meghana/Belgique/decembre/traitements/CanRatio7.sqlite', filetype = 'SQLITE', overwrite = TRUE)

#New tests Canopy Ratio
test2CanRatio8 = CanopyRatio(UREC_merge = UREC_merge_mtm8, raster_file = vrt_mhc_mask8, EPSG = EPSG, urban_vector_mask= urban_vector_mask)
test2CanRatio7 = CanopyRatio(UREC_merge = UREC_merge_mtm7, raster_file = vrt_mhc_mask7, EPSG = 'EPSG:2949', urban_vector_mask= urban_vector_mask)
test2CanRatio = rbind(test2CanRatio7,test2CanRatio8)



UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')

#Combine testCanRatio7 and testCanRatio8
testCanRatio7 = st_read('C:/Meghana/Belgique/decembre/traitements/CanRatio7.sqlite')
testCanRatio7 = testCanRatio7[,c("id_uea","rive","id","canopyratio")] #Select only columns we are interested in
testCanRatio7 = st_transform(testCanRatio7, 'EPSG:32198') #convert projection to common projection (WGS84)
testCanRatio8 = st_read('C:/Meghana/Belgique/decembre/traitements/CanRatio8.sqlite')#names are the same as testCanRatio7
testCanRatio8 = st_transform(testCanRatio8, 'EPSG:32198') #convert projection to common projection (WGS84)
#Combine testCanRatio7 and 8 
testCanRatio = rbind(testCanRatio7, testCanRatio8)
st_write(testCanRatio,'C:/Meghana/Belgique/decembre/traitements/testCanRatio.sqlite' )
#test that all ids are unique
n_occur = data.frame(table(testCanRatio$id))
doubles = n_occur[n_occur$Freq>1,]

#TODO : figure out why you get different results for the same UREC. They seem to be a cheval on two mhc tiles in two different projections. 
#TODO : Write out extents of both VRTs to see which UREC are cut by the extents


##########################
#Ratio de la surface vegetatio sur l'aire totale de l'UREC
#############################
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')

raster_file = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_Veg.tif')
i=1
VegetationRatio <- function(UREC_merge, raster_file){
  #UREC_merge : spatVector file of feature surface per UREC that has been reprojected to match raster file projetion and clipped to extent of raster_file (see main)
  #raster_file : raster file (or vrt) of MHC mosaic split based on projection (either MTM7 or MTM8)
  #EPSG : Character string of projection. example : "EPSG: 2949"
  #Urban_vector_mask : Spactvector file of vecotirized urban areas at same resolution as raster_file
  UREC_merge$VegRatio = NA
  
  
  for (i in 1:nrow(UREC_merge)){
    feature =  UREC_merge[i,]
    print(paste0('processing UREC : ', feature$id))
    features_sf = st_as_sf(feature) #load polygon as sf object to be used in exact extract
    features_sf = st_transform(features_sf, st_crs(raster_file))
    raster_file_clip = terra::crop(raster_file, features_sf)
    #vrt_mhc_mask7_clip[vrt_mhc_mask7_clip<1.5]<-NA
    feature_raster = terra::rasterize(features_sf, raster_file_clip)
    vrt_mhc_mask7_feature = terra::mask(raster_file_clip, feature_raster)
   # porj_urbain = terra::project(urban_vector_mask, EPSG)
  #  proj_urbain_sf = st_as_sf(porj_urbain)
   # vrt_mhc_mask7_feature_urbain = terra::mask(vrt_mhc_mask7_feature, proj_urbain_sf, inverse = T)
    
    feature$VegSurface =as.numeric(exactextractr::exact_extract(vrt_mhc_mask7_feature, features_sf, 'count'))*100
    feature$area = as.numeric(st_area(features_sf))
    feature$VegRatio = feature$VegSurface/feature$area 
    
    UREC_merge[i,]$VegRatio = feature$VegRatio
    
    
    
  }
  return(UREC_merge)
}
testVegRatio = VegetationRatio(UREC_merge = UREC_merge, raster_file = raster_file)
st_write(testVegRatio,'C:/Meghana/Belgique/decembre/traitements/VegRatio.sqlite')

############################
#Number of classes in an UREC
UREC_merge = testVegRatio #I want to add a column to UREC_merge with new columns calculated
raster_file = rast('C:/Meghana/Belgique/decembre/data/raster/ut19_mask.tif')
i=1
testNbrClass = NumberClasses(UREC_merge = UREC_merge, raster_file=raster_file)
st_write(testNbrClass,'C:/Meghana/Belgique/decembre/traitements/Nbr_class.sqlite' )






############################
#Surface des differentes classes(main classes)
ut19_classes <- read.csv("C:/Meghana/donnee_brutes/UT_2019_10m.csv", sep=";")
ut19_classes$CODE_UT = as.numeric(ut19_classes$CODE_UT)
ut19_classes = ut19_classes[,-1] # On supprime la 1ere colonne 'OID_'
ut19_groups = ut19_classes %>% group_by(DESC_CAT)
UREC_merge = st_read('C:/Meghana/Belgique/decembre/traitements/Nbr_class.sqlite')

raster_file = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_mask.tif')
csv_class_correspondence = ut19_groups
t= csv_class_correspondence %>% tally()

UREC_merge_surfaceClass = SurfaceClass(UREC_merge=UREC_merge, csv_class_correspondence = csv_class_correspondence, raster_file = raster_file)
st_write(UREC_merge_surfaceClass, 'C:/Meghana/Belgique/decembre/traitements/SurfaceClass.sqlite')

##############################




###Tests on similar id ###
testCanRatio_df = as.data.frame(testCanRatio)
testCanRatio_df = subset(testCanRatio_df, select = c(id,CanopyRatio)) #subset to columns you are interested in 
testCanRatio7_df$CanopyRatio7 =testCanRatio7_df$CanopyRatio
testCanRatio7_df =as.data.frame(testCanRatio7_df)
testCanRatio7_df = subset(testCanRatio7_df, select = c(id, CanopyRatio7))

UREC_merge1 = left_join(UREC_merge, testCanRatio_df)
UREC_merge1 = left_join(UREC_merge, testCanRatio7_df, by = c('id' = 'id'))

test = rbind(testCanRatio7_df, testCanRatio_df)
n_occur = data.frame(table(test$id))
doubles = n_occur[n_occur$Freq>1,]


y = filter(test, id =='c3de5088459e45ae97d4042a8591de72_rive9' )



#############################
#Urec average width

UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
UEA_merge = st_read('C:/Meghana/Belgique/decembre/data/UEA_merge.shp')
uea = UEA_merge[1,]
uea1 = st_cast(uea, to = 'LINESTRING')
semis = st_line_sample(uea1, density = 0.02, type = 'regular') 
voronoi = st_voronoi(semis)%>% st_cast()
voronoi = st_cast(voronoi, to = 'LINESTRING')
polygon1 = UREC_merge[18,] %>% st_cast(t0 = 'POLYGONE')
voronoi1 = st_intersection(voronoi,polygon1 )


  


UREC_merge_width = LateralWidthFunction(UREC_merge = UREC_merge, UEA_merge = UEA_merge)


           