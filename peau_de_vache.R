#Algorithme Peau de vache
install.packages('spatialEco')
library(raster)
library(terra)
library(spatialEco)
library(landscapemetrics)


#######################################################################################
extent = st_read('C:/Meghana/Peau_de_vache/emprise3.shp')
poly_voronoi = st_read('C:/Meghana/Peau_de_vache/voronoi2.shp')
extent_poly = extent(poly_voronoi)
crs(extent_poly)<- 'EPSG:32198'
peau1 = as.polygons(extent_poly)
peau1_sub = terra::intersect(peau1, poly_voronoi)


raster_clip = raster('C:/Meghana/Peau_de_vache/raster3.tif')
raster_clip= terra::crop(raster_clip, poly_voronoi) #Reduce the extent of the ESA raster to the extent of 
#########################################
#new set of rasters
rast_clip1 = raster_clip 
crs(rast_clip1) <- 'EPSG:32198' 
rast_clip1[1:38,1:273]<-1
plot(rast_clip1, col = 'dark green')

#new set of rasters
rast_clip2 = raster_clip 
crs(rast_clip2) <- 'EPSG:32198' 
rast_clip2[1:38,1:273]<-0
plot(rast_clip2, col = 'grey')


#new set of rasters
rast_clip3 = rast_clip1 
crs(rast_clip3) <- 'EPSG:32198' 
rast_clip3[1:19,]<-0
writeRaster(rast_clip3, 'C:/Meghana/Peau_de_vache/rasters/raster_clip3.tif')
plot(rast_clip3)


rast_clip4 = rast_clip1 
crs(rast_clip4) <- 'EPSG:32198' 
rast_clip4[20:38,]<-0
plot(rast_clip4 )

rast_clip5 = rast_clip1 
crs(rast_clip5) <- 'EPSG:32198' 
rast_clip5[,1:136]<-0
plot(rast_clip5)

rast_clip6 = rast_clip1 
crs(rast_clip6) <- 'EPSG:32198' 
rast_clip6[,137:273]<-0
plot(rast_clip6)

rast_clip7 = rast_clip1 
crs(rast_clip7) <- 'EPSG:32198' 
rast_clip7[1:28,]<-0 #bande vegetalise de 100m (10pixel de large)
plot(rast_clip7)

rast_clip8 = rast_clip1 
crs(rast_clip8) <- 'EPSG:32198' 
rast_clip8[1:38,1:273]<-sample(0:1,10374, replace =T)
plot(rast_clip8)

##################################################################

#################################################################################
#Create function for specific classes 

i=1
PDV_continuity_metric <-
  function(UREC_full,
           UREC_sampling,
           raster_file,
           classe_UT) {
    #UREC_full = shapefile of UREC (UREC_merge) as sf data.frame
    #raster_UT = raster of Utilisation du territoire masked for values we are interested in. e.g.  <Forest> (need to do this step beforehand , in ArcMap sometimes)
    # var 3  = character string of name of colum referencing the specific UT selection eg. "Forest"
    
    new_col_name = paste0('continuity_', classe_UT)
    sample = UREC_sampling
    sample = st_transform(sample, crs(raster_file))
    raster_file[raster_file != 1] <- NA
   #Extract the surface of forested/ grassland/urban/agriculture pixels in each UREC to get other metric
    # raster_converage = coverage_fraction(raster_file, sample )
    sample$surface_full = exactextractr::exact_extract(raster_file, sample, 'count') *
      100 #multiply sum by area of each pixel (meed to do this with sf object!)
    
    sample$area = as.numeric(st_area(sample)) #calculate the area of each sampling unit in the UREC
    sample$prop = sample$surface_full / sample$area # get the ratio of forested area in each sampling unit by the area of the sampling unit
    
    continuity = median(sample$prop) #create a variable of the median for continuity of forested areas
    
    UREC_full[1, new_col_name] = continuity
    return(UREC_full)
}


##################################################
#run functions
extent = PDV_continuity_metric(
  UREC_full = extent,
  UREC_sampling = poly_voronoi,
  raster_file = rast_clip1,
  classe_UT = 'Forest1'
)

extent=  PDV_continuity_metric(
  UREC_full = extent,
  UREC_sampling = poly_voronoi,
  raster_file =  rast_clip2,
  classe_UT = 'Forest2'
)

extent = PDV_continuity_metric(
  UREC_full = extent,
  UREC_sampling = poly_voronoi,
  raster_file = rast_clip3,
  classe_UT = 'Forest3'
)

extent =  PDV_continuity_metric(
  UREC_full = extent,
  UREC_sampling = poly_voronoi,
  raster_file = rast_clip4,
  classe_UT = 'Forest4'
)

extent= PDV_continuity_metric(
  UREC_full = extent,
  UREC_sampling = poly_voronoi,
  raster_file =  rast_clip5,
  classe_UT = 'Forest5'
)


extent = PDV_continuity_metric(
  UREC_full = extent,
  UREC_sampling = poly_voronoi,
  raster_file = rast_clip6,
  classe_UT = 'Forest6'
)
extent = PDV_continuity_metric(
  UREC_full = extent,
  UREC_sampling = poly_voronoi,
  raster_file = rast_clip7,
  classe_UT = 'Forest7'
)

extent = PDV_continuity_metric(
  UREC_full = extent,
  UREC_sampling = poly_voronoi,
  raster_file = rast_clip8,
  classe_UT = 'Forest8'
)



###########################################################

PDV_fragementation_metric_function <- function(UREC_full, raster_file, col_name) {
  #UREC_full = shapefile of UREC (UREC_merge) as sf data.frame
  #raster_file = raster_file of Utilisation du territoire masked for values we are interested in. e.g.  <Forest> (need to do this step beforehand , in ArcMap sometimes)
  # var 3  = character string of name of colum referencing the specific UT selection eg. "Forest" 
  area_new_col = paste0('area_', col_name)
  pd_new_col = paste0('pd_', col_name)
  raster_file[raster_file == 0] <- NA

  for (i in 1:nrow(UREC_full)) {
    UREC = UREC_full[i,]
    UREC = st_transform(UREC, st_crs(raster_file))
    ut_urec = terra::crop(raster_file, UREC)
    urec_raster = terra::rasterize(UREC, ut_urec) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
    ut_mask = terra::mask(ut_urec, urec_raster)
    
    check = check_landscape(ut_mask, verbose = T)
    if (check$OK == "✖") {
      area_mn = 0   #Needs to be changed
      pd = NA        #Needs to be changed
      
    } else {
      #Calculate landscape metrics
      
      area_mn =  lsm_l_area_mn(ut_mask)
      area_mn = area_mn$value     #Mean area of patches
      pd = lsm_l_pd(ut_mask)
      pd = pd$value               #Patch Density
      
      #Add values to UREC table based on primary key
      
      # UREC$area_mnU = area_mn$value       #Needs to be changed
      #UREC$pdU= pd$value                 #Needs to be changed
    }
    
    UREC_full[i, area_new_col] = area_mn
    UREC_full[i, pd_new_col] = pd
  }
  return(UREC_full)
}

extent = PDV_fragementation_metric_function(UREC_full = extent, raster = rast_clip1, col_name = 'fragstat_Forest1')
extent = PDV_fragementation_metric_function(UREC_full = extent, raster = rast_clip2, col_name = 'fragstat_Forest2')
extent = PDV_fragementation_metric_function(UREC_full = extent, raster = rast_clip3, col_name = 'fragstat_Forest3')
extent = PDV_fragementation_metric_function(UREC_full = extent, raster = rast_clip4, col_name = 'fragstat_Forest4')
extent = PDV_fragementation_metric_function(UREC_full = extent, raster = rast_clip5, col_name = 'fragstat_Forest5')
extent = PDV_fragementation_metric_function(UREC_full = extent, raster = rast_clip6, col_name = 'fragstat_Forest6')
extent = PDV_fragementation_metric_function(UREC_full = extent, raster = rast_clip7, col_name = 'fragstat_Forest7')
extent = PDV_fragementation_metric_function(UREC_full = extent, raster = rast_clip8, col_name = 'fragstat_Forest8')

st_write(extent, 'C:/Meghana/Peau_de_vache/results.shp')
st_write(extent, 'C:/Meghana/Peau_de_vache/results.sqlite')
##################################################################
norm_poly = extent
max_area = max(extent$area_fragstat_Forest1,extent$area_fragstat_Forest2,extent$area_fragstat_Forest3,extent$area_fragstat_Forest4,extent$area_fragstat_Forest5,extent$area_fragstat_Forest6,extent$area_fragstat_Forest7,extent$area_fragstat_Forest8)
min_area = min(extent$area_fragstat_Forest1,extent$area_fragstat_Forest2,extent$area_fragstat_Forest3,extent$area_fragstat_Forest4,extent$area_fragstat_Forest5,extent$area_fragstat_Forest6,extent$area_fragstat_Forest7,extent$area_fragstat_Forest8)
norm_poly$area_fragstat_Forest1 = (extent$area_fragstat_Forest1- min_area)/(max_area-min_area)
norm_poly$area_fragstat_Forest2 = (extent$area_fragstat_Forest2- min_area)/(max_area-min_area)
norm_poly$area_fragstat_Forest3 = (extent$area_fragstat_Forest3- min_area)/(max_area-min_area)
norm_poly$area_fragstat_Forest4= (extent$area_fragstat_Forest4- min_area)/(max_area-min_area)
norm_poly$area_fragstat_Forest5 = (extent$area_fragstat_Forest5- min_area)/(max_area-min_area)
norm_poly$area_fragstat_Forest6 = (extent$area_fragstat_Forest6- min_area)/(max_area-min_area)
norm_poly$area_fragstat_Forest7 = (extent$area_fragstat_Forest7- min_area)/(max_area-min_area)
norm_poly$area_fragstat_Forest8 = (extent$area_fragstat_Forest8- min_area)/(max_area-min_area)

max_pd = max(extent$pd_fragstat_Forest1,extent$pd_fragstat_Forest2,extent$pd_fragstat_Forest3,extent$pd_fragstat_Forest4,extent$pd_fragstat_Forest5,extent$pd_fragstat_Forest6,extent$pd_fragstat_Forest7,extent$pd_fragstat_Forest8)
min_pd = min(extent$pd_fragstat_Forest1,extent$pd_fragstat_Forest2,extent$pd_fragstat_Forest3,extent$pd_fragstat_Forest4,extent$pd_fragstat_Forest5,extent$pd_fragstat_Forest6,extent$pd_fragstat_Forest7,extent$pd_fragstat_Forest8)
norm_poly$pd_fragstat_Forest1 = (extent$pd_fragstat_Forest1- min_pd)/(max_pd-min_pd)
norm_poly$pd_fragstat_Forest2 = (extent$pd_fragstat_Forest2- min_pd)/(max_pd-min_pd)
norm_poly$pd_fragstat_Forest3 = (extent$pd_fragstat_Forest3- min_pd)/(max_pd-min_pd)
norm_poly$pd_fragstat_Forest4 = (extent$pd_fragstat_Forest4- min_pd)/(max_pd-min_pd)
norm_poly$pd_fragstat_Forest5 = (extent$pd_fragstat_Forest5- min_pd)/(max_pd-min_pd)
norm_poly$pd_fragstat_Forest6 = (extent$pd_fragstat_Forest6- min_pd)/(max_pd-min_pd)
norm_poly$pd_fragstat_Forest7 = (extent$pd_fragstat_Forest7- min_pd)/(max_pd-min_pd)
norm_poly$pd_fragstat_Forest8 = (extent$pd_fragstat_Forest8- min_pd)/(max_pd-min_pd)

max_continuity = max(extent$continuity_Forest1,extent$continuity_Forest2,extent$extent$continuity_Forest3,extent$continuity_Forest4,extent$continuity_Forest5,extent$continuity_Forest6,extent$continuity_Forest7,extent$continuity_Forest8)
min_continuity = min(extent$continuity_Forest1,extent$continuity_Forest2,extent$extent$continuity_Forest3,extent$continuity_Forest4,extent$continuity_Forest5,extent$continuity_Forest6,extent$continuity_Forest7,extent$continuity_Forest8)
norm_poly$continuity_Forest1 = (extent$continuity_Forest1- min_continuity)/(max_continuity-min_continuity)
norm_poly$continuity_Forest2 = (extent$continuity_Forest2- min_continuity)/(max_continuity-min_continuity)
norm_poly$continuity_Forest3 = (extent$continuity_Forest3- min_continuity)/(max_continuity-min_continuity)
norm_poly$continuity_Forest4 = (extent$continuity_Forest4- min_continuity)/(max_continuity-min_continuity)
norm_poly$continuity_Forest5 = (extent$continuity_Forest5- min_continuity)/(max_continuity-min_continuity)
norm_poly$continuity_Forest6 = (extent$continuity_Forest6- min_continuity)/(max_continuity-min_continuity)
norm_poly$continuity_Forest7 = (extent$continuity_Forest7- min_continuity)/(max_continuity-min_continuity)
norm_poly$continuity_Forest8 = (extent$continuity_Forest8- min_continuity)/(max_continuity-min_continuity)

st_write(norm_poly, 'C:/Meghana/Peau_de_vache/results_norm.shp')
st_write(norm_poly, 'C:/Meghana/Peau_de_vache/results_norm.sqlite')

norm_poly_new = norm_poly

norm_poly_new$pd_fragstat_Forest1 = -1*(norm_poly$pd_fragstat_Forest1)+1
norm_poly_new$pd_fragstat_Forest2 = -1*(norm_poly$pd_fragstat_Forest2)+1
norm_poly_new$pd_fragstat_Forest3 = -1*(norm_poly$pd_fragstat_Forest3)+1
norm_poly_new$pd_fragstat_Forest4 = -1*(norm_poly$pd_fragstat_Forest4)+1
norm_poly_new$pd_fragstat_Forest5 = -1*(norm_poly$pd_fragstat_Forest5)+1
norm_poly_new$pd_fragstat_Forest6 = -1*(norm_poly$pd_fragstat_Forest6)+1
norm_poly_new$pd_fragstat_Forest7 = -1*(norm_poly$pd_fragstat_Forest7)+1
norm_poly_new$pd_fragstat_Forest8 = -1*(norm_poly$pd_fragstat_Forest8)+1




source('Statistics_new.R')
norm_poly_vect = vect('C:/Meghana/Peau_de_vache/results_norm.sqlite')
norm_poly_vect_Forest1 =  subset(norm_poly, select = c(area_fragstat_Forest1,pd_fragstat_Forest1,continuity_Forest1))


IndContH_Forest1 = (norm_poly_vect_Forest1$area_fragstat_Forest1+norm_poly_vect_Forest1$pd_fragstat_Forest1+norm_poly_vect_Forest1$continuity_Forest1)/3
IndContH_Forest2 =(norm_poly_vect$area_fragstat_forest2+norm_poly_vect$pd_fragstat_forest2+norm_poly_vect$continuity_forest2)/3
IndContH_Forest3 =(norm_poly_vect$area_fragstat_forest3+norm_poly_vect$pd_fragstat_forest3+norm_poly_vect$continuity_forest3)/3
IndContH_Forest4 =(norm_poly_vect$area_fragstat_forest4+norm_poly_vect$pd_fragstat_forest4+norm_poly_vect$continuity_forest4)/3
IndContH_Forest5 =(norm_poly_vect$area_fragstat_forest5+norm_poly_vect$pd_fragstat_forest5+norm_poly_vect$continuity_forest5)/3
IndContH_Forest6 =(norm_poly_vect$area_fragstat_forest6+norm_poly_vect$pd_fragstat_forest6+norm_poly_vect$continuity_forest6)/3
IndContH_Forest7 =(norm_poly_vect$area_fragstat_forest7+norm_poly_vect$pd_fragstat_forest7+norm_poly_vect$continuity_forest7)/3
IndContH_Forest8 =(norm_poly_vect$area_fragstat_forest8+norm_poly_vect$pd_fragstat_forest8+norm_poly_vect$continuity_forest8)/3



nIndContH_Forest1 = (norm_poly_vect_Forest1$area_fragstat_Forest1+norm_poly_vect_Forest1$pd_fragstat_Forest1+norm_poly_vect_Forest1$continuity_Forest1)/3
IndContH_Forest2 =(norm_poly_vect$area_fragstat_forest2+norm_poly_vect$pd_fragstat_forest2+norm_poly_vect$continuity_forest2)/3
IndContH_Forest3 =(norm_poly_vect$area_fragstat_forest3+norm_poly_vect$pd_fragstat_forest3+norm_poly_vect$continuity_forest3)/3
IndContH_Forest4 =(norm_poly_vect$area_fragstat_forest4+norm_poly_vect$pd_fragstat_forest4+norm_poly_vect$continuity_forest4)/3
IndContH_Forest5 =(norm_poly_vect$area_fragstat_forest5+norm_poly_vect$pd_fragstat_forest5+norm_poly_vect$continuity_forest5)/3
IndContH_Forest6 =(norm_poly_vect$area_fragstat_forest6+norm_poly_vect$pd_fragstat_forest6+norm_poly_vect$continuity_forest6)/3
IndContH_Forest7 =(norm_poly_vect$area_fragstat_forest7+norm_poly_vect$pd_fragstat_forest7+norm_poly_vect$continuity_forest7)/3
IndContH_Forest8 =(norm_poly_vect$area_fragstat_forest8+norm_poly_vect$pd_fragstat_forest8+norm_poly_vect$continuity_forest8)/3
#######################################################################################################################################
#Create empty df with 

# created vector with 5 characters
columns= c("Forest1","Forest2",'Forest3','Forest4','Forest5','Forest6', 'Forest7', 'Forest8')

# pass this vector length to ncol parameter
# and nrow with 0
IndContH_new = data.frame(matrix(nrow = 1, ncol = length(columns)))

# assign column names
colnames(IndContH_new) = columns

# display
print(IndContH_new)



IndContH_new$Forest1 = (norm_poly_new$area_fragstat_Forest1 +norm_poly_new$pd_fragstat_Forest1 + norm_poly_new$continuity_Forest1)/3
IndContH_new$Forest2 = (norm_poly_new$area_fragstat_Forest2 +norm_poly_new$pd_fragstat_Forest2 + norm_poly_new$continuity_Forest2)/3 #TODO Change this to NA
IndContH_new$Forest3 = (norm_poly_new$area_fragstat_Forest3 +norm_poly_new$pd_fragstat_Forest3 + norm_poly_new$continuity_Forest3)/3
IndContH_new$Forest4 = (norm_poly_new$area_fragstat_Forest4 +norm_poly_new$pd_fragstat_Forest4 + norm_poly_new$continuity_Forest4)/3
IndContH_new$Forest5 = (norm_poly_new$area_fragstat_Forest5 +norm_poly_new$pd_fragstat_Forest5 + norm_poly_new$continuity_Forest5)/3
IndContH_new$Forest6 = (norm_poly_new$area_fragstat_Forest6 +norm_poly_new$pd_fragstat_Forest6 + norm_poly_new$continuity_Forest6)/3
IndContH_new$Forest7 = (norm_poly_new$area_fragstat_Forest7 +norm_poly_new$pd_fragstat_Forest7 + norm_poly_new$continuity_Forest7)/3
IndContH_new$Forest8 = (norm_poly_new$area_fragstat_Forest8 +norm_poly_new$pd_fragstat_Forest8 + norm_poly_new$continuity_Forest8)/3
IndContH_new = round(IndContH_new, 2)
st_write(IndContH_new, 'C:/Meghana/Peau_de_vache/results_norm_new.sqlite')
st_write(IndContH_new, 'C:/Meghana/Peau_de_vache/results_norm_new.shp')

# -------------------------------------------------------------------------



#New set of rasters with four classes
##################################################################
unit = extent
sampling_units = poly_voronoi


#Raster Forest
rastF = rast_clip1
rastF_mh = rastF
rastF_mh[rastF_mh != 2] <- NA # replace what is not greassland by NA
rastF_A = rastF
rastF_A[rastF_A !=3]<-NA
rastF_V = rastF
rastF_V[rastF_V ==1]<-1
plot(rastF, col ='dark green')



#raster only MH
rastMH = rastF
rastMH[rastMH == 1]<-2

rastMH_F = rastMH
rastMH_F[rastMH_F == 2] <- NA # replace what is not greassland by NA
rastMH_A = rastMH
rastMH_A[rastMH_A ==2]<-NA
rastMH_V = rastMH
rastF_V[rastF_V ==2]<-1
plot(rastMH, col = 'purple')


#Raster half MH and other half Forest aleatoirement
rastFMH = rastF
rastFMH[1:38,1:273]<-sample(1:2,10374, replace =T)
plot(rastFMH)
rastFMH_F = rastFMH
rastFMH_F[rastFMH_F ==2]<-NA
plot(rastFMH_F)
rastFMH_MH = rastFMH
rastFMH_MH[rastFMH_MH ==1]<-NA
rastFMH_MH[rastFMH_MH ==2]<-1
plot(rastFMH_MH)
rastFMH_A = rastFMH
rastFMH_A[1:38,1:273]<-NA
plot(rastFMH_A)
rastFMH_V = rastFMH
rastFMH_V[rastFMH_V ==2]<-1
plot(rastFMH, col = c(' dark green', 'purple'))


#Raster only Agriculture
rastA = rastF
rastA[1:38,1:273]<-3
plot(rastA)
rastA_F = rastA
rastA_F[rastA_F == 3] <- NA # replace what is not greassland by NA
rastA_MH = rastA
rastA_MH[rastA_MH ==3]<-NA
rastA_V = rastA
rastA_V[rastA_V ==3]<-1
plot(rastA, col ='gold')

#Raster only Vegetation
rastV = rastF
rastV[1:38,1:273]<-sample(1:3,10374, replace =T)
plot(rastV, col = c('dark green', 'purple', 'gold'))
rastV_V = rastF #everything is vegetation and has a value of 1
rastV_F = rastV
rastV_F[rastV_F !=1]<-NA
plot(rastV_F)
rastV_MH = rastV
rastV_MH[rastV_MH !=2]<-NA
rastV_MH[rastV_MH ==2]<-1
plot(rastV_MH)
rastV_A = rastV
rastV_A[rastV_A==1]<-NA
rastV_A[rastV_A ==2]<-NA
rastV_A[rastV_A ==3]<-1
plot(rastV_A)





#Calculate metrics

#RastF (all forest)
######################
pdv_results_rastF = PDV_continuity_metric(
  UREC_full = unit,
  UREC_sampling = sampling_units,
  raster_file = rastF,
  classe_UT = 'Forest'
)
pdv_results_rastF = PDV_continuity_metric(
  UREC_full = pdv_results_rastF,
  UREC_sampling = sampling_units,
  raster_file = rastF_V,
  classe_UT = 'Vegetation'
)
pdv_results_rastF = PDV_continuity_metric(
  UREC_full = pdv_results_rastF,
  UREC_sampling = sampling_units,
  raster_file = rastF_A,
  classe_UT = 'Agricole'
)
pdv_results_rastF = PDV_continuity_metric(
  UREC_full = pdv_results_rastF,
  UREC_sampling = sampling_units,
  raster_file = rastF_mh,
  classe_UT = 'MH'
)

pdv_results_rastF = PDV_fragementation_metric_function(UREC_full = pdv_results_rastF,raster_file = rastF, col_name = 'Forest' )
pdv_results_rastF = PDV_fragementation_metric_function(UREC_full = pdv_results_rastF,raster_file = rastF, col_name = 'Forest' )
pdv_results_rastF = PDV_fragementation_metric_function(UREC_full = pdv_results_rastF,raster_file = rastF_A, col_name = 'Agricole' )
pdv_results_rastF = PDV_fragementation_metric_function(UREC_full = pdv_results_rastF,raster_file = rastF_V, col_name = 'Vegetation' )
pdv_results_rastF = PDV_fragementation_metric_function(UREC_full = pdv_results_rastF,raster_file = rastF_mh, col_name = 'MH' )
pdv_results_rastF = PDV_Naturality_index_function(UREC_Nat =pdv_results_rastF, raster_file = rastF)
# pdv_results_rastF = subset(pdv_results_rastF, select = c(id, geometry, continuity_Forest, continuity_Vegetation,

#                                                          continuity_Agricole, continuity_MH, pd_Forest,
#                                                          area_Agricole, pd_Agricole, area_Vegetation))

#rast_mh all MH
#####################################################################
pdv_results_rastMH = PDV_continuity_metric(
  UREC_full = unit,
  UREC_sampling = sampling_units,
  raster_file = rastMH_F,
  classe_UT = 'Forest'
)
pdv_results_rastMH = PDV_continuity_metric(
  UREC_full = pdv_results_rastMH,
  UREC_sampling = sampling_units,
  raster_file = rastMH_V,
  classe_UT = 'Vegetation'
)
pdv_results_rastMH = PDV_continuity_metric(
  UREC_full = pdv_results_rastMH,
  UREC_sampling = sampling_units,
  raster_file = rastMH_A,
  classe_UT = 'Agricole'
)
pdv_results_rastMH = PDV_continuity_metric(
  UREC_full = pdv_results_rastMH,
  UREC_sampling = sampling_units,
  raster_file = rastMH,
  classe_UT = 'MH'
)

pdv_results_rastMH = PDV_fragementation_metric_function(UREC_full = pdv_results_rastMH,raster_file = rastMH_F, col_name = 'Forest' )
pdv_results_rastMH = PDV_fragementation_metric_function(UREC_full = pdv_results_rastMH,raster_file = rastMH_A, col_name = 'Agricole' )
pdv_results_rastMH = PDV_fragementation_metric_function(UREC_full = pdv_results_rastMH,raster_file = rastMH_V, col_name = 'Vegetation' )
pdv_results_rastMH = PDV_fragementation_metric_function(UREC_full = pdv_results_rastMH,raster_file = rastMH, col_name = 'MH' )
pdv_results_rastMH = PDV_Naturality_index_function(UREC_Nat = pdv_results_rastMH, raster_file = rastMH)
# pdv_results_rastMH = subset(pdv_results_rastMH, select = c(id, geometry, continuity_Forest, continuity_Vegetation,
#                                                            continuity_Agricole, continuity_MH, pd_Forest,
#                                                            area_Agricole, pd_Agricole, area_Vegetation))

#RastFMH
##########################################
pdv_results_rastFMH = PDV_continuity_metric(
  UREC_full = unit,
  UREC_sampling = sampling_units,
  raster_file = rastFMH_F,
  classe_UT = 'Forest'
)
pdv_results_rastFMH = PDV_continuity_metric(
  UREC_full = pdv_results_rastFMH,
  UREC_sampling = sampling_units,
  raster_file = rastFMH_V,
  classe_UT = 'Vegetation'
)
pdv_results_rastFMH = PDV_continuity_metric(
  UREC_full = pdv_results_rastFMH,
  UREC_sampling = sampling_units,
  raster_file = rastFMH_A,
  classe_UT = 'Agricole'
)
pdv_results_rastFMH = PDV_continuity_metric(
  UREC_full = pdv_results_rastFMH,
  UREC_sampling = sampling_units,
  raster_file = rastFMH_MH,
  classe_UT = 'MH'
)

pdv_results_rastFMH = PDV_fragementation_metric_function(UREC_full = pdv_results_rastFMH,raster_file = rastFMH_F, col_name = 'Forest' )
pdv_results_rastFMH = PDV_fragementation_metric_function(UREC_full = pdv_results_rastFMH,raster_file = rastFMH_A, col_name = 'Agricole' )
pdv_results_rastFMH = PDV_fragementation_metric_function(UREC_full = pdv_results_rastFMH,raster_file = rastFMH_V, col_name = 'Vegetation' )
pdv_results_rastFMH = PDV_fragementation_metric_function(UREC_full = pdv_results_rastFMH,raster_file = rastFMH_MH, col_name = 'MH' )
pdv_results_rastFMH = PDV_Naturality_index_function(UREC_Nat = pdv_results_rastFMH, raster_file = rastFMH)
# pdv_results_rastFMH = subset(pdv_results_rastMH, select = c(id, geometry, continuity_Forest, continuity_Vegetation,
#                                                             continuity_Agricole, continuity_MH, pd_Forest,
#                                                             area_Agricole, pd_Agricole, area_Vegetation))


#rastA (only agriculture)
#######################################
pdv_results_rastA = PDV_continuity_metric(
  UREC_full = unit,
  UREC_sampling = sampling_units,
  raster_file = rastA_F,
  classe_UT = 'Forest'
)
pdv_results_rastA = PDV_continuity_metric(
  UREC_full = pdv_results_rastA,
  UREC_sampling = sampling_units,
  raster_file = rastA_V,
  classe_UT = 'Vegetation'
)
pdv_results_rastA = PDV_continuity_metric(
  UREC_full = pdv_results_rastA,
  UREC_sampling = sampling_units,
  raster_file = rastA,
  classe_UT = 'Agricole'
)
pdv_results_rastA = PDV_continuity_metric(
  UREC_full = pdv_results_rastA,
  UREC_sampling = sampling_units,
  raster_file = rastA_MH,
  classe_UT = 'MH'
)

pdv_results_rastA = PDV_fragementation_metric_function(UREC_full = pdv_results_rastA,raster_file = rastA_F, col_name = 'Forest' )
pdv_results_rastA = PDV_fragementation_metric_function(UREC_full = pdv_results_rastA,raster_file = rastA, col_name = 'Agricole' )
pdv_results_rastA = PDV_fragementation_metric_function(UREC_full = pdv_results_rastA,raster_file = rastA_V, col_name = 'Vegetation' )
pdv_results_rastA = PDV_fragementation_metric_function(UREC_full = pdv_results_rastA,raster_file = rastA_MH, col_name = 'MH' )
pdv_results_rastA = PDV_Naturality_index_function(UREC_Nat = pdv_results_rastA, raster_file = rastA )

# pdv_results_rastA = subset(pdv_results_rastA, select = c(id, geometry, continuity_Forest, continuity_Vegetation,
#                                                          continuity_Agricole, continuity_MH, pd_Forest,
#                                                          area_Agricole, pd_Agricole, area_Vegetation))

#rastV (only vegetation)
#########################################################
pdv_results_rastV = PDV_continuity_metric(
  UREC_full = unit,
  UREC_sampling = sampling_units,
  raster_file = rastV_F,
  classe_UT = 'Forest'
)
pdv_results_rastV = PDV_continuity_metric(
  UREC_full = pdv_results_rastV,
  UREC_sampling = sampling_units,
  raster_file = rastV_V,
  classe_UT = 'Vegetation'
)
pdv_results_rastV = PDV_continuity_metric(
  UREC_full = pdv_results_rastV,
  UREC_sampling = sampling_units,
  raster_file = rastV_A,
  classe_UT = 'Agricole'
)
pdv_results_rastV = PDV_continuity_metric(
  UREC_full = pdv_results_rastV,
  UREC_sampling = sampling_units,
  raster_file = rastV_MH,
  classe_UT = 'MH'
)

pdv_results_rastV = PDV_fragementation_metric_function(UREC_full = pdv_results_rastV,raster_file = rastV_F, col_name = 'Forest' )
pdv_results_rastV = PDV_fragementation_metric_function(UREC_full = pdv_results_rastV,raster_file = rastV_A, col_name = 'Agricole' )
pdv_results_rastV = PDV_fragementation_metric_function(UREC_full = pdv_results_rastV,raster_file = rastV_V, col_name = 'Vegetation' )
pdv_results_rastV = PDV_fragementation_metric_function(UREC_full = pdv_results_rastV,raster_file = rastV_MH, col_name = 'MH' )
pdv_results_rastV = PDV_Naturality_index_function(UREC_Nat = pdv_results_rastV, raster_file = rastV)
# pdv_results_rastV = subset(pdv_results_rastV, select = c(id, geometry, continuity_Forest, continuity_Vegetation,
#                                                          continuity_Agricole, continuity_MH, pd_Forest,
#                                                          area_Agricole, pd_Agricole, area_Vegetation))


########################################################
#Combine different case dataframes together 
pdv_results_new = rbind(pdv_results_rastF,pdv_results_rastMH)
pdv_results_new$id = c('Only forest', 'Only MH')
pdv_results_new = rbind(pdv_results_new,pdv_results_rastFMH)
pdv_results_new[3,]$id = 'Half forest half MH'
pdv_results_new = rbind(pdv_results_new,pdv_results_rastA)
pdv_results_new[4,]$id = 'Only Agriculture'
pdv_results_new = rbind(pdv_results_new,pdv_results_rastV)
pdv_results_new[5,]$id = 'Only Vegetation'

#Select the metrics we are interedted in 
pdv_results_indice = subset(pdv_results_new, select = -c(area_Agricole,
                                                         area_Forest, 
                                                        area_MH,
                                                        area_Vegetation))

#####################################################
#Normalise and inverse the dataset 
######
source('Normalization_functions.R')

pdv_results_new_norm1 = Normalization_function(UREC_merge = pdv_results_indice)

pdv_results_new_norm1 = New_inverse_fun(UREC_norm = pdv_results_new_norm1, 
                                        col_names = c('pd_Forest_nrm', 'pd_Agricole_nrm', 'pd_MH_nrm', 'pd_Vegetation_nrm'))

pdv_results_new_norm1$pd_Vegetation_nrm =1

#########################################################
#Create indice of Connectivity du paysage
######
#with equal weights
pdv_indice_contP= pdv_results_new_norm1

pdv_indice_contP$FE_poids_egal = round((pdv_indice_contP$continuity_Vegetation_nrm +
                                         pdv_indice_contP$continuity_Forest_nrm + 
                                          pdv_indice_contP$continuity_Agricole_nrm + 
                                          pdv_indice_contP$continuity_MH_nrm +
                                          pdv_indice_contP$pd_Forest_nrm + 
                                          pdv_indice_contP$pd_Agricole_nrm+
                                          pdv_indice_contP$pd_Vegetation_nrm+
                                          pdv_indice_contP$pd_MH_nrm)/8,2)
  

max_pdv_indice_contP_FE_poids_egal = max(pdv_indice_contP$FE_poids_egal)
min_pdv_indice_contP_FE_poids_egal = min(pdv_indice_contP$FE_poids_egal)
pdv_indice_contP$FE_poids_egal_nrm = (pdv_indice_contP$FE_poids_egal - min_pdv_indice_contP_FE_poids_egal)/(max_pdv_indice_contP_FE_poids_egal - min_pdv_indice_contP_FE_poids_egal)

#With assigned weights
pdv_indice_contP$FE_poids = round((
  pdv_indice_contP$continuity_Vegetation_nrm * (5 / 32) +
    pdv_indice_contP$continuity_Forest_nrm * (5 / 32)+
    pdv_indice_contP$continuity_Agricole_nrm* (1 / 32) +
    pdv_indice_contP$continuity_MH_nrm* (5 / 32) +
    pdv_indice_contP$pd_Forest_nrm * (5 / 32) +
    pdv_indice_contP$pd_Agricole_nrm* (1 / 32) +
    pdv_indice_contP$pd_Vegetation_nrm* (5 / 32) +
    pdv_indice_contP$pd_MH_nrm* (5 / 32)
),
2
)

max_pdv_indice_contPFE_poids = max(pdv_indice_contP$FE_poids)
min_pdv_indice_contPFE_poids = min(pdv_indice_contP$FE_poids)
pdv_indice_contP$FE_poids_nrm = (pdv_indice_contP$FE_poids - min_pdv_indice_contPFE_poids)/ (max_pdv_indice_contPFE_poids - min_pdv_indice_contPFE_poids)


################################################################3
#Plotting the data
################


windows(10,5)
#par(mar = c(5,4,4,8))
plot(
  pdv_indice_contP$FE_poids,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)

points(pdv_indice_contP$FE_poids_egal, pch = 3, col = 'red')
legend("topright", 
       legend=c("Indice avec poids", " Indice poids egal"),
       col=c("black", "red"), pch =3, cex=0.8)


windows(10,5)
plot(
  pdv_indice_contP$FE_poids_nrm,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1 nrm",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)

points(pdv_indice_contP$FE_poids_egal_nrm, pch = 3, col = 'red')
legend("topright", legend=c("Indice avec poids", " Indice poids egal"),
       col=c("black", "red"), pch =3, cex=0.8)




#############Plot with metrics

#Vegetation
pdv_indice_contP$FEContP_Vegetation1 = round(
  (pdv_indice_contP$continuity_Vegetation_nrm + pdv_indice_contP$pd_Vegetation_nrm)/2 
  ,
  2
)


plot(
  pdv_indice_contP$FE_poids_nrm,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1 nrm",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)

points(pdv_indice_contP$FE_poids_egal_nrm, pch = 3, col = 'red')
points(pdv_indice_contP$FEContP_Vegetation1, pch = 3, col = 'blue')
legend("bottomleft", legend=c("Indice avec poids", " Indice poids egal", 'moyenne Veg'),
       col=c("black", "red", 'blue'), pch =3, cex=0.8)


#MH
pdv_indice_contP$FEContP_MH1 = round(
  (pdv_indice_contP$continuity_MH_nrm + pdv_indice_contP$pd_MH_nrm)/2 
  ,
  2
)


plot(
  pdv_indice_contP$FE_poids_nrm,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1 nrm",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)

points(pdv_indice_contP$FE_poids_egal_nrm, pch = 3, col = 'red')
points(pdv_indice_contP$FEContP_MH1, pch = 3, col = 'blue')
legend("topright", legend=c("Indice avec poids", " Indice poids egal", 'moyenne MH'),
       col=c("black", "red", 'blue'), pch =3, cex=0.8)

#Forest
pdv_indice_contP$FEContP_Forest1 = round(
  (pdv_indice_contP$continuity_Forest_nrm + pdv_indice_contP$pd_Forest_nrm)/2 
  ,
  2
)


plot(
  pdv_indice_contP$FE_poids_nrm,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1 nrm",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)

points(pdv_indice_contP$FE_poids_egal_nrm, pch = 3, col = 'red')
points(pdv_indice_contP$FEContP_Forest1, pch = 3, col = 'blue')
legend("topright", legend=c("Indice avec poids", " Indice poids egal", 'moyenne Forest'),
       col=c("black", "red", 'blue'), pch =3, cex=0.8)

#Agricole
pdv_indice_contP$FEContP_Agricole = round(
  (pdv_indice_contP$continuity_Agricole_nrm + pdv_indice_contP$pd_Agricole_nrm)/2 
  ,
  2
)


plot(
  pdv_indice_contP$FE_poids_nrm,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1 nrm",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)

points(pdv_indice_contP$FE_poids_egal_nrm, pch = 3, col = 'red')
points(pdv_indice_contP$FEContP_Agricole, pch = 3, col = 'blue')
legend("topleft", legend=c("Indice avec poids", " Indice poids egal", 'moyenne Agricole'),
       col=c("black", "red", 'blue'), pch =3, cex=0.8)


##########################################################################################

#New tests for Connectivity du paysage 
###############################################
#indice de naturalite similar to IQBR with only forest, milieu humide, agriculture and urban


PDV_Naturality_index_function <- function(UREC_Nat, raster_file) {
  
  UREC_Nat = st_transform(UREC_Nat, crs(raster_file))
  #Reduce the extent of the MHC raster to the extent of UREC
  raster_file_clip = terra::crop(raster_file, UREC_merge) #Needs to be changed depending on which extent we are using
  UREC_nat_rasterize = terra::rasterize(UREC_Nat, raster_file_clip, 'id') #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
  raster_file_mask = terra::mask(raster_file, UREC_nat_rasterize)
  Occ_sol = raster_file_mask
  #Create a forest mask
  Occ_sol_F = Occ_sol
  Occ_sol_F[Occ_sol_F != 1] <-
    NA # replace what is not forest by NA
  Occ_sol_F[Occ_sol_F == 1] <- 1 # replace 10 (forest) by 1
  
  #Create a MH mask
  Occ_sol_MH = Occ_sol
  Occ_sol_MH[Occ_sol_MH != 2] <-
    NA # replace what is not greassland by NA
  Occ_sol_MH[Occ_sol_MH == 2] <- 1 # replace 30 (grassland) by 1
  #Create an Agriculture mask
  Occ_sol_A = Occ_sol
  Occ_sol_A[Occ_sol_A != 3] <-
    NA # replace what is not urban (built-up) by NA
  Occ_sol_A[Occ_sol_A == 3] <- 1 # replace 50 (built-up) by 1
  #Create an urban mask
  Occ_sol_U = Occ_sol
  Occ_sol_U[Occ_sol_U != 4] <-
    NA # replace what is not agriculture (cropland) by NA
  Occ_sol_U[Occ_sol_U == 4] <- 1 # replace 40 (cropland) by 1
  
  #Extract surface areas for each land use type
  UREC_Nat$surface_full_F = exactextractr::exact_extract(Occ_sol_F, UREC_Nat, 'sum') *
    100 #multiply sum by area of each pixel (meed to do this with sf object!)
  UREC_Nat$surface_full_MH = exactextractr::exact_extract(Occ_sol_MH, UREC_Nat, 'sum') *
    100
  UREC_Nat$surface_full_A = exactextractr::exact_extract(Occ_sol_A, UREC_Nat, 'sum') *
    100
  UREC_Nat$surface_full_U = exactextractr::exact_extract(Occ_sol_U, UREC_Nat, 'sum') *
    100
  #Extract the proportion of each land use type relative to the area of the spatial unit
  UREC_Nat$area = as.numeric(st_area(UREC_Nat)) #calculate the area of each sampling unit in the UREC
  UREC_Nat$propF = UREC_Nat$surface_full_F / UREC_Nat$area # get the ratio of forested area in each sampling unit by the area of the sampling unit
  UREC_Nat$propMH = UREC_Nat$surface_full_MH / UREC_Nat$area # get the ratio of grassland area in each sampling unit by the area of the sampling unit
  UREC_Nat$propU = UREC_Nat$surface_full_U / UREC_Nat$area # get the ratio of urban area in each sampling unit by the area of the sampling unit
  UREC_Nat$propA = UREC_Nat$surface_full_A / UREC_Nat$area # get the ratio of urban area in each sampling unit by the area of the sampling unit
  
  UREC_Nat$IndiceNaturalite = (UREC_Nat$propF * 10 + UREC_Nat$propMH * 10 + UREC_Nat$propU * 0 +
                                 UREC_Nat$propA * 1.9) / 10
  
  return(UREC_Nat)
}



#################NEW SET OF RASTERS###################
#Only Forest
new_rastF = rastF
new_rastF_VO = new_rastF
new_rastF_VO[new_rastF_VO ==1]<-1
new_rastF_VO[new_rastF_VO ==2]<-1

new_resutls_F  = PDV_continuity_metric(UREC_full = extent, UREC_sampling = poly_voronoi, raster_file = new_rastF_VO, classe_UT =  'Vegetation_optimale')
new_resutls_F = PDV_fragementation_metric_function(UREC_full = new_resutls_F, raster_file = new_rastF_VO, col_name = "Vegetation_optimal" )
new_resutls_F = PDV_Naturality_index_function(UREC_Nat = new_resutls_F, raster_file = new_rastF)

#Only MH 
new_rastMH = rastMH
new_rastMH_VO = new_rastMH
new_rastMH_VO[new_rastMH_VO ==1]<-1
new_rastMH_VO[new_rastMH_VO ==2]<-1

new_resutls_MH  = PDV_continuity_metric(UREC_full = extent, UREC_sampling = poly_voronoi, raster_file = new_rastMH_VO, classe_UT =  'Vegetation_optimale')
new_resutls_MH = PDV_fragementation_metric_function(UREC_full = new_resutls_MH, raster_file = new_rastMH_VO, col_name = "Vegetation_optimal" )
new_resutls_MH = PDV_Naturality_index_function(UREC_Nat = new_resutls_MH, raster_file = new_rastMH)

#F and Mh 
new_rastFMH = rastFMH
new_rastFMH_VO = new_rastFMH
new_rastFMH_VO[new_rastFMH_VO ==1]<-1
new_rastFMH_VO[new_rastFMH_VO ==2]<-1

new_resutls_FMH  = PDV_continuity_metric(UREC_full = extent, UREC_sampling = poly_voronoi, raster_file = new_rastFMH_VO, classe_UT =  'Vegetation_optimale')
new_resutls_FMH = PDV_fragementation_metric_function(UREC_full = new_resutls_FMH, raster_file = new_rastFMH_VO, col_name = "Vegetation_optimal" )
new_resutls_FMH = PDV_Naturality_index_function(UREC_Nat = new_resutls_FMH, raster_file = new_rastFMH)


#Only A 
new_rastA = rastA
new_rastA_VO = new_rastA
new_rastA_VO[new_rastA_VO ==1]<-1
new_rastA_VO[new_rastA_VO ==2]<-1
new_rastA_VO[new_rastA_VO !=1] <-NA


new_resutls_A  = PDV_continuity_metric(UREC_full = extent, UREC_sampling = poly_voronoi, raster_file = new_rastA_VO, classe_UT =  'Vegetation_optimale')
new_resutls_A = PDV_fragementation_metric_function(UREC_full = new_resutls_A, raster_file = new_rastA_VO, col_name = "Vegetation_optimal" )
new_resutls_A = PDV_Naturality_index_function(UREC_Nat = new_resutls_A, raster_file = new_rastA)

# ONly U
new_rastU = rastF
new_rastU[new_rastU ==1]<-4 
new_rastU_VO = new_rastU
new_rastU_VO[new_rastU_VO ==1]<-1
new_rastU_VO[new_rastU_VO ==2]<-1
new_rastU_VO[new_rastU_VO !=1]<-NA
plot(new_rastU, col = c('grey'))

new_resutls_U  = PDV_continuity_metric(UREC_full = extent, UREC_sampling = poly_voronoi, raster_file = new_rastU_VO, classe_UT =  'Vegetation_optimale')
new_resutls_U = PDV_fragementation_metric_function(UREC_full = new_resutls_U, raster_file = new_rastU_VO, col_name = "Vegetation_optimal" )
new_resutls_U = PDV_Naturality_index_function(UREC_Nat = new_resutls_U, raster_file = new_rastU)



#third A F and MH
new_rastV = rastV
new_rastV_VO = new_rastV
new_rastV_VO[new_rastV_VO ==1]<-1
new_rastV_VO[new_rastV_VO ==2]<-1
new_rastV_VO[new_rastV_VO !=1]<-NA

new_resutls_V  = PDV_continuity_metric(UREC_full = extent, UREC_sampling = poly_voronoi, raster_file = new_rastV_VO, classe_UT =  'Vegetation_optimale')
new_resutls_V = PDV_fragementation_metric_function(UREC_full = new_resutls_V, raster_file = new_rastV_VO, col_name = "Vegetation_optimal" )
new_resutls_V = PDV_Naturality_index_function(UREC_Nat = new_resutls_V, raster_file = new_rastV)


#quarter of all landuse types
new_rastVU  = rastF
new_rastVU[1:38,1:273]<- sample(1:4,10374, replace =T)
new_rastVU_VO = new_rastVU
new_rastVU_VO[new_rastVU_VO ==1]<-1
new_rastVU_VO[new_rastVU_VO ==2]<-1
new_rastVU_VO[new_rastVU_VO !=1]<-NA
plot(new_rastVU,col = c('dark green', 'purple', 'gold','grey'))

new_resutls_VU  = PDV_continuity_metric(UREC_full = extent, UREC_sampling = poly_voronoi, raster_file = new_rastVU_VO, classe_UT =  'Vegetation_optimale')
new_resutls_VU = PDV_fragementation_metric_function(UREC_full = new_resutls_VU, raster_file = new_rastVU_VO, col_name = "Vegetation_optimal" )
new_resutls_VU = PDV_Naturality_index_function(UREC_Nat = new_resutls_VU, raster_file = new_rastVU)

new_results = rbind(new_resutls_F, new_resutls_MH)
new_results$id =  c('Only forest', 'Only MH')
new_results= rbind(new_results, new_resutls_FMH)
new_results[3,]$id = 'Half forest half MH'
new_results= rbind(new_results, new_resutls_A)
new_results[4,]$id = 'Only Agriculture'
new_results= rbind(new_results, new_resutls_V)
new_results[5,]$id = '1/3 Veg, Forest, MH'
new_results= rbind(new_results, new_resutls_U)
new_results[6,]$id = 'Only Urban'
new_results= rbind(new_results, new_resutls_VU)
new_results[7,]$id = '1/4 Veg, Forest, MH, Urban'

sub_new_results =  subset(new_results, select = c(id,
                                                  continuity_Vegetation_optimale, 
                                                  pd_Vegetation_optimal,
                                                  IndiceNaturalite))
#Normalise the metrics

source('Normalization_functions.R')

sub_new_results_norm = Normalization_function(UREC_merge = sub_new_results)

sub_new_results_norm1 = New_inverse_fun(UREC_norm = sub_new_results_norm, 
                                        col_names = c( 'pd_Vegetation_optimal_nrm'))


sub_new_results_norm1$IndiceConP = (sub_new_results_norm1$continuity_Vegetation_optimale_nrm + 
                                      sub_new_results_norm1$pd_Vegetation_optimal_nrm + 
                                      sub_new_results_norm1$IndiceNaturalite_nrm)/3
window(5,10)
plot(sub_new_results_norm1$IndiceConP)
axis(1, at = sub_new_results_norm1$id)

windows(10,5)
#par(mar = c(5,4,4,8))
plot(
  pdv_indice_contP$FE_poids,
  xlab = 'Unite spatiale',
  ylab = "Valeur de l'indice entre 0 et 1",
  pch = 3,
  main = "Indice de connectivite du paysage",
  ylim = c(0,1)
)

save.image()
