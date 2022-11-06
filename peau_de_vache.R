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

PDV_fragementation_metric_function <- function(UREC_full, raster, col_name) {
  #UREC_full = shapefile of UREC (UREC_merge) as sf data.frame
  #raster = raster of Utilisation du territoire masked for values we are interested in. e.g.  <Forest> (need to do this step beforehand , in ArcMap sometimes)
  # var 3  = character string of name of colum referencing the specific UT selection eg. "Forest" 
  area_new_col = paste0('area_', col_name)
  pd_new_col = paste0('pd_', col_name)
  raster[raster == 0] <- NA

  for (i in 1:nrow(UREC_full)) {
    UREC = UREC_full[i,]
    UREC = st_transform(UREC, st_crs(raster))
    ut_urec = terra::crop(raster, UREC)
    urec_raster = terra::rasterize(UREC, ut_urec) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
    ut_mask = terra::mask(ut_urec, urec_raster)
    
    check = check_landscape(ut_mask, verbose = T)
    if (check$OK == "✖") {
      area_mn = 0   #Needs to be changed
      pd = 0        #Needs to be changed
      
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

