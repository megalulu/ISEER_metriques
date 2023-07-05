# #This script is to validate ISEER in Maximes units
# install.packages("ids") #a voir si on en a encore besoin
# library(ids)
#Load maximes units 

v_units = st_read("C:/Meghana/Belgique/decembre/data/Validation/validation_units1.shp")
#we need to set up these units in the same architecture as UREC with id, id_uea and rive
keep_v_units = c('Id')
v_units = v_units[,names(v_units)%in% keep_v_units]
names(v_units)
v_units$rive = sample(1:533, 533, replace=F)#create a numeric column with random numbers to represent rive
v_units$id_uea = ids::random_id(533, 4)  #create random character id for column id_uea (these may repeate but it doesn't matter for now)
v_units$id = v_units$Id #change name of column Id to id
v_units = subset(v_units, select = -c(Id)) #remove duplicate column we created Id
v_units = st_zm(v_units, drop = T, what = 'ZM')
v_units1 = st_transform(v_units, "EPSG:32198")

st_write(v_units, "C:/Meghana/Belgique/decembre/data/Validation/validation_units2.sqlite")
st_write(v_units, "C:/Meghana/Belgique/decembre/data/Validation/validation_units2.shp")

#Calculate metrics we need for ISEER

# 1. Surface vegetation 
######
#Load data 
raster_surface = raster('C:/Meghana/donnee_brutes/UTILISATION_TERRITOIRE/utilisation_territoire_2019/utilisation_territoire_2019/utilisation_territoire_2019.tif')
raster_surface = terra::crop(raster_surface, v_units1)
writeRaster(raster_surface, "C:/Meghana/Belgique/decembre/data/Validation/crop_raster_validation.tif")
csv__class_correspondence = read.csv2("C:/Meghana/donnee_brutes/UT_2019_10m.csv")
#UREC_merge = v_units (in this case, no need to reload)

v_surface = SurfaceClass(UREC_merge = v_units1, csv_class_correspondence = csv__class_correspondence, raster_file = raster_surface)
#check that sum of surface areas = 1
v_surface$vegetation = v_surface$Forestier + v_surface$Humide + v_surface$Agricole
st_write(v_surface, 'C:/Meghana/Belgique/decembre/traitements/Validation1/units_surface.sqlite')

##########
#2. Pente 
####################
#load data 
v_units = st_read("C:/Meghana/Belgique/decembre/data/Validation/validation_units2.sqlite")

pentes_mtm7 = rast('C:/Meghana/donnee_brutes/LIDAR/Pentes/MTM7/pente_mtm7.vrt')
pentes_mtm7_clip = terra::crop(pentes_mtm7, v_units)

pentes_v_units = AverageSlope(UREC_merge = v_units, slope_raster = pentes_mtm7_clip)
st_write(pentes_v_units,'C:/Meghana/Belgique/decembre/traitements/Validation1/units_avrslope.sqlite')

##########
#2. HydCond
####################
#load data average HydCon, urbain mask, units projected to HydCon
avr_Hc = terra::rast('C:/Meghana/Belgique/decembre/traitements/SIIGSOL_traitements/avr_HydraulicConductivity.tif')
v_units_Hydcon = st_transform(v_units, st_crs(avr_Hc))#transform units to same projection as raster avr_Hc
avr_Hc_crop = terra::crop(avr_Hc, v_units_Hydcon) #Crop area of raster avr_Hc to only have units area
avr_Hc_crop[is.na(avr_Hc_crop)]<-0 #Assing a value of 0 to all NA area in order to make calculations easier
#load vector data of urban mask
urb_mask_vect = st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/Ut_anthro_vec.shp')
raster_avrHc = avr_Hc_crop
#Change resolution of the raster!
raster_avrHc = disagg(raster_avrHc, 10)# make the spatial resolution of avrhydcon into 10x10

UREC_merge = v_units_Hydcon
urban_vector = urb_mask_vect

avr_hydCond_t1 = AverageHydraulicConductivity(raster_avrHc = raster_avrHc, UREC_merge = UREC_merge, urban_vector = urban_vector)
st_write(avr_hydCond_t1,'C:/Meghana/Belgique/decembre/traitements/Validation1/units_HydCon.sqlite')

#################
#3. Hepb (Hauteur emergee au peid de berge)
#########
#Load data 
mnt_7 = rast('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MNT/MTM7/mnt_mtm7.vrt')
v_units = st_read("C:/Meghana/Belgique/decembre/data/Validation/validation_units2.sqlite")
v_units_hepb = st_transform(v_units, st_crs(mnt_7))
urb_mask_vect = st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/Ut_anthro_vec.shp')
urb_mask_vect = st_transform(urb_mask_vect,st_crs(v_units_hepb ))

Hep_units = AverageHepb(UREC_merge = v_units_hepb, raster_mnt = mnt_7, urban_mask_vect =urb_mask_vect )
st_write(Hep_units,'C:/Meghana/Belgique/decembre/traitements/Validation1/units_Hepb.sqlite' )

############################
#5. mean height
#######
#Load Data 
####################
#Create virutal raster for MHC_mtm7
# mhc_list_files = list.files('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MHC', pattern = '*.tif', full.names = T)
# mhc_list_files=mhc_list_files[-35]
# mhc_output = 'D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MHC/mhc_mtm7.vrt'
# 
# terra::vrt(x = mhc_list_files, filename =  mhc_output)
# vrt_7 = rast(mhc_output)
########################################
#Load data 
mhc_7 = rast('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MHC/mhc_mtm7.vrt')
v_units = vect("C:/Meghana/Belgique/decembre/data/Validation/validation_units2.sqlite")
v_units_tree = terra::project(v_units, crs(mhc_7))
mhc_7_crop = terra::crop(mhc_7, v_units_tree)
urb_mask_vect = vect('C:/Meghana/Belgique/decembre/traitements/Validation1/Ut_anthro_vec.shp')

v_units_tree = TreeStats(UREC_merge = v_units_tree, raster_file = mhc_7_crop, EPSG = "EPSG: 2949", urban_vector_mask =urb_mask_vect )
writeVector(v_units_tree,'C:/Meghana/Belgique/decembre/traitements/Validation1/units_treeHeight.sqlite', filetype = 'SQLITE')

######################
#Indice de naturalite
#Load data 
#raster and units data already loaded 
csv_correspondance_indNat = read.csv2("C:/Meghana/donnee_brutes/correspondance_indiceNat_ut19_10mV3.csv")
csv_correspondance_indNat$CODE_UT = as.numeric(csv_correspondance_indNat$CODE_UT)
v_indNat = Indice_Naturalite_functions(UREC_merge = v_units1, raster_file = raster_surface, csv_class_correspondence = csv_correspondance_indNat)
st_write(v_indNat, 'C:/Meghana/Belgique/decembre/traitements/Validation1/units_indnat.sqlite')

#######################
#Largeur laterale
v_units = vect("C:/Meghana/Belgique/decembre/data/Validation/validation_units2.sqlite")
v_units$meanwidth = 20










#########################
# Metriques taux de dispersion 
#load data and mask for forests and MH 
raster_file_fmh = rast("C:/Meghana/Belgique/decembre/traitements/Validation1/clip_UT_FMH2.tif")
raster_file_fmh
non_vegopt_mask = st_read("C:/Meghana/Belgique/decembre/traitements/Validation1/Ut_nonVegopt_vec.shp")
t = raster_file_fmh

new_values <- setValues(t, 1)
t <- new_values
t = terra::mask(t, non_vegopt_mask,inverse = T, updatevalue = 2)
t[t==2]<-NA

test_units = st_read("C:/Meghana/Belgique/decembre/traitements/Validation1/test_polygones_pd.shp")
v_pd = fragstat_function(raster_file = raster_file_fmh, UREC_merge = test_units, occ_sol = occ_sol)

v_units = st_read("C:/Meghana/Belgique/decembre/data/Validation/validation_units2.sqlite")
v_units_Pd = fragstat_function(raster_file = raster_file_fmh, UREC_merge = v_units, occ_sol = 'vegopt')

v_units_Pd2 = fragstat_function(raster_file = t, UREC_merge = v_units, occ_sol = 'vegopt')
v_units_Pd3 = v_units_Pd2
st_write(v_units_Pd2, 'C:/Meghana/Belgique/decembre/traitements/Validation1/units_pd.sqlite')


############################
#Metric overhanging canopy 
##########################################################
#Lets extract the area covered by vegetation in the water units 
#Load data

water_units = vect('C:/Meghana/Belgique/decembre/traitements/Validation1/water_units4.sqlite.shp')

mch_mtm7 = terra::rast('D:/Meghana/Ordi_CERFO/CERFO_LP010/Meghana/Belgique/donnees_brutes/LiDAR/MHC/mhc_mtm7.vrt')
EPSG = "EPSG: 32187"
Urban_vector_mask = vect('C:/Meghana/Belgique/decembre/traitements/Validation1/Ut_anthro_vec.shp')

water_units_temp = OverhangingCanopy(UREC_water = water_units, raster_file = mch_mtm7, EPSG = EPSG, urban_vector_mask = Urban_vector_mask)
water_units_temp1 = water_units_temp[,-1]
writeVector(water_units_temp1, 'C:/Meghana/Belgique/decembre/traitements/Validation1/water_units_overhanging.shp', filetype = 'ESRI Shapefile', overwrite = T)

#Combine this data based on a common unique id to units data. Here it is probably based on rive_units and rive
water_units_temp1_sf = st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/water_units_overhanging.shp')
water_units_temp1_sf = st_drop_geometry(water_units_temp1_sf)
units_temp = left_join(units, water_units_temp1_sf, by = c('rive'='rive_unit') )
# Remove all rows with NA in the "x" column using subset()
units_temp1 <- subset(units_temp, !is.na(canopyRati))
st_write(units_temp1, 'C:/Meghana/Belgique/decembre/traitements/Validation1/units_overhangingCan.sqlite')

############################################################################################################################
############################################################################################################################
############################################################################################################################
#Comparison of ISEER and Max index and IQBR

#Lets put all the data together, Normalize the metrics and calculate ISEER. 
#Load data and fix data
# ISEER1_v$iseer1 = (ISEER1_v$overhanging + ISEER1_v$vegetation_nrm +
#                      ISEER1_v$inv_pd_vegopt + ISEER1_v$inv_pente + 
#                      ISEER1$meanheight_nrm+ ISEER1_v$hepb_nrm + 
#                      ISEER1_v$meanwidth_nrm + ISEER1$indice_nat_nrm) /8
units_overhanging = st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/units_overhangingCan.sqlite')
keep_overhanging = c('rive', 'id_unit', 'id_uea_uni','canopyrati')
units_overhanging = units_overhanging[,names(units_overhanging)%in% keep_overhanging]
  
  
  
units_surface = st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/units_surface.sqlite')
keep_surf = c('rive','id_uea',  'id','vegetation' )
units_surface = units_surface[,names(units_surface)%in% keep_surf]
units_surface = st_drop_geometry(units_surface)


units_pd = st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/units_pd.sqlite')
keep_pd = c("rive" , "id_uea", "id","pd_vegopt")
units_pd = units_pd[,names(units_pd)%in% keep_pd]
units_pd = st_drop_geometry(units_pd)


units_hepb = st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/units_Hepb.sqlite')
units_hepb = st_drop_geometry(units_hepb)


units_mean_height = st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/units_treeHeight.sqlite')
drop_height = c('medianheight', 'sdheight', 'majorityheight')
units_mean_height = units_mean_height[,!names(units_mean_height)%in% drop_height]
units_mean_height = st_drop_geometry(units_mean_height)

units_indNat= st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/units_indnat.sqlite')
units_indNat = st_drop_geometry(units_indNat)

units_slope = st_read('C:/Meghana/Belgique/decembre/traitements/Validation1/units_avrslope.sqlite')
units_slope = st_drop_geometry(units_slope)


#####################################
#join all the datasets together

units_iseer <- left_join(units_overhanging, units_indNat, by = "rive") %>%
  select(rive,id_unit,id_uea_uni,canopyrati,indice_nat)

units_iseer <- left_join(units_iseer, units_mean_height, by = "rive") %>%
  select(rive,id_unit,id_uea_uni,canopyrati,indice_nat,meanheight )

units_iseer <- left_join(units_iseer, units_hepb, by = "rive") %>%
  select(rive,id_unit,id_uea_uni,canopyrati,indice_nat,meanheight,hepb )
units_iseer <- left_join(units_iseer, units_pd, by = "rive") %>%
  select(rive,id_unit,id_uea_uni,canopyrati,indice_nat,meanheight,hepb,pd_vegopt )

units_iseer <- left_join(units_iseer, units_surface, by = "rive") %>%
  select(rive,id_unit,id_uea_uni,canopyrati,indice_nat,meanheight,hepb,pd_vegopt,vegetation )


units_iseer <- left_join(units_iseer, units_slope, by = "rive") %>%
  select(rive,id_unit,id_uea_uni,canopyrati,indice_nat,meanheight,hepb,pd_vegopt,vegetation,avrslope )



units_iseer$mean_width= 20 #add mean width of units which is already known. 

#Write the metrics out 
st_write(units_iseer,'C:/Meghana/Belgique/decembre/traitements/Validation1/units_iseer.sqlite')

##################################################
#Do normalization of function
units_iseer_nrm = units_iseer
units_iseer_nrm <- rename(units_iseer_nrm, id_uea = id_uea_uni, id = id_unit)
units_iseer_nrm = Normalization_function(units_iseer_nrm)
units_iseer_nrm$inv_pd_vegopt_nrm = -units_iseer_nrm$pd_vegopt_nrm +1
units_iseer_nrm$inv_slope_nrm = - units_iseer_nrm$avrslope_nrm +1

st_write(units_iseer_nrm,'C:/Meghana/Belgique/decembre/traitements/Validation1/units_iseer_nrm_all_metrics.sqlite')

keep_nrm = c("rive", "id","id_uea",
             "canopyrati_nrm","indice_nat_nrm",
             "meanheight_nrm",    "hepb_nrm" ,
             "vegetation_nrm" , "mean_width_nrm",
             "inv_pd_vegopt_nrm","inv_slope_nrm")

units_iseer_nrm_m1 = units_iseer_nrm[,names(units_iseer_nrm)%in% keep_nrm]
units_iseer_nrm_m1$inv_pd_vegopt_nrm[is.na(units_iseer_nrm_m1$inv_pd_vegopt_nrm)] <- 0
units_iseer_nrm_m1[is.na(units_iseer_nrm_m1)] <- 0

units_iseer_nrm_m1$iseer1 = (units_iseer_nrm_m1$canopyrati_nrm+units_iseer_nrm_m1$indice_nat_nrm+
                               units_iseer_nrm_m1$meanheight_nrm+units_iseer_nrm_m1$hepb_nrm+
                               units_iseer_nrm_m1$vegetation_nrm+units_iseer_nrm_m1$mean_width_nrm+
                               units_iseer_nrm_m1$inv_pd_vegopt_nrm+ units_iseer_nrm_m1$inv_slope_nrm)/8


windows(5,10)
plot(units_iseer_nrm_m1$iseer1)
max(units_iseer_nrm_m1$iseer1)
