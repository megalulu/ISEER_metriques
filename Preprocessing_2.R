#Pre-Processing for new UREC 

#Setting up UREC_Cammille and UREC_Maxime 
#Load data 
UREC_c = st_read('C:/Meghana/donnees_Camille/UREC_new2/OUTPUTS/UREC_Camille.shp')
UREC_m = st_read('C:/Meghana/donnees_Maxime/OUTPUT/UREC_maxime.shp')
#Select columns that we care about
UREC_c = UREC_c[c("Id_UEA","rive")]
UREC_m = UREC_m[c('Id_UEA', 'rive')]
#Add unique values for rive column
UREC_c$rive =seq.int(nrow(UREC_c))
UREC_m$rive =seq.int(nrow(UREC_m))
#transform CRS to match e
UREC_m = st_transform(UREC_m, crs(UREC_c))
UREC_new = rbind(UREC_c, UREC_m)
plot(UREC_new$geometry)
st_is_valid(UREC_new)
UREC_new = st_make_valid(UREC_new)
#Create new field with unique Id (combination of Id_UEA and rive)
UREC_new$id = paste0(UREC_new$Id_UEA, '_rive', UREC_new$rive)
UREC_new = st_cast(UREC_new, to ='MULTIPOLYGON')
names(UREC_new)
st_is_valid(UREC_new)
#Write file to fodler
st_write(UREC_new, 'C:/Meghana/Belgique/traitements/data/UREC_MaxCam/UREC_MaxCam1.shp', delete_layer = T)


####################################
#New UREC MaxCam with non erronus UREC
UREC_new = st_read( 'C:/Meghana/Belgique/traitements/data/UREC_MaxCam/UREC_MaxCamValid.shp')
UREC_new = subset(UREC_new, select = -c(ID_1) )
UREC_new$id = paste0(UREC_new$Id_UEA, '_rive', UREC_new$rive)
st_is_valid(UREC_new)
UREC_new= UREC_new[-c(30, 34, 103, 118, 143, 146, 141, 142),]

st_write(UREC_new, 'C:/Meghana/Belgique/traitements/data/UREC_MaxCam/UREC_MaxCamValid3.shp', delete_layer = T) #THis is a good file!
st_write(UREC_new, 'C:/Meghana/Belgique/traitements/UREC_MaxCam/results_MAxCam.shp', delete_layer = T) #THis is a good file!



i=1
for (i in 1:nrow(UREC_new)){
  rive = UREC_new[i,]
  name = paste0('C:/Meghana/Belgique/traitements/UREC_MaxCam/UREC_MaxCam_rive/', rive$id, '.shp')
  st_write(rive, name, delete_layer  = T)
}

#Load UEA_Cam and UEA_Max
UEA_c = st_read('C:/Meghana/Belgique/traitements/data/UREC_MaxCam/UEA_Cam.shp')
UEA_m =st_read('C:/Meghana/Belgique/traitements/data/UREC_MaxCam/UEA_Max_new.shp')
#Extract only Id_UEA column
UEA_c = UEA_c[c('Id_UEA')]
UEA_m = UEA_m[c('Id_UEA')]
#Merge both UEA into one file
UEA_MaxCam = rbind(UEA_c, UEA_m)
#write out each individual UEA to file 
i = 1
for (i in 1:nrow(UEA_MaxCam)){
  uea = UEA_MaxCam[i,]
  name = paste0('C:/Meghana/Belgique/traitements/UREC_MaxCam/UEA/', UEA_MaxCam[i,]$Id_UEA, '.shp')
  st_write(uea, name, delete_layer  = T)
}
#Write out merge UEA file
st_write(UEA_MaxCam,'C:/Meghana/Belgique/traitements/data/UREC_MaxCam/UEA_MaxCam.shp', delete_layer = T )

axe = st_read('C:/Meghana/Belgique/traitements/UREC_MaxCam/axe_regroupe.shp')
axe = st_as_sfc(axe)
axe = st_cast(axe, to = 'LINESTRING')


i =118 #i that do not work : 30, 34, 103, 118, 143, 146, 141, 142 #
#Ran this for loop untill it didn't work with UREC_MaxCam_valid.shp 
#Removed the row that doesn work. Reran the for loop untill it didn't work. 
#TODO Need to implement try function 

missing_sampling <- list()

for (i in 1:nrow(UREC_new)){
  rive = UREC_new[i,]
  print(i)
  name = paste0('C:/Meghana/Belgique/traitements/UREC_MaxCam/axe/', rive$id, '.shp')
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
    st_write(rive_sampling,paste0('C:/Meghana/Belgique/decembre/data/sampling/', rive$id, '.shp'), delete_layer = T)
  }else {
    missing_sampling <- append(missing_sampling, i)
    
 }

}

UREC_new[141,]


axe = st_as_sfc(axe)
axe = st_cast(axe, to = 'LINESTRING')
semis = st_line_sample(axe, density = 0.02, type = 'regular')
semis
semis = st_cast(semis, to ='POINT')

semis_intersection = st_intersection(UREC[1,], semis)
semis_intersection_sfg = st_as_sfc(semis_intersection)
voronoi = st_voronoi(semis_intersection_sfg)


plot(voronoi$geometry)
st_write(semis,'C:/Meghana/Belgique/traitements/UREC_MaxCam/semis2.shp') 

voronoi = st_voronoi(semis) 
voronoi = st_cast(voronoi, to = 'MULTIPOLYGON')
voronoi = st_transform(voronoi, crs(UREC_new))
voronoi = st_cast(voronoi, to = 'POLYGON')
st_write(voronoi,'C:/Meghana/Belgique/traitements/UREC_MaxCam/voronoi1.shp')

UREC_new_full = st_union()
i = 2
for (i in nrow(UREC_new)){
  UREC = UREC_new[i,]
  UREC_sampling = st_intersection(UREC,voronoi)
  plot(UREC_sampling$geometry)
}
voronoi_lines <- st_intersection(voronoi , UREC_new)


