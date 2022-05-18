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

#Step 1: Load UREC data and merge shapfiles into one 
# define working directory (to adapt)
setwd("C:/Meghana/Belgique")
path0='C:/Meghana/Belgique'
#load UEA data
file_in = paste0(path0, '/UEAf_merge/UEAf_merge.shp')
UEA = st_read(file_in, stringsAsFactors = F)
#load UREC data
file_in = paste0(path0, '/results/UREC_merge.shp')
UREC_merge = st_read(file_in, stringsAsFactors = F)

#Put the UREC shapefiles in a list
#file_list = list.files(path0, pattern = '*.shp', full.names = TRUE)
#read all the shapefiles and store them in a  list
#shapefile_list = lapply(file_list, FUN = read_sf)
#merge all the shapefiles into one new variable that can be written out
#UREC <- do.call(st_union, shapefile_list)
#verify validity of the geometry
test = st_is_valid(UREC_merge, reason = T)
table(test)
#correct the geometry 
UREC_merge = st_make_valid(UREC_merge)#only do this if valid geometry finds errors
test = st_is_valid(UREC_merge, reason = T)#test the geometry again 
table(test)
st_write(UREC_merge, 'C:/Meghana/Belgique/results/UREC_merge.shp', overwrite = T)



#clip you UEA based on the UREC_merge shapefile
UEA_clip = st_intersection(UEA, UREC_merge)
test = st_is_valid(UEA_clip, reason = T)
table(test)
UEA_clip= st_make_valid(UEA_clip)
test = st_is_valid(UEA_clip, reason = T)
table(test)
#do for loop to deal with different geometries in UEA_clip
i = 2
for (i in UEA_clip){
  UEA = UEA_clip[i]
  if (class(UEA) == 'GEOMETRYCOLLECTION'){
    UEA = st_cast(UEA, to = 'LINESTRING')
  }
    
}
#Write the clip UEA file to 
st_write(UEA_clip, 'C:/Meghana/Belgique/results/UEA_clip.shp', overwrite= T)


######################################
#Do dissagregation steps : 
######################################
#Read data made in Qgis
axe = st_read('centerline_DISSOLVED.shp') #centerline
st_crs(axe) = 32198 #set projection to NAD83 Quebec conique conforme de Lambert
UREC = st_read('UREC.shp') #UREC
st_crs(UREC) = 32198
#select one side fo UREC for test
UREC1 = UREC[UREC$OBJECTID_1==3,]

UREC_union = st_union(UREC)
plot(st_geometry(UREC_union))
#Sample points along line every 50m
semis = st_line_sample(axe, density = 0.02, type = 'regular')
#create polygon of voronoi at each point and cast it to have the same geometry
poly_voronoi = st_voronoi(semis) %>% st_cast()
#cross the UREC data with the polygon of voronoi to obtain sampling plots
sampling = st_intersection(UREC1, poly_voronoi)
plot(st_geometry(sampling))
