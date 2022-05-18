#Libraries to upload
library(sp)
library(sf)
library(raster)
library(gdalUtils)
gdalibrary(dplyr)
library(units)
library(purrr)
library(lwgeom)
library(xlsx)
library (nngeo)
library(installr)
library(RSAGA)


#Metric : Forest conitnuity
##################################################################
#set directory 
setwd = 'C:/Meghana/Belgique'

#import new qgis worldcover data (was not able to do this in R)

#Make raster stack of worldmap ESA raster layers
path_esa = paste0(getwd(),"/donnees_brutes/ESA_WORLDCOVER") # directory with both ESA worldCover tiles(full tiles)
list_file=list.files(path_esa,pattern="*.tif$",full.names=TRUE) #put files in list
file_vrt=paste0(path_esa,"/esa.vrt") #create path for where to put virtual raster
gdalbuildvrt(gdalfile=list_file, output.vrt= file_vrt,overwrite=TRUE) #Create virtual raster from files in list 

esa = raster(file_vrt) #load virtual raster
esa = projectRaster(esa, crs = 32198)  #Reproject virtual raster (This step took many hours...)

#j'ai essayer de réduire la taille des rasters en clippant (dans QGIS) les tuiles à l'étendue des UREC
path_esa_clip = paste0(getwd(),"/donnees_brutes/ESA_WORLDCOVER/UREC_merge_clip") #directory with clipped ESA worldCover tiles 
list_file_clip = list.files(path_esa_clip, pattern = '*.tif$', full.names = T ) #put clipped files in list
file1_vrt = paste0(path_esa_clip,"/esa_clip.vrt")
gdalbuildvrt(gdalfile = list_file_clip,
             output.vrt = file1_vrt, overwrite = TRUE) #j'obtiens l'erreur suivante à cette étape 

esa_clip = raster(file1_vrt) #open virtual raster
esa_clip = projectRaster(esa_clip, crs = 32198) #reproject virtual raster
######################################################################################################################################################################
###################################################################################################################################################################### 
#Je n'ai pas réussis à faire la prochaine partie. Mais voici le code que je voulais utiliser pour la métriques de continuité forestières
# Mask all the pixels that are not forest and attribute a value of 1 to all pixels that are of class forest
mask_foret <- esa_clip
mask_foret[mask_foret == 10] <- 1 # replace 10 (forest) by 1
mask_foret[mask_foret != 1] <- NA # replace what is not a 1 par NA
file_mask_forest = paste0(path_esa_clip, '/esa_clip_mask_forest.tif')
writeRaster(
  mask_foret,
  file_mask_forest, overwrite = T
)

