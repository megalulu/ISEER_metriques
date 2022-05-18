#Tests for clipping stuff 
library(sf)
library(terra)



setwd = 'C:/Meghana/Belgique'
path_vrt = paste0(getwd(), '/donnees_brutes/ESA_WORLDCOVER/clipWorldcover/esa_clip_NAD83.vrt')
vrt = rast(path_vrt)
plot(vrt)
path_polygon = paste0(getwd(), '/traitements/results/UREC_rives_new/27917246329c4218a2cf6562ac89d22e_rive13.shp')
polygon = vect(path_polygon)
plot(polygon, add = T)
vrt_crop = terra::crop(vrt, polygon)
plot(vrt_crop)
plot(polygon, add = T)
polygon_raster = terra::rasterize(polygon,vrt_crop)
plot(polygon_raster)
vrt_mask = terra::mask(vrt_crop, polygon_raster)
plot(vrt_mask)
vrt_mask[vrt_mask != 10] <- NA # replace what is not forest by NA
vrt_mask[vrt_mask == 10] <- 1 # replace 10 (forest) by 1
