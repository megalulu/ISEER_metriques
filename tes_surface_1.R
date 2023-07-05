#Surface des UT sans la zones eau sont deja calculer dans maintient et creation d'habitat
#Load data 
UREC_surface = st_read('C:/Meghana/Belgique/decembre/traitements/fonction_productivite/prod_sans_eau/UREC_surfaces_UT_sans_eau.sqlite')#This seeems wrong...
##New set of surface classe 
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water.shp')

ut19_full= raster('C:/Meghana/Belgique/decembre/data/raster/ut19_mask.tif')
csv__class_correspondence = read.csv2("C:/Meghana/donnee_brutes/UT_2019_10m.csv")
#UREC_merge = UREC_merge[145,]
UREC_surface_class = SurfaceClass(UREC_merge = UREC_merge, csv_class_correspondence = csv__class_correspondence,
                                  raster_file = ut19_full)
st_write(UREC_surface_class,'C:/Meghana/Belgique/decembre/traitements/UREC_surface_new.sqlite', delete_layer = T)
l=0
