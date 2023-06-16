#Lenght test
#UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1.shp')
UREC_merge = st_read('C:/Meghana/Belgique/decembre/data/UREC_mergeValid1_water.shp')#this is all the urec cut to exclud area with water

UEA_merge = st_read('C:/Meghana/Belgique/decembre/data/UEA_merge_new.shp')
#semis = st_read('C:/Meghana/Belgique/decembre/traitements/semis1.shp')
voronoi = st_read('C:/Meghana/Belgique/decembre/traitements/voronoi1.shp')
#UREC_merge = UREC_merge[1:2,]
i = 1

LateralWidthFunction <- function(UREC_merge, UEA_merge, voronoi){
UREC_merge$meanwidth = NA
for (i in 1:nrow(UREC_merge)){
  print(paste0('measuring ', UREC_merge[i,]$id))
  rive = UREC_merge[i,]
  Id_uea=  rive$Id_UEA
  #Create a query to find the appropraite UEA 
  query = UEA_merge$Id_UEA == Id_uea
  uea = filter(UEA_merge, query) #Find the correct UEA based on Id_uea
  uea = st_cast(uea, to = 'LINESTRING')
  class_uea = class(uea)
  #intersection between poly voronoi and UREC
  UREC_voronoi = st_intersection(voronoi, rive)
  UREC_voronoi_line = st_cast(UREC_voronoi, to = 'LINESTRING')
  widths = st_length(UREC_voronoi_line)
  average_widths = mean(widths)
  UREC_merge[i,]$meanwidth = average_widths
}
return(UREC_merge)
}

  

# 
 UREC_merge = UREC_merge[1,]
# UREC_merge = UREC_merge[1,]
# UEA_merge = UEA_merge[61,]
UREC_merge_width = LateralWidthFunction(UREC_merge = UREC_merge, UEA_merge = UEA_merge, voronoi = voronoi)
st_write(UREC_merge_width,'C:/Meghana/Belgique/decembre/traitements/fonction_habitat/UREC_Water/UREC_lateral_width.shp' )


# n_occur = data.frame(table(UREC_merge$Id_UEA))
# doubles = n_occur[n_occur$Freq!=2,]


#uea61
#urec69