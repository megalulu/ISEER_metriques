#THIS SCRIPT CONTAINS FUNCTIONS TO CREATE SAMPLING PLOTS FOR UREC
#You need : UREC, centerline axis, 

SamplingPlots <- function (UREC, axe){
  axe = st_transform(axe, crs(UREC))
  axe = st_as_sfc(axe)
  axe = st_cast(axe, to = 'LINESTRING')
  
  #Create polygone de voronoi for each feature in UREC_new
  semis = st_line_sample(axe, density = 0.02, type = 'regular') #create sample points every 50m
  voronoi= st_voronoi(semis , bOnlyEdges = T)
  
}

