#Script to make index functions 

#Indice de fonction ecologique 
test = stats_contH1
UREC_merge = test
poids = 8
col_names = list("id_uea", "rive", "id","continuity_vegetation_nrm",
"area_vegetation_nrm" ,
"continuity_forest_nrm",
"continuity_agriculture_nrm",
"continuity_mh_nrm",
"pd_forest_nrm" ,
"area_agriculture_nrm",
"pd_agriculture_nrm")

name_indice = 'Indice_Continuite_Paysage'

n =1

IndiceFE <- function(UREC_merge,poids,col_names,  name_indice, ){
  name_indice 
  
  UREC_indice = UREC_merge[,c('id_uea', 'rive', 'id')]
  
  
  for (n in 1:nrow(UREC_merge)){
  UREC_merge[n,name_indic] = sum(UREC_indice[n,])/poids  
    
  }
  
}
