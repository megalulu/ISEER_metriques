#Splitting polygons 

drummondville = st_read('C:/Meghana/donnees_Camille/UREC_Camille/ELRfixgeom_drummondville.shp')
plot(drummondville$geometry)
uea_drommondville = st_read('C:/Meghana/donnees_Camille/UREC_Camille/UEA_drummondville_new.shp')
plot(uea_drommondville$geometry, add = T, col = 'blue')

drummondville_split = st_split(drummondville, uea_drommondville)
split_poly <- drummondville_split %>% 
  st_collection_extract(c("POLYGON"))
plot(split_poly$geometry)
st_write(split_poly, 'C:/Meghana/donnees_Camille/UREC_Camille/R_split_TBD/drummondville_split1.shp')
save.image()
