#Some test for canope en surplomb sur la riviere

#We are going to redo largeur riviere stuff because i'm not convinced we did it right in main
#read data of pts ref
PtRef03 <- st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb", #this may need to change depending on the UDH
                   layer = "PtRef",
                   stringsAsFactors = F)

#Read new UEA_with new Id (that do not match our UREC, because they updated the fucking primary key!)
UEA_03 = st_read("C:/Meghana/Belgique/decembre/data/CRHQ_03_UEA_clip_full.shp")
#intersect pts and UEA 
PtRef03_clip = st_intersection(PtRef03, UEA_03)
PtRef03_clip_v = vect(PtRef03_clip)
#read table that has info about river width
PtRef_mod_lotique03 = st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro03.gdb", 
                              layer = "PtRef_mod_lotique",
                              stringsAsFactors = F)
#Join table with river info and PtRef cliped based on Id_ptRef
PtRef03_mod_lotique_join = left_join(PtRef03_clip, PtRef_mod_lotique03, by = c('Id_PtRef' = 'Id_PtRef'))
#extract only the columns we need (Id_UEA, Id_PtRef and Largeur_mod)
PtRef03_mod_lotique_join = subset(PtRef03_mod_lotique_join, select = c(Id_PtRef, Id_UEA,
                                                                   Largeur_mod))
PtRef03_mod_lotique_join = subset(PtRef03_mod_lotique_join, Largeur_mod != -999) #remove rows = -999 in column of interest (Largeur_mod)
PtRef03_mod_lotique_join = subset(PtRef03_mod_lotique_join, !is.na(Largeur_mod))

largeur_riviere_03= PtRef03_mod_lotique_join %>% dplyr::group_by(Id_UEA) %>% dplyr::summarize(river_width = mean(Largeur_mod)) #group table by Id_UEA and summarize the column Largeur_mod with median function
largeur_riviere_03_v = vect(largeur_riviere_03)
writeVector(largeur_riviere_03_v, "C:/Meghana/Belgique/decembre/data/River_width/pt_ref03_largeur_riviere.shp") #write the table containing (Id_UEA and median river_width)

largeur_riviere_03 = st_drop_geometry(largeur_riviere_03)
#Join table of median width grouped-by Id_UEA with the UEA table
UEA03_larg_mod = dplyr::left_join(UEA_03,largeur_riviere_03, by=c('Id_UEA'='Id_UEA')) #choose correct Primary Key!!
UEA03_larg_mod_na = subset(UEA03_larg_mod, !is.na(river_width))

#Create buffer of rivers 
buffer03 = st_buffer(UEA03_larg_mod_na, dist = UEA03_larg_mod_na$river_width)
st_write(buffer03, "C:/Meghana/Belgique/decembre/data/River_width/buffer03.shp" )

############DO it all over for CRHQ 05################
PtRef05 <- st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb", #this may need to change depending on the UDH
                   layer = "PtRef",
                   stringsAsFactors = F)

UEA_05 = st_read("C:/Meghana/Belgique/decembre/data/CRHQ_05_UEA_clip.shp")

PtRef_mod_lotique05 = st_read(dsn = "C:/Meghana/donnee_brutes/CRHQ/CRHQ_RegHydro05.gdb", 
                              layer = "PtRef_mod_lotique",
                              stringsAsFactors = F)


