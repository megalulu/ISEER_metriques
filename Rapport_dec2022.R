#Rapport stats 

UREC_contP = st_read('C:/Meghana/Belgique/decembre/results/results_new1_indice.SQLite')
summary(UREC_contP$conp)
UREC_ombrage = st_read('C:/Meghana/Belgique/decembre/results/indiceOmbrage.SQLite')
summary(UREC_ombrage$canopyratio_new_nrm)
sd(UREC_ombrage$canopyratio_new_nrm)

summary(UREC_ombrage$meanombrage_adj_nrm)
sd(UREC_ombrage$meanombrage_adj_nrm)

ISEER = st_read('C:/Meghana/Belgique/decembre/results/UREC_ISEER.shp')
ISEER$ISEER
mean(ISEER$ISEER)
sd(ISEER$ISEER)
summary(ISEER$ISEER)
