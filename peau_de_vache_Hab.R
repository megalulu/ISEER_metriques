#Peaux de vahce Creation et maintient de l'habitat 
unit = vect('C:/Meghana/Peau_de_vache/emprise3.shp')
unit_sf = st_read('C:/Meghana/Peau_de_vache/emprise3.shp')
raster_clip = raster('C:/Meghana/Peau_de_vache/raster3.tif')
unit_rast = rasterize(unit_sf, raster_clip)
raster_clip= terra::mask(raster_clip,unit_rast ) #Reduce the extent of the ESA raster to the extent of 


mask_urbain1 = st_read('C:/Meghana/Belgique/decembre/data/mask_urbain_1.shp')
mask_urbain1 = st_transform(mask_urbain1, crs(raster_clip))
mask_urbain1 = vect(mask_urbain1)
#########################################


###Forest
#Create data 1. Utilisation du terrtioire, 2. MHC,
#1. Utilisation du territoire forest
peau_forest = raster_clip
peau_forest[,]<-4
#2. MHC
peau_F_height = raster_clip
peau_F_height[,]<-sample(1.55:30,26040, replace =T)
#Calculate metrics : 
Forest_vegRatio_optimale = VegetationRatio(UREC_merge = unit, raster_file = peau_forest)
Forest_CanRatio = CanopyRatio(UREC_merge = unit, raster_file = peau_F_height, EPSG = 'EPSG:32198', urban_vector_mask =mask_urbain1 )
Forest_mean_height = TreeStats(UREC_merge = unit, raster_file = peau_F_height, EPSG = 'EPSG:32198', urban_vector_mask =mask_urbain1 )
pdv_forest = Forest_vegRatio_optimale
pdv_forest$CanRatio = Forest_CanRatio$CanopyRatio
pdv_forest$mean_height = Forest_mean_height$meanHeight
pdv_forest$Anthropique = 0
pdv_forest$name = 'Forest'



##Urbain
#Create data 1. Utilisation du territoire, 2. MHC
#1. Utilisation du territoire urbain
peau_urbain = raster_clip
peau_urbain[,]<-1
#2. MHC
peau_U_height = raster_clip
peau_U_height[,]<-sample(0:1.5,26040, replace =T)
#Calculate metrics 
pdv_urbain = unit
pdv_urbain$VegRatio = 0
pdv_urbain$CanRatio = 0
pdv_urbain$mean_height = 0                         
pdv_urbain$Anthropique = 1
pdv_urbain$name = 'Urban'


##Agricole
#Create data 1. Utilisation du territoire, 2. MHC
#1. Utilisation du territoire urbain
peau_agricole = raster_clip
peau_agricole[,]<-2
#2. MHC
peau_A_height = raster_clip
peau_A_height[,]<-sample(0:1.5,26040, replace =T)
#Calculate metrics 
agriculture_VegRatio = VegetationRatio(UREC_merge = unit, raster_file = peau_agricole)
Agriculture_CanRatio = CanopyRatio(UREC_merge = unit, raster_file = peau_A_height, EPSG = 'EPSG:32198', urban_vector_mask = unit)
agriculture_mean_height = TreeStats(UREC_merge = unit, raster_file = peau_A_height, EPSG = 'EPSG:32198', urban_vector_mask = unit)
#Put data into one shapefile
pdv_agricole = agriculture_VegRatio
pdv_agricole$CanRatio = Agriculture_CanRatio$CanopyRatio
pdv_agricole$mean_height = 0                         
pdv_agricole$Anthropique = 0
pdv_agricole$name = 'Agricole'

## moitier Agricole moitier foret
#1. Create data : Utilisation du territoire, moitier agricole, moitier foret
peau_AF1 = raster_clip 
peau_AF1[,1:155]<-NA
peau_AF1[,156:310]<-4
plot(peau_AF1)
#2. MHC 
peau_AF1_height = raster_clip
peau_AF1_height[,1:155]<- sample(0:1.5, 13020, replace = T)
peau_AF1_height[,156:310]<-sample(1.5:30, 13020, replace = T)
plot(peau_AF1_height)
#Calculate metrics 
AF1_VegRatio = VegetationRatio(UREC_merge = unit, raster_file = peau_AF1)
AF1_CanRatio = CanopyRatio(UREC_merge = unit, raster_file = peau_AF1_height, EPSG = 'EPSG:32198', urban_vector_mask = mask_urbain1)
AF1_mean_height = TreeStats(UREC_merge = unit, raster_file = peau_AF1_height, EPSG = 'EPSG:32198', urban_vector_mask = mask_urbain1)
#Put data into one shapefile
pdv_AF1 =AF1_VegRatio
pdv_AF1$CanRatio = AF1_CanRatio$CanopyRatio
pdv_AF1$mean_height = AF1_mean_height$meanHeight
pdv_AF1$Anthropique = 0
pdv_AF1$name = 'half forest/agriculture'


#All values that are not forest, should be set to NA
peau_AF2[peau_AF2 == 0]<-NA

#2. MHC 
peau_AF2_height = peau_AF2
# Set the values of all pixels with a value of 1 to a random number between 1.5 and 30
peau_AF2_height[peau_AF2_height == 4] <- runif(length(peau_AF2_height[peau_AF2_height == 4]), 1.5, 30)
plot(peau_AF2_height)
#Calculate metrics 
AF2_VegRatio = VegetationRatio(UREC_merge = unit, raster_file = peau_AF2)
AF2_CanRatio = CanopyRatio(UREC_merge = unit, raster_file = peau_AF2_height, EPSG = 'EPSG:32198', urban_vector_mask = mask_urbain1)
AF2_mean_height = TreeStats(UREC_merge = unit, raster_file = peau_AF2_height, EPSG = 'EPSG:32198', urban_vector_mask = mask_urbain1)
#Put data into one shapefile
pdv_AF2 =AF2_VegRatio
pdv_AF2$CanRatio = AF2_CanRatio$CanopyRatio
pdv_AF2$mean_height = AF2_mean_height$meanHeight
pdv_AF2$Anthropique = 0
pdv_AF2$name = 'aleatoire half forest/agriculture'


#######
## moitier Agricole moitier urbain
#1. Create data : Utilisation du territoire, moitier agricole, moitier urbain
peau_AU1 = raster_clip 
peau_AU1[,1:155]<-NA
peau_AU1[,156:310]<-1
plot(peau_AU1)
#2. MHC  == 0 parceque pas de foret

#Calculate metrics 
#no need because most are related to forest attributes which are not available in this case 
#Put data into one shapefile
pdv_AU1 =unit
pdv_AU1$VegRatio = 0
pdv_AU1$CanRatio =0
pdv_AU1$mean_height =0
pdv_AU1$Anthropique = 0.5
pdv_AU1$name = 'half urbain/agriculture'

####
## Moitier Agricole moitier urbain aleatoire
#1. Create data : Utilisation du territoire, moitier agricole, moitier urbain
peau_AU2 = raster_clip 
plot(peau_AU2)
set.seed(123) # Set the random seed for reproducibility
values <- c(rep(0, ncell(peau_AU2)/2), rep(1, ncell(peau_AU2)/2))
values <- sample(values) # Shuffle the values randomly
peau_AU2=setValues(peau_AU2, values)
plot(peau_AU2)
#All values that are not forest, should be set to NA
peau_AU2[peau_AU2 == 0]<-NA
#2. MHC  == 0 parceque pas de foret

#Calculate metrics 
#no need because most are related to forest attributes which are not available in this case 
#Put data into one shapefile
pdv_AU2 =unit
pdv_AU2$VegRatio = 0
pdv_AU2$CanRatio =0
pdv_AU2$mean_height =0
pdv_AU2$Anthropique = 0.5
pdv_AU2$name = 'aleatoire half urbain/agriculture'













#Put all the pdv together
pdv_hab1 = rbind(pdv_forest, pdv_urbain)
pdv_hab1 = rbind(pdv_hab1, pdv_agricole)
pdv_hab1 = rbind(pdv_hab1, pdv_AF1)
pdv_hab1 = rbind(pdv_hab1, pdv_AF2)
pdv_hab1 = rbind(pdv_hab1, pdv_AU1)
pdv_hab1 = rbind(pdv_hab1, pdv_AU2)
View(as.data.frame(pdv_hab1))

#Normalize pdv_metrics 
pdv_hab1_nrm = st_as_sf(pdv_hab1)
pdv_hab1_nrm = subset(pdv_hab1_nrm, select = -c(name))#Remove name column to run normalization function
pdv_hab1_nrm = Normalization_function(pdv_hab1_nrm)
pdv_hab1_nrm$name = pdv_hab1$name#add name column back

#Create indice de fonction ecologique 
#Inverser les valeurs d'antrhopique nrm 
pdv_hab1_nrm$inv_anthropique_nrm = -pdv_hab1_nrm$Anthropique_nrm +1
pdv_hab1_nrm$FE1 = (pdv_hab1_nrm$inv_anthropique_nrm+pdv_hab1_nrm$mean_height_nrm+
                      pdv_hab1_nrm$CanRatio_nrm+pdv_hab1_nrm$VegRatio_nrm)/4
  
plot(pdv_hab1_nrm$FE1, xlab = pdv_hab1_nrm$name)

# Create plot with row as x-axis and value as y-axis
ggplot(pdv_hab1_nrm, aes(x = name, y = FE1)) +
  geom_point(stat = "identity") +
  xlab("name") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))#change the angle of the x lables

