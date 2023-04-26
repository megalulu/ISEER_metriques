#peau de vache_ProdEr
unit = vect('C:/Meghana/Peau_de_vache/emprise3.shp')
unit_sf = st_read('C:/Meghana/Peau_de_vache/emprise3.shp')
raster_clip = raster('C:/Meghana/Peau_de_vache/raster3.tif')
unit_rast = rasterize(unit_sf, raster_clip)
raster_clip= terra::mask(raster_clip,unit_rast ) #Reduce the extent of the ESA raster to the extent of 


mask_urbain1 = st_read('C:/Meghana/Belgique/decembre/data/mask_urbain_1.shp')
mask_urbain1 = st_transform(mask_urbain1, crs(raster_clip))
mask_urbain1 = vect(mask_urbain1)

#########################################
#Create data 

###Forest, slow slope, hydrolic conductivity high
#Create data 1. Utilisation du terrtioire, 2. pente, 3. Hydrolic conductivity
#1. Utilisation du territoire forest
peau_forest = raster_clip
peau_forest[,]<-4
#2. pente 
peau_F_slope = raster_clip
peau_F_slope[,]<-sample(1:5,26040, replace =T)
#3. Conductivite hydraulique eleve
peau_hydro_max = raster_clip
peau_hydro_max[,]<- 11.78
#Calculate metrics : 
pdv_Forest = unit
pdv_Forest$agricole = 0
pdv_Forest$anthropique = 0
Forest_slope = AverageSlope(UREC_merge = unit, slope_raster  = peau_F_slope)
pdv_Forest$slope = Forest_slope$avrSlope
pdv_Forest$HydCond = 11.78
pdv_Forest$name = 'Forest'
  
########
#Urbain, high slope, low hydrolic conductivity
#1. Utilisation du territoire urbain
peau_urbain = raster_clip
peau_urbain[,]<-1
#2. Pente 
peau_urbain_slope = raster_clip
peau_urbain_slope[,]<-sample(20:43, 26040, replace = T)
plot(peau_urbain_slope)
#3. Conductivity hydrolic low
peau_U_hydro = raster_clip
peau_U_hydro[,]<- 0.1

pdv_urbain= unit
pdv_urbain$agricole = 0
pdv_urbain$anthropique = 1
pdv_urbain_slope = AverageSlope(UREC_merge = unit, slope_raster = peau_urbain_slope)
pdv_urbain$slope = pdv_urbain_slope$avrSlope
pdv_urbain$HydCond = 0.1
pdv_urbain$name = 'Urbain'

########
#agriculture , high slope, low hydrolic conductivity
#1. Utilisation du territoire urbain
peau_agriculture = raster_clip
peau_agriculture[,]<-2
#2. Pente 
peau_agriculture_slope = peau_urbain_slope
peau_agriculture_slope[,]<-sample(20:43, 26040, replace = T)
#3. Conductivity hydrolic low
peau_A_hydro = raster_clip
peau_A_hydro[,]<- 0.1

pdv_agricole= unit
pdv_agricole$agricole = 1
pdv_agricole$anthropique = 0
pdv_agricole_slope = AverageSlope(UREC_merge = unit, slope_raster = peau_agriculture_slope)
pdv_agricole$slope = pdv_urbain_slope$avrSlope
pdv_agricole$HydCond = 0.1
pdv_agricole$name = 'Agriculture'




## moitier Agricole moitier foret, low slope, high hydrolic conductivity
#1. Create data : Utilisation du territoire, moitier agricole, moitier foret
peau_AF1 = raster_clip 
peau_AF1[,1:155]<-NA
peau_AF1[,156:310]<-4
plot(peau_AF1)
#2. Slope 
peau_AF1_slope = raster_clip
peau_AF1_slope[,]<- sample(1:5, 26040, replace = T)
plot(peau_AF1_slope)
#3. Hydrolic conductivit 
peau_AF1_HydroCond = 11.78

#Calculate metrics 
pdv_AF1= unit
pdv_AF1$agricole = 0.5
pdv_AF1$anthropique = 0
af1_slope = AverageSlope(UREC_merge = unit, slope_raster= peau_AF1_slope)
pdv_AF1$slope = af1_slope$avrSlope
pdv_AF1$HydCond = 11.78
pdv_AF1$name = 'half forest/agriculture and high hydrollic cond.'


## moitier Agricole moitier foret, low slope, low hydrolic conductivity
#everything stays the same as AF1 but hydrolic conductivity = 0.1
pdv_AF2 = pdv_AF1
pdv_AF2$HydCond = 0.1
pdv_AF2$name = 'Half forest/Agricole and low hydrolic cond'


####Moitier agricole moiter urbain with low slope and high hydroloc cond. 
#1. Create data : Utilisation du territoire, moitier agricole, moitier foret
peau_AU1 = raster_clip 
peau_AU1[,1:155]<-0
peau_AU1[,156:310]<-1
plot(peau_AU1)
#2. Slope 
peau_AU1_slope = raster_clip
peau_AU1_slope[,]<- sample(1:5, 26040, replace = T)
plot(peau_AU1_slope)
#3. Hydrolic conductivit 
peau_AU1_hydroCond = 11.78
#put data together into pdv
pdv_AU1= unit
pdv_AU1$agricole = 0.5
pdv_AU1$anthropique = 0.5
au1_slope = AverageSlope(UREC_merge = unit, slope_raster= peau_AU1_slope)
pdv_AU1$slope = au1_slope$avrSlope
pdv_AU1$HydCond = 11.78
pdv_AU1$name = 'half urbain/agriculture and high hydrollic cond.'



## moitier Agricole moitier urbain, low slope, low hydrolic conductivity
#everything stays the same as AF1 but hydrolic conductivity = 0.1
pdv_AU2 = pdv_AU1
pdv_AU2$HydCond = 0.1
pdv_AU2$name = 'Half urbain/Agricole and low hydrolic cond'











############################################
#Put all the pdv together
pdv_ProdER = rbind(pdv_Forest, pdv_urbain)
pdv_ProdER = rbind(pdv_ProdER, pdv_agricole)
pdv_ProdER = rbind(pdv_ProdER, pdv_AF1)
pdv_ProdER = rbind(pdv_ProdER, pdv_AF2)
pdv_ProdER = rbind(pdv_ProdER, pdv_AU1)
pdv_ProdER = rbind(pdv_ProdER, pdv_AU2)

View(as.data.frame(pdv_ProdER))

#Normalize pdv_metrics 
pdv_ProdER_nrm = st_as_sf(pdv_ProdER)
pdv_ProdER_nrm = subset(pdv_ProdER_nrm, select = -c(name))#Remove name column to run normalization function
pdv_ProdER_nrm = Normalization_function(pdv_ProdER_nrm)
pdv_ProdER_nrm$name = pdv_ProdER$name#add name column back
View(pdv_ProdER_nrm)

#Create indice de fonction ecologique 
#Inverser les valeurs d'antrhopique nrm 
pdv_ProdER_nrm$inv_anthropique_nrm = -pdv_ProdER_nrm$anthropique_nrm +1
pdv_ProdER_nrm$inv_agricole_nrm = -pdv_ProdER_nrm$agricole +1
pdv_ProdER_nrm$inv_slope_nrm = -pdv_ProdER_nrm$slope_nrm+1

pdv_ProdER_nrm$FE1 = (
  pdv_ProdER_nrm$inv_anthropique_nrm + pdv_ProdER_nrm$inv_agricole_nrm +
    pdv_ProdER_nrm$inv_slope_nrm + pdv_ProdER_nrm$HydCond_nrm
) / 4

#Create plot with row as x-axis and value as y-axis
ggplot(pdv_ProdER_nrm, aes(x = name, y = FE1)) +
  geom_point(stat = "identity") +
  xlab("name") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))#change the angle of the x lables






