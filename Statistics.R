#Some basic stats : Summary statistics, correlation matrix and PCA
###############################################################################
#import libraries
install.packages("corrplot")
install.packages("Hmisc")
install.packages('ggplot')
install.packages("ggpubr")
install.packages("factoextra")
library(Hmisc)
library(corrplot)
library(ggplot2)
library("ggpubr")
library('car')
library(factoextra)
library(psych)

#import UREC_merge_norm from results/(already in my environment as UREC_norm)
UREC_norm = vect('C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_norm.shp')
writeVector(UREC_norm, 'C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_norm.SQLite', filetype = 'SQLite',  overwrite = TRUE)
UREC_norm_sqlite = vect('C:/Meghana/Belgique/traitements/results/UREC_merge/UREC_merge_norm.SQLite')
print(names(UREC_norm))
#Extract columns we care about for each EF (RegT = Regulation de la temperature de l'eau)
RegT = UREC_norm[,c("Id_UEA","Id_rive", "srfCan_m2_nrm",  "IndOmbrage_nrm")] #DataFrame for Regulation de la temperature de l'eau
ContH = UREC_norm[,c("Id_UEA","Id_rive","contF_nrm",  "contG_nrm","contU_nrm"  ,"contA_nrm")] #DataFrame for Continuite de l

################################################################################
#Fonction 6: Regulation de la temperature de l'eau
################################################################################
#Check normality of data
#Frequency distribution of metrics
Freq_surfCan  = hist(RegT$srfCan_m2_nrm, main = 'Frequency distribution of  surface of the canopy Metric')
Freq_IndOmbr = hist(RegT$IndOmbrage_nrm, main = 'Frequency distribution of Hillshade index Metric')
#Create a density plot of metric values 
Density_surfCan = ggdensity(RegT$srfCan_m2_nrm,
          main = "Density plot of canopy surface",
          xlab = "Canopy surface")
Denisty_hillshade = ggdensity(RegT$IndOmbrage_nrm, 
                              main = "Density plot of hillshade index",
                              xlab = "Hillshade index")
#Create a quantile-quantile plot :draws the correlation between a given sample and the normal distribution. A 45-degree reference line is also plotted.
qqPlot(RegT$srfCan_m2_nrm) # Canopy surface 
qqPlot(RegT$IndOmbrage_nrm) #Hillshade index

#Do Shapiro-Wilk's normality test (one variable at a time)
shapiro.test(RegT$srfCan_m2_nrm) #RegTemp Surface Canopy is not normally distributed (p-valu < 0.05)
shapiro.test(RegT$IndOmbrage_nrm) #Hillshade index is not normally distributed (p-value < 0.05)

#Do Pearson's correlation between the variables reprensenting each function (Pearson's correlation does not depend on normality of data)
RegT_sub = RegT[,c('srfCan_m2_nrm', 'IndOmbrage_nrm')] #extract columns to test correlation and drop geometry
RegT_sub = as.data.frame(RegT_sub) #transform spatVect into dataframe to create Scatterplot 

res <- cor.test(x = RegT$srfCan_m2_nrm, y = RegT$IndOmbrage_nrm, method = 'pearson')
res #Pearson's correlation test
res$p.value #P-value of Pearson's Correlation 
res$estimate # Extract the correlation coefficient
res$statistic #Value of t-test
#Create Scatter plot of correlation between the two variables
ggscatter(RegT, x = "srfCan_m2_nrm", y = "IndOmbrage_nrm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Canopy Surface", ylab = "hillshade index")
#PCA analysis 
princomp(RegT_sub, cor = TRUE, scores = TRUE) #for analysing variables
#Visualising PCA analysis
res_pca <- prcomp(RegT_sub, scale = TRUE)
fviz_eig(res_pca)
fviz_pca_var(res_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#This is a 3 in 1 plot of density, frequency, correlation
pairs.panels(RegT_sub, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


################################################################################
#Fonction 8: Continuite de l'habitat
################################################################################
#Check normality of data
#Create a frequency distribution plots 
Freq_contF  = hist(ContH$contF_nrm, main = 'Frequency distribution of Forest Continuity Metric')
Freq_contG = hist(ContH$contG_nrm, main = 'Frequency distribution of Grassland Continuity Metric')
Freq_contU = hist(ContH$contU_nrm, main = 'Frequency distribution of Urban Continuity Metric')
Freq_contA = hist(ContH$contA_nrm, main = 'Frequency distribution of Agriculture Continuity Metric')

#Create a density plot of metric values 
Density_contF = ggdensity(ContH$contF_nrm,
                            main = "Density plot of forest continuity",
                            xlab = "Ratio of forest surface on unit area")
Density_contG = ggdensity(ContH$contG_nrm,
                          main = "Density plot of grassland continuity",
                          xlab = "Ratio of grassland surface on unit area")
Density_contU = ggdensity(ContH$contU_nrm,
                          main = "Density plot of urban continuity",
                          xlab = "Ratio of Urban surface on unit area")
Density_contA= ggdensity(ContH$contA_nrm,
                          main = "Density plot of agriculture continuity",
                          xlab = "Ratio of Agriculture surface on unit area")
#Create a quantile-quantile plot :draws the correlation between a given sample and the normal distribution. A 45-degree reference line is also plotted.
qqPlot(ContH$contF_nrm, main = 'Quantile distribution plot of Forest Continuity') # Continuite forestiere 
qqPlot(ContH$contG_nrm, main = 'Quantile distribution plot of Grassland Continuity') # Continuite herbacee 
qqPlot(ContH$contU_nrm, main = 'Quantile distribution plot of Urban Continuity') # Continuite Urbaine
qqPlot(ContH$contA_nrm, main = 'Quantile distribution plot of Agriculture Continuity') # Continuite Agricole


#Do Shapiro-Wilk's normality test (one variable at a time)
shapiro.test(ContH$contF_nrm) #ContHab: Forest Continuity is not normally distributed (p-valu < 0.05)
shapiro.test(ContH$contG_nrm) #ContHab: Grassland Continuity is not normally distributed (p-value < 0.05)
shapiro.test(ContH$contU_nrm) #ContHab: Urban Continuity is not normally distributed (p-value < 0.05)
shapiro.test(ContH$contA_nrm) #ContHab: Agriculture Continuity is not normally distributed (p-value < 0.05)


#Do Pearson's correlation between the variables reprensenting each function (Pearson's correlation does not depend on normality of data)
ContH_sub = as.data.frame(ContH[,c("contF_nrm",  "contG_nrm","contU_nrm"  ,"contA_nrm")])#Subset data for numeric colums on which to do Correlation and transform into data frame
#Correlation scatter plot for matrix of data with R scores and frequency distribution and density plots. 
pairs.panels(ContH_sub, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
         





#PCA analysis 
princomp(ContH_sub, cor = TRUE, scores = TRUE) #for analysing variables
#Visualising PCA analysis
ContH_sub_PCA <- prcomp(ContH_sub, scale = TRUE)
fviz_eig(ContH_sub_PCA)
fviz_pca_var(ContH_sub_PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



################################################################################
#FRAGSTAT Clean up of data

fragstat <-
  read.csv(
    "C:/Meghana/Belgique/traitements/FRAGSTAT_test/results/test_results/batch_test.csv",
    header = TRUE,
    sep = ";"
  )

names(fragstat)

fragstat$PD_m2 = fragstat$PD..hectare.1.*0.0001 #conver patch density from (hec[-1] to m[-2])
names(fragstat)

head(fragstat$PD_m2)
head(fragstat$PD..hectare.1.)
f = 2

for (f in colnames(fragstat)) {
  column = fragstat[f] #select one column
  colname = names(column)
  print(colname)
  #Check if column name is 'Id_UEA' or 'Id_rive' or 'ext_geometry' or IndOmbrage (already has values between 0 and 1) : these columns should not be normalized
  if (colname != 'LID') {
    print('Colename is not LID')
    new_colname =   paste0(names(column), '_nrm')#Create name for normalized column
    print(new_colname) #Print new column name to make sure it looks correct
    max_val = max(column) #find maximum value in column and put it in a variable
    print(max_val)
    min_val = min(column) #find minimum value in column and put it in a variable
    print(min_val)
    #iterate though each row of the column we are looking at (f)
    for (n in 1:nrow(fragstat)) {
      normalisation = (column - min_val) / (max_val - min_val) #Min max normalisation equation
      #Create a new column with new column name and add normalisation value for the row. (This may not be optimal)
      fragstat[, new_colname] <- normalisation 
    }
  }
}


names(fragstat)
fragstat_sub = fragstat[, c('LPI_nrm','AREA_MD_nrm','SHAPE_MN_nrm', 'SHAPE_MD_nrm','PD_m2_nrm')]
names(fragstat_sub)

#Data distribution 
#Create a frequency distribution plots 
Freq_LPI_nrm  = hist(fragstat_sub$LPI_nrm, main = 'Frequency distribution of Largest Patch Index (LPI)')
Freq_AREA_MD_nrm = hist(fragstat_sub$AREA_MD_nrm, main = 'Frequency distribution of area median')
Freq_SHAPE_MN_nrm = hist(fragstat_sub$SHAPE_MN_nrm, main = 'Frequency distribution of SHAPE mean')
Freq_SHAPE_MD_nrm = hist(fragstat_sub$SHAPE_MD_nrm, main = 'Frequency distribution of SHAPE median')
Freq_PD_m2_nrm = hist(fragstat_sub$PD_m2_nrm, main = 'Frequency distribution of Patch Density in m2')


#Create a density plot of metric values 
Density_LPI_nrm = ggdensity(fragstat_sub$LPI_nrm, main = 'Density distribution of Largest Patch Index (LPI)',
                          xlab = "Largest patch index ")
Density_AREA_MD_nrm = ggdensity(fragstat_sub$AREA_MD_nrm, main = 'Density distribution of area median',
                          xlab = "Median patch area")
Density_SHAPE_MN_nrm = ggdensity(fragstat_sub$SHAPE_MN_nrm, main = 'Density distribution of SHAPE mean',
                          xlab = "Mean Shape index")
Density_SHAPE_MD_nrm= ggdensity(fragstat_sub$SHAPE_MD_nrm, main = 'Density distribution of  Shape median',
                         xlab = "Median Shape Index")
Density_PD_m2_nrm = ggdensity(fragstat_sub$PD_m2_nrm, main = 'Density distribution of patch density per m-2',
                              xlab = 'Patch density (m-2)')
#Do shapiro Wilks normality test
shapiro.test(fragstat_sub$LPI_nrm) #ContHab: Forest Continuity is not normally distributed (p-valu < 0.05)
shapiro.test(fragstat_sub$AREA_MD_nrm) #ContHab: Grassland Continuity is not normally distributed (p-value < 0.05)
shapiro.test(fragstat_sub$SHAPE_MN_nrm) #ContHab: Urban Continuity is not normally distributed (p-value < 0.05)
shapiro.test(fragstat_sub$SHAPE_MD_nrm) #ContHab: Agriculture Continuity is not normally distributed (p-value < 0.05)
shapiro.test(fragstat_sub$PD_m2_nrm)


#Pearson's correlation test 
pairs.panels(fragstat_sub, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#Do a PCA
#PCA analysis 
princomp(fragstat_sub, cor = TRUE, scores = TRUE) #for analysing variables
#Visualising PCA analysis
fragstat_sub_PCA <- prcomp(fragstat_sub, scale = TRUE)
fviz_eig(fragstat_sub_PCA)
fviz_pca_var(fragstat_sub_PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



#Combine all the Habitat Connectivity metrics together and redo correlation matrix and PCA analysis
ContH_full = cbind(ContH_sub, fragstat_sub) #Combine fragstat_sub with ContH_sub (!!!!ATENTION one row missing from fragstat_sub --> add row of 0s)
#Pearson's correlation test 
pairs.panels(ContH_full, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
#PCA analysis 
princomp(ContH_full, cor = TRUE, scores = TRUE) #for analysing variables
#Visualising PCA analysis
ContH_full_PCA <- prcomp(ContH_full, scale = TRUE)
fviz_eig(fragstat_sub_PCA)
fviz_pca_var(ContH_full_PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



################################################################################
#This is for all the variables at the same time. TO BE DELETED WHEN NO LONGER NEEDED
df_norm = UREC_norm[,c("Id_UEA","Id_rive","width_nrm", "contF_nrm","contG_nrm","sinuosity_nrm" )] #extract only normalized data from table
df_cor = UREC_norm[,c("width_nrm", "contF_nrm","contG_nrm","sinuosity_nrm" )]
df_cor = st_drop_geometry(df_cor)
#create summary statistics table 
stdev(df_cor)

  

#Create pearson'S correlation coefficient table (matrix)

df_cor_rcorr = rcorr(as.matrix(df_cor))
df_cor_coeff = df_cor_rcorr$r
df_cor_p = df_cor_rcorr$P
corrplot(pearson) #ugly correlation plot
palette = colorRampPalette(c("green", "white", "red")) (20) #create colour palette for heat map
heatmap(x = pearson, col = palette, symm = TRUE, cexRow=0.7,cexCol = 0.7,Colv = NA, Rowv = NA  )

#Other ways to do correlation
ContH_sub_res <- cor(ContH_sub, 
                     method = "pearson")
#Other wayts to create Scatter plot of correlation between the two variables
pairs(ContH_sub, hist = T, cor = T, use = 'pearson',lower.panel = panel.cor, cex = 1.5,
      main = 'Correlation pannel for metric Cont : Forest, Grassland, Urban, and Agriculture')