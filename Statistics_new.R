#Clean version of statistics. Maybe with function

#Install and import libraries
############################
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
using("corrplot",'Hmisc' ,"ggplot","ggpubr",'factoextra', 'corrplot')


#Import Librarries
#############################
library(Hmisc)
library(corrplot)
library(ggplot2)
library(ggpubr)
library(car)
library(factoextra)
library(psych)



##############################################################################
              #Visualising Correlation matrix between variables
##############################################################################
# df : object Spatvect <-  metrics to do correlation analysis on. Dataframe should only have numeric values and the two columns ('id_uea', 'rive', 'id)
# var2 : Character string <- Name of the set of metrics you are testing (can be name of ecosystem function)
# output : res (matrix array) <-table of correlation matrix 
Correlation_matrix <- function(df, var2) {
  df = as.data.frame(df)
  df_sub = df %>% select(-c('id_uea', 'rive', 'id')) #subset dataframe to only select numeric data
  res <-
    round(cor(df_sub, method = c('spearman')), 2) #Create Correlation matrix
  
  #Visualising correlation matrix
  corrplot.mixed(
    res,
    lower = 'number',
    upper = 'square',
    tl.col = 'black',
    tl.pos = 'lt',
    tl.cex = 1,
    number.cex = 0.5,
    diag = 'n',
    title = paste0('Correlation plot for metrics in ', var2),
    mar=c(0,0,1,0)
  ) #Big Correlation plot with heat map: upper triangle (squares), lower triangle (numbers)
  
  corrplot(
    res,
    method = 'number',
    type = "lower",
    order = "original",
    tl.col = "black",
    tl.srt = 45,
    is.corr = T,
    diag = T,
    number.cex = 0.5,
    title = paste0('Correlation plot of ', var2),
    mar=c(0,0,1,0)
  ) #Correlation plot with heat map : Only lower triangle correlation plot with numbers
  
  #Create matrix of p-values to see significance of correlation
  testRes = cor.mtest(res, conf.level = 0.95)
  corrplot(
    res,
    p.mat = testRes$p,
    sig.level = 0.05,
    method = 'square',
    type = 'lower',
    insig = 'blank',
    addCoef.col = 'black',
    number.cex = 0.40,
    tl.col = 'black',
    order = 'AOE',
    diag = FALSE,
    win.asp = 1,
    title = paste0('Corr plot ', var2, ' (p-value<0.05 blank)'),
    mar = c(0,0,1,0)
   
    )
  
  
  return(res)
  
}



##############################################################################
          #PCA function for visualisation
##############################################################################

# df : Dataframe <- or sf or spatvector object with only colums that you want to do PCA for (only numeric values)
#df_name : String of characters <- Name to add to title of plots
#Returns : 1. One scree plot of PCA
          #2. PCA plot
          #3. df_PCA_tbl : a list of matrices containing all the results for the active individuals/variables including:
              #coord (Coordinates on graph of each vector) : #coordinates for the individuals/variables
              #cos2 : cos2 for the individuals/variables
              #contrib :  # contributions of the individuals/variables 
             # cor : matrix of covariance between variables
df = UREC_ISEER
PCA_graph_function <- function(df, df_name, axe ){
  library(factoextra)
  
  #Transform sf or spatVec object into dataframe
  df = as.data.frame(df)
  df = df %>% select(-c('id_uea', 'rive', 'id')) #subset dataframe to only select numeric data
  
  df_PCA <- princomp(df, cor = TRUE, scores = TRUE) #for analysing variables
  
  #Visualising PCA analysis
  
  df_PCA_tbl =get_pca_var(df_PCA)
  
  Screeplot = fviz_eig(df_PCA, title = paste0('Screeplot ', df_name))
  print(Screeplot)
  PCA_plot  = fviz_pca_var(df_PCA, axes = axe,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE ,  # Avoid text overlapping
               title =  paste0('PCA plot  ', df_name)
               )
  print(PCA_plot)
  
  return(df_PCA_tbl)
  
}




