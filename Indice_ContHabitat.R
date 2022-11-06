#This script is for creating the index of Habitat Connectivity
# The four metrics used are Shape_mnG Area_mnG pdG lpiG Shape_mnF PdU PdA ContF ContA
source('Statistics_new.R')
#Get Vect data from ContH 
ContH
#Subset data with all metrics we want to look at
ContH_Fe = ContH[, c('id_uea','id_rive','pdg_nrm','shape_mnf_nrm', 'pdu_nrm', 'pda_nrm', 'contf_nrm', 'conta_nrm')]
Cont_Fe_sub1 = ContH_Fe[, c('pdg_nrm','shape_mnf_nrm', 'pdu_nrm', 'pda_nrm', 'contf_nrm', 'conta_nrm')]
#Cont_Fe_sub1 = as.data.frame(Cont_Fe_sub1)
#corr_ContH_Fe = round(cor(Cont_Fe_sub1, method = c('spearman')), 8)

ContH_Fe = as.data.frame(ContH_Fe)
ContH_Fe = replace(ContH_Fe,ContH_Fe == -999, 0 )
ContH_Fe_sub2 = ContH_Fe[, c('pdg_nrm', 'pdu_nrm', 'pda_nrm', 'contf_nrm', 'conta_nrm')]

corr_m_ContH_Fe <- round(cor(ContH_Fe_sub2, method = c('spearman')), 2) #Create Correlation matrix to make sure the metrics are not correlated

ContH_Fe$indice_Fe_ContH =   (ContH_Fe$pdg_nrm  + ContH_Fe$pdu_nrm + ContH_Fe$pda_nrm + ContH_Fe$contf_nrm +
  ContH_Fe$conta_nrm)* (1/5)
ContH_Fe$id = paste0(ContH_Fe$id_uea, '_rive', ContH_Fe$id_rive)
ContH_Fe_PASL = left_join(ContH_Fe, df_PASL_IQBR, by = c('id') )

ContH_Fe_PASL = ContH_Fe_PASL %>% select( 'indice_Fe_ContH', 'indice_PASL_IQBR')
ContH_Fe_PASL = na.omit(ContH_Fe_PASL)
corr_ContH_Fe_PASL = round(cor(ContH_Fe_PASL, method = c('spearman')), 2)


ContH_Fe_sub = ContH[, c('id_uea','id_rive','contf_nrm', 'conta_nrm')]
ContH_Fe$Fe = (
  ContH_Fe$pdg_nrm + ContH_Fe$shape_mnf_nrm + ContH_Fe$pdu_nrm + ContH_Fe$pda_nrm + ContH_Fe$contf_nrm +
    ContH_Fe$conta_nrm
) * (1 / 6)
s
ContH_Fe_sub$Fe = (ContH_Fe_sub$contf_nrm +
    ContH_Fe_sub$conta_nrm
) * (1 / 2)

df = ContH_Fe_sub[,c('id_uea', 'id_rive', 'Fe')]
df$id = paste0(df$id_uea, '_rive', df$id_rive)
df = as.data.frame(df)

#3####################################################################
# TESTING CORRELATION WITH IQBR MADE FROM UT
IQBR$id =paste0(IQBR$Id_UEA, '_rive', IQBR$Id_rive) 
IQBR_df = as.data.frame(IQBR)
df_join = left_join(df, IQBR, by = c('id'='id'))
df_join = df_join[,c('id_uea', 'id_rive', 'id', 'IQBR', 'Fe')]
names(df_join)
df_cor = df_join[,c( 'IQBR', 'Fe')]

Correlation_matrix(df_cor, 'FeContH vs IQBR')
corr_IQBR_CONTHAB <- round(cor(df_cor, method = c('spearman')), 8)

#############################################################################
#   TESTING IQBR correlation made with PASL df_PASL_IQBR
df_PASL_IQBR
df2_join = left_join(df, df_PASL_IQBR, by = c('id'='id'))
df2_cor = df2_join[,c( 'id_uea', 'id_rive', 'indice_PASL_IQBR', 'Fe')]
df2_cor = na.omit(df2_cor)
Correlation_matrix(df2_cor, 'FeContH vs df_PASL_IQBR')
corr_PASL_CONTHAB <- round(cor(df2_cor, method = c('spearman')), 8)

################################################################################
#Test VSURF 

install.packages('VSURF')
library(VSURF)

test =  UREC_norm_sqlite[, c(
  "id_uea"    ,     "id_rive" ,
  "contu_nrm"   ,   "conta_nrm" ,"contf_nrm"  ,    "contg_nrm"  ,
  "shape_mnf_nrm",
  "area_mnf_nrm",
  "pdf_nrm" ,
  "lpif_nrm"  ,
  "shape_mng_nrm" ,
  "area_mng_nrm" ,
  "pdg_nrm"  ,
  "lpig_nrm",
  "shape_mnu_nrm",
  "area_mnu_nrm"  ,
  "pdu_nrm"   ,
  "lpiu_nrm"   ,
  "shape_mna_nrm"  ,
  "area_mna_nrm" ,
  "pda_nrm" ,
  "lpia_nrm"
)]
test$id = paste0(test$id_uea, '_rive', test$id_rive)
test = as.data.frame(test)
IQBR$id =paste0(IQBR$Id_UEA, '_rive', IQBR$Id_rive) 
IQBR_df = as.data.frame(IQBR)
df_join = left_join(test, IQBR, by = c('id'='id'))
names(df_join)

test_sub = df_join %>% select(-c('id_uea', 'id', 'id_rive', 'Id_UEA', 'Id_rive'))
test_sub = as.data.frame(test_sub)

test1 = VSURF(test_sub, y = test_sub$IQBR, ntree = 10,nfor.thres = 5,  nfor.interp = 3, nfor.pred = 3)



