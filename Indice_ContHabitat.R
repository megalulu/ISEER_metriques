#This script is for creating the index of Habitat Connectivity
# The four metrics used are Shape_mnG Area_mnG pdG lpiG Shape_mnF PdU PdA ContF ContA

#Get Vect data from ContH 
ContH
#Subset data with all metrics we want to look at
ContH_Fe = ContH[, c('id_uea','id_rive','pdg_nrm','shape_mnf_nrm', 'pdu_nrm', 'pda_nrm', 'contf_nrm', 'conta_nrm')]
COntH_Fe_sub =
ContH_Fe$Fe = (
  ContH_Fe$pdg_nrm + ContH_Fe$shape_mnf_nrm + ContH_Fe$pdu_nrm + ContH_Fe$pda_nrm + ContH_Fe$contf_nrm +
    ContH_Fe$conta_nrm
) * (1 / 6)


df = ContH_Fe[,c('id_uea', 'id_rive', 'Fe')]
df$id = paste0(df$id_uea, '_rive', df$id_rive)
df = as.data.frame(df)
UREC_merge$id =paste0(UREC_merge$Id_UEA, '_rive', UREC_merge$Id_rive) 
UREC_merge_df = as.data.frame(UREC_merge)
df_join = left_join(df, UREC_merge, by = c('id'='id'))
df_join = df_join[,c('id_uea', 'id_rive', 'id', 'IQBR', 'Fe')]
names(df_join)

df_cor = df_join[,c( 'IQBR', 'Fe')]

Correlation_matrix(df_cor, 'FeContH vs IQBR')
