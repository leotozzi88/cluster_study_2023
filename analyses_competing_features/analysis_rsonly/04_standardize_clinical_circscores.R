library(data.table)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

data=read.csv('appeal/analyses_competing_features/analysis_rsonly/dataset_merged_qc_imputed_combat.csv')
img_vars=c('ic.791103_Left_AG.to.752691_Left_amPFC', 'ic.752691_Left_amPFC.to.414207_Right_AG', 'ic.752691_Left_amPFC.to.073720_Medial_PCC', 'ic.791103_Left_AG.to.073720_Medial_PCC', 'ic.414207_Right_AG.to.073720_Medial_PCC', 'ic.477522_Left_antInsula.to.447647_Left_Amygdala', 'ic.954861_Right_antInsula.to.450767_Right_Amygdala', 'ic.477522_Left_antInsula.to.954861_Right_antInsula', 'ic.532911_Left_lPFC.to.156999_Left_msPFC', 'ic.275836_Right_lPFC.to.156999_Left_msPFC', 'ic.125909_Left_aIPL.to.532911_Left_lPFC', 'ic.525931_Right_aIPL.to.275836_Right_lPFC', 'ic.125909_Left_aIPL.to.831650_Left_precuneus', 'ic.525931_Right_aIPL.to.216192_Right_precuneus')

# Save controls in a separate data set
data_hc=data[data$group==0, ]
write.csv(data_hc, 'data/dataset_merged_qc_imputed_combat_hc.csv')

# Save clinical subjects in a separate data set
data_clin=data[data$group==1, ]
write.csv(data_clin, 'data/dataset_merged_qc_imputed_combat_clin.csv', row.names = FALSE)

# Standardize each imaging variable to mean and SD of controls
data_clin_std=data_clin
for (var in img_vars){
  hc_mean=mean(data_hc[, var]) 
  hc_sd=sd(data_hc[, var]) 
  data_clin_std[, var]=(data_clin_std[, var]-hc_mean)/hc_sd
}

# Do the same for the whole data set 
data_all_std=data
for (var in img_vars){
  hc_mean=mean(data_hc[, var]) 
  hc_sd=sd(data_hc[, var]) 
  data_all_std[, var]=(data_all_std[, var]-hc_mean)/hc_sd
}

# Rename the features to their abbreviations
img_vars_names=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7')
setnames(data_clin_std, img_vars, img_vars_names)

# Save data
write.csv(data_clin_std, 'appeal/analyses_competing_features/analysis_rsonly/dataset_merged_qc_imputed_combat_clin_std.csv', row.names = FALSE)
write.csv(data_all_std, 'appeal/analyses_competing_features/analysis_rsonly/dataset_merged_qc_imputed_combat_all_std.csv', row.names = FALSE)

