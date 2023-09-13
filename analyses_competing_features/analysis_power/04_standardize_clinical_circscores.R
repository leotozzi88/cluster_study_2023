library(data.table)
library(vroom)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

data=as.data.frame(vroom('appeal/analyses_competing_features/analysis_power/dataset_merged_qc_imputed_combat.csv'))
img_vars=grep("power", names(data), value = TRUE)

# Save controls in a separate data set
data_hc=data[data$group==0, ]
write.csv(data_hc, 'appeal/analyses_competing_features/analysis_power/dataset_merged_qc_imputed_combat_hc.csv')

# Save clinical subjects in a separate data set
data_clin=data[data$group==1, ]
write.csv(data_clin, 'appeal/analyses_competing_features/analysis_power/dataset_merged_qc_imputed_combat_clin.csv', row.names = FALSE)

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

# Save data
write.csv(data_clin_std, 'appeal/analyses_competing_features/analysis_power/dataset_merged_qc_imputed_combat_clin_std.csv', row.names = FALSE)
write.csv(data_all_std, 'appeal/analyses_competing_features/analysis_power/dataset_merged_qc_imputed_combat_all_std.csv', row.names = FALSE)

