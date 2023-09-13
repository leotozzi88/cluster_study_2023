library(data.table)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

data=read.csv('data/dataset_merged_qc_imputed_combat.csv')
img_vars=c('ic.791103_Left_AG.to.752691_Left_amPFC', 'ic.752691_Left_amPFC.to.414207_Right_AG', 'ic.752691_Left_amPFC.to.073720_Medial_PCC', 'ic.791103_Left_AG.to.073720_Medial_PCC', 'ic.414207_Right_AG.to.073720_Medial_PCC', 'ic.477522_Left_antInsula.to.447647_Left_Amygdala', 'ic.954861_Right_antInsula.to.450767_Right_Amygdala', 'ic.477522_Left_antInsula.to.954861_Right_antInsula', 'act.con.sad_vs_neu.779062_Left_Amygdala', 'act.con.sad_vs_neu.176064_Right_Amygdala', 'act.con.sad_vs_neu.707656_Left_antInsula', 'act.con.sad_vs_neu.534482_Right_antInsula', 'act.con.sad_vs_neu.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.707656_Left_antInsula.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.707656_Left_antInsula', 'ppi.con.sad_vs_neu.534482_Right_antInsula.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.534482_Right_antInsula', 'ppi.con.sad_vs_neu.779062_Left_Amygdala.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.779062_Left_Amygdala', 'ppi.con.sad_vs_neu.176064_Right_Amygdala.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.176064_Right_Amygdala', 'act.con.thr_vs_neu.779062_Left_Amygdala', 'act.con.thr_vs_neu.176064_Right_Amygdala', 'act.con.thr_vs_neu.131520_Medial_dACC', 'ppi.con.thr_vs_neu.779062_Left_Amygdala.131520_Medial_dACC', 'ppi.con.thr_vs_neu.131520_Medial_dACC.779062_Left_Amygdala', 'ppi.con.thr_vs_neu.176064_Right_Amygdala.131520_Medial_dACC', 'ppi.con.thr_vs_neu.131520_Medial_dACC.176064_Right_Amygdala', 'act.nco.thr_vs_neu.779062_Left_Amygdala', 'act.nco.thr_vs_neu.176064_Right_Amygdala', 'act.nco.thr_vs_neu.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.779062_Left_Amygdala.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.911981_Medial_sgACC.779062_Left_Amygdala', 'ppi.nco.thr_vs_neu.176064_Right_Amygdala.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.911981_Medial_sgACC.176064_Right_Amygdala', 'act.con.happy_vs_neu.182355_Medial_mOFC', 'act.con.happy_vs_neu.001716_Left_vStriatum', 'act.con.happy_vs_neu.877949_Right_vStriatum', 'ic.532911_Left_lPFC.to.156999_Left_msPFC', 'ic.275836_Right_lPFC.to.156999_Left_msPFC', 'ic.125909_Left_aIPL.to.532911_Left_lPFC', 'ic.525931_Right_aIPL.to.275836_Right_lPFC', 'ic.125909_Left_aIPL.to.831650_Left_precuneus', 'ic.525931_Right_aIPL.to.216192_Right_precuneus', 'act.gng.nogo_vs_go.013136_Left_dlPFC', 'act.gng.nogo_vs_go.533294_Right_dlPFC', 'act.gng.nogo_vs_go.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.013136_Left_dlPFC.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.291082_Medial_dACC.013136_Left_dlPFC', 'ppi.gng.nogo_vs_go.533294_Right_dlPFC.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.291082_Medial_dACC.533294_Right_dlPFC')

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
img_vars_names=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'NS4', 'NS5', 'NS3', 'NS2', 'NS1', 'NS2NS1b', 'NS1NS2a', 'NS3NS1b', 'NS1NS3a', 'NS4NS1b', 'NS1NS4a', 'NS5NS1b', 'NS1NS5a', 'NT2', 'NT3', 'NT1', 'NT2NT1a', 'NT1NT2b', 'NT3NT1b', 'NT1NT3a', 'NTN3', 'NTN2', 'NTN1', 'NTN2NTN1a', 'NTN1NTN2b', 'NTN3NTN1a', 'NTN1NTN3b', 'P1', 'P2', 'P3', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'C1', 'C3', 'C2', 'C1C2a', 'C2C1b', 'C3C2b', 'C2C3a')
setnames(data_clin_std, img_vars, img_vars_names)

# Average the 2 directions of the PPI
data_clin_std[, 'NS2NS1']=rowMeans(data_clin_std[, c('NS2NS1b', 'NS1NS2a')])
data_clin_std[, 'NS3NS1']=rowMeans(data_clin_std[, c('NS3NS1b', 'NS1NS3a')])
data_clin_std[, 'NS4NS1']=rowMeans(data_clin_std[, c('NS4NS1b', 'NS1NS4a')])
data_clin_std[, 'NS5NS1']=rowMeans(data_clin_std[, c('NS5NS1b', 'NS1NS5a')])
data_clin_std[, 'NT2NT1']=rowMeans(data_clin_std[, c('NT2NT1a', 'NT1NT2b')])
data_clin_std[, 'NT3NT1']=rowMeans(data_clin_std[, c('NT3NT1b', 'NT1NT3a')])
data_clin_std[, 'NTN2NTN1']=rowMeans(data_clin_std[, c('NTN2NTN1a', 'NTN1NTN2b')])
data_clin_std[, 'NTN3NTN1']=rowMeans(data_clin_std[, c('NTN3NTN1a', 'NTN1NTN3b')])
data_clin_std[, 'C1C2']=rowMeans(data_clin_std[, c('C1C2a', 'C2C1b')])
data_clin_std[, 'C3C2']=rowMeans(data_clin_std[, c('C3C2b', 'C2C3a')])

# Save data
write.csv(data_clin_std, 'data/dataset_merged_qc_imputed_combat_clin_std.csv', row.names = FALSE)
write.csv(data_all_std, 'data/dataset_merged_qc_imputed_combat_all_std.csv', row.names = FALSE)

