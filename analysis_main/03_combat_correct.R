#### Libraries, imports and global variables ####

library('ez.combat')
set.seed(12345)
setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')
data=read.csv('data/dataset_merged_qc_imputed.csv')

#### COMBAT correction ####

img_vars=c('ic.791103_Left_AG.to.752691_Left_amPFC', 'ic.752691_Left_amPFC.to.414207_Right_AG', 'ic.752691_Left_amPFC.to.073720_Medial_PCC', 'ic.791103_Left_AG.to.073720_Medial_PCC', 'ic.414207_Right_AG.to.073720_Medial_PCC', 'ic.477522_Left_antInsula.to.447647_Left_Amygdala', 'ic.954861_Right_antInsula.to.450767_Right_Amygdala', 'ic.477522_Left_antInsula.to.954861_Right_antInsula', 'act.con.sad_vs_neu.779062_Left_Amygdala', 'act.con.sad_vs_neu.176064_Right_Amygdala', 'act.con.sad_vs_neu.707656_Left_antInsula', 'act.con.sad_vs_neu.534482_Right_antInsula', 'act.con.sad_vs_neu.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.707656_Left_antInsula.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.707656_Left_antInsula', 'ppi.con.sad_vs_neu.534482_Right_antInsula.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.534482_Right_antInsula', 'ppi.con.sad_vs_neu.779062_Left_Amygdala.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.779062_Left_Amygdala', 'ppi.con.sad_vs_neu.176064_Right_Amygdala.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.176064_Right_Amygdala', 'act.con.thr_vs_neu.779062_Left_Amygdala', 'act.con.thr_vs_neu.176064_Right_Amygdala', 'act.con.thr_vs_neu.131520_Medial_dACC', 'ppi.con.thr_vs_neu.779062_Left_Amygdala.131520_Medial_dACC', 'ppi.con.thr_vs_neu.131520_Medial_dACC.779062_Left_Amygdala', 'ppi.con.thr_vs_neu.176064_Right_Amygdala.131520_Medial_dACC', 'ppi.con.thr_vs_neu.131520_Medial_dACC.176064_Right_Amygdala', 'act.nco.thr_vs_neu.779062_Left_Amygdala', 'act.nco.thr_vs_neu.176064_Right_Amygdala', 'act.nco.thr_vs_neu.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.779062_Left_Amygdala.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.911981_Medial_sgACC.779062_Left_Amygdala', 'ppi.nco.thr_vs_neu.176064_Right_Amygdala.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.911981_Medial_sgACC.176064_Right_Amygdala', 'act.con.happy_vs_neu.182355_Medial_mOFC', 'act.con.happy_vs_neu.001716_Left_vStriatum', 'act.con.happy_vs_neu.877949_Right_vStriatum', 'ic.532911_Left_lPFC.to.156999_Left_msPFC', 'ic.275836_Right_lPFC.to.156999_Left_msPFC', 'ic.125909_Left_aIPL.to.532911_Left_lPFC', 'ic.525931_Right_aIPL.to.275836_Right_lPFC', 'ic.125909_Left_aIPL.to.831650_Left_precuneus', 'ic.525931_Right_aIPL.to.216192_Right_precuneus', 'act.gng.nogo_vs_go.013136_Left_dlPFC', 'act.gng.nogo_vs_go.533294_Right_dlPFC', 'act.gng.nogo_vs_go.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.013136_Left_dlPFC.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.291082_Medial_dACC.013136_Left_dlPFC', 'ppi.gng.nogo_vs_go.533294_Right_dlPFC.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.291082_Medial_dACC.533294_Right_dlPFC')

# Run COMBAT
cb=ez.combat(data,
             'scanner',
             adjust.var = img_vars,
             exclude.var = NULL,
             output = c("overwrite"),
             use.eb = TRUE,
             verbose = TRUE)
data_adjusted=cb$df

write.csv(data_adjusted, 'data/dataset_merged_qc_imputed_combat.csv', row.names = FALSE)

