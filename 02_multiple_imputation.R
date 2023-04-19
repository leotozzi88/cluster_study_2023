library(miceRanger)
library(VIM)

set.seed(12345)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper/')

data=read.csv('data/dataset_merged_qc.csv')
img_vars=c('ic.791103_Left_AG.to.752691_Left_amPFC', 'ic.752691_Left_amPFC.to.414207_Right_AG', 'ic.752691_Left_amPFC.to.073720_Medial_PCC', 'ic.791103_Left_AG.to.073720_Medial_PCC', 'ic.414207_Right_AG.to.073720_Medial_PCC', 'ic.477522_Left_antInsula.to.447647_Left_Amygdala', 'ic.954861_Right_antInsula.to.450767_Right_Amygdala', 'ic.477522_Left_antInsula.to.954861_Right_antInsula', 'act.con.sad_vs_neu.779062_Left_Amygdala', 'act.con.sad_vs_neu.176064_Right_Amygdala', 'act.con.sad_vs_neu.707656_Left_antInsula', 'act.con.sad_vs_neu.534482_Right_antInsula', 'act.con.sad_vs_neu.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.707656_Left_antInsula.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.707656_Left_antInsula', 'ppi.con.sad_vs_neu.534482_Right_antInsula.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.534482_Right_antInsula', 'ppi.con.sad_vs_neu.779062_Left_Amygdala.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.779062_Left_Amygdala', 'ppi.con.sad_vs_neu.176064_Right_Amygdala.426426_Medial_pgACC', 'ppi.con.sad_vs_neu.426426_Medial_pgACC.176064_Right_Amygdala', 'act.con.thr_vs_neu.779062_Left_Amygdala', 'act.con.thr_vs_neu.176064_Right_Amygdala', 'act.con.thr_vs_neu.131520_Medial_dACC', 'ppi.con.thr_vs_neu.779062_Left_Amygdala.131520_Medial_dACC', 'ppi.con.thr_vs_neu.131520_Medial_dACC.779062_Left_Amygdala', 'ppi.con.thr_vs_neu.176064_Right_Amygdala.131520_Medial_dACC', 'ppi.con.thr_vs_neu.131520_Medial_dACC.176064_Right_Amygdala', 'act.nco.thr_vs_neu.779062_Left_Amygdala', 'act.nco.thr_vs_neu.176064_Right_Amygdala', 'act.nco.thr_vs_neu.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.779062_Left_Amygdala.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.911981_Medial_sgACC.779062_Left_Amygdala', 'ppi.nco.thr_vs_neu.176064_Right_Amygdala.911981_Medial_sgACC', 'ppi.nco.thr_vs_neu.911981_Medial_sgACC.176064_Right_Amygdala', 'act.con.happy_vs_neu.182355_Medial_mOFC', 'act.con.happy_vs_neu.001716_Left_vStriatum', 'act.con.happy_vs_neu.877949_Right_vStriatum', 'ic.532911_Left_lPFC.to.156999_Left_msPFC', 'ic.275836_Right_lPFC.to.156999_Left_msPFC', 'ic.125909_Left_aIPL.to.532911_Left_lPFC', 'ic.525931_Right_aIPL.to.275836_Right_lPFC', 'ic.125909_Left_aIPL.to.831650_Left_precuneus', 'ic.525931_Right_aIPL.to.216192_Right_precuneus', 'act.gng.nogo_vs_go.013136_Left_dlPFC', 'act.gng.nogo_vs_go.533294_Right_dlPFC', 'act.gng.nogo_vs_go.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.013136_Left_dlPFC.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.291082_Medial_dACC.013136_Left_dlPFC', 'ppi.gng.nogo_vs_go.533294_Right_dlPFC.291082_Medial_dACC', 'ppi.gng.nogo_vs_go.291082_Medial_dACC.533294_Right_dlPFC')

data_impute=data

# Save patterns of missingness
aggr_plot <- aggr(data_impute[, img_vars], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=img_vars, cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"), plot = FALSE)
aggr_df=as.data.frame(aggr_plot$missings)
aggr_df$propotion=aggr_df$Count/nrow(data)
write.csv(aggr_df, 'data/missing_img_data.csv', row.names = FALSE)

# Impute features for each scanner separately
for (scanner in data$scanner){
  
if (sum(is.na(data_impute[data_impute$scanner==scanner, c(img_vars)]))>0){
miceObj <- miceRanger(
  data_impute[data_impute$scanner==scanner, c(img_vars)]
  , m=1
  , returnModels = TRUE
  , verbose=TRUE
)

dataList <- completeData(miceObj)
data_impute[data_impute$scanner==scanner, c(img_vars)]=as.data.frame(dataList$Dataset_1)[, c(img_vars)]
}
}

# Save imputed data
write.csv(data_impute, 'data/dataset_merged_qc_imputed.csv', row.names = FALSE)
