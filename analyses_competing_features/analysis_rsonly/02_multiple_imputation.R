library(miceRanger)
library(VIM)

set.seed(12345)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper/')

data=read.csv('appeal/analyses_competing_features/analysis_rsonly/dataset_merged_qc.csv')
img_vars=c('ic.791103_Left_AG.to.752691_Left_amPFC', 'ic.752691_Left_amPFC.to.414207_Right_AG', 'ic.752691_Left_amPFC.to.073720_Medial_PCC', 'ic.791103_Left_AG.to.073720_Medial_PCC', 'ic.414207_Right_AG.to.073720_Medial_PCC', 'ic.477522_Left_antInsula.to.447647_Left_Amygdala', 'ic.954861_Right_antInsula.to.450767_Right_Amygdala', 'ic.477522_Left_antInsula.to.954861_Right_antInsula', 'ic.532911_Left_lPFC.to.156999_Left_msPFC', 'ic.275836_Right_lPFC.to.156999_Left_msPFC', 'ic.125909_Left_aIPL.to.532911_Left_lPFC', 'ic.525931_Right_aIPL.to.275836_Right_lPFC', 'ic.125909_Left_aIPL.to.831650_Left_precuneus', 'ic.525931_Right_aIPL.to.216192_Right_precuneus')

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
write.csv(data_impute, 'appeal/analyses_competing_features/analysis_rsonly/dataset_merged_qc_imputed.csv', row.names = FALSE)
