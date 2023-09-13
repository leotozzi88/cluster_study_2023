library(miceRanger)
library(VIM)

set.seed(12345)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper/')

data=read.csv('appeal/analyses_competing_features/analysis_tokuda/dataset_merged_qc.csv')
img_vars=grep("tok_", names(data), value = TRUE)

data_impute=data

# Save patterns of missingness
aggr_plot <- aggr(data_impute[, img_vars], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=img_vars, cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"), plot = FALSE)
aggr_df=as.data.frame(aggr_plot$missings)
aggr_df$propotion=aggr_df$Count/nrow(data)
write.csv(aggr_df, 'appeal/analyses_competing_features/analysis_tokuda/missing_img_data.csv', row.names = FALSE)

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
write.csv(data_impute, 'appeal/analyses_competing_features/analysis_tokuda/dataset_merged_qc_imputed.csv', row.names = FALSE)

