library('ez.combat')

set.seed(12345)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

data=read.csv('appeal/analyses_competing_features/analysis_tokuda/dataset_merged_qc_imputed.csv')
img_vars=grep("tok_", names(data), value = TRUE)

# Run COMBAT
cb=ez.combat(data,
          'scanner',
          adjust.var = img_vars,
          exclude.var = NULL,
          output = c("overwrite"),
          use.eb = TRUE,
          verbose = TRUE)
data_adjusted=cb$df

write.csv(data_adjusted, 'appeal/analyses_competing_features/analysis_tokuda/dataset_merged_qc_imputed_combat.csv', row.names = FALSE)

