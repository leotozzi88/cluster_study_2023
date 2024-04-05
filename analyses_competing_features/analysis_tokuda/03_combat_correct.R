#### Libraries, imports and global variables ####

library('ez.combat')
set.seed(12345)
setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')

#### COMBAT ####

data=read.csv('analyses_competing_features/analysis_tokuda/out/dataset_merged_qc_imputed.csv')
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

write.csv(data_adjusted, 'analyses_competing_features/analysis_tokuda/out/dataset_merged_qc_imputed_combat.csv', row.names = FALSE)

