library(emmeans)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

# Import data
data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu_dataspl.csv')

# Scale severity between 0 and 1
data[!is.na(data$hdrs21_total_bl), 'treat_severity_bl_scaled']=data[!is.na(data$hdrs21_total_bl), 'hdrs21_total_bl']/53 
data[!is.na(data$hdrs21_total_fu), 'treat_severity_fu_scaled']=data[!is.na(data$hdrs21_total_fu), 'hdrs21_total_fu']/53 
data[!is.na(data$scl20_bl), 'treat_severity_bl_scaled']=data[!is.na(data$scl20_bl), 'scl20_bl']/4
data[!is.na(data$scl20_fu), 'treat_severity_fu_scaled']=data[!is.na(data$scl20_fu), 'scl20_fu']/4

# Test for effects of cluster and treatment
treats=c('Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'TAU')
data_mod=data[data$treatment_arm %in% treats,]
data_mod$clu=factor(data_mod$clu)
mod=lm(treat_severity_fu_scaled~treat_severity_bl_scaled+clu*treatment_arm, data=data_mod)
anova(mod)

# Contrasts between clusters for each treatment
mod_emmeans=emmeans(mod, list(pairwise ~ clu |  treatment_arm), adjust='none', CI=TRUE)
emmeans_df=as.data.frame(mod_emmeans)
confint(mod_emmeans) # CIs




# Save predicted values
data[data$treatment_arm %in% treats & !is.na(data$treat_severity_fu_scaled),'predicted_treatsev']=predict(mod)

# Define remission
data[!is.na(data$hdrs21_total_fu) & data$hdrs21_total_fu<=7, 'remission']=1
data[!is.na(data$hdrs21_total_fu) & data$hdrs21_total_fu>7, 'remission']=0
data[!is.na(data$scl20_fu) & data$scl20_fu<=0.5, 'remission']=1
data[!is.na(data$scl20_fu) & data$scl20_fu>0.5, 'remission']=0

# Define response 
data[!is.na(data$hdrs21_total_fu) & data$hdrs21_total_fu<=7, 'remission']=1
data[!is.na(data$hdrs21_total_fu) & data$hdrs21_total_fu>7, 'remission']=0
data[!is.na(data$scl20_fu) & data$scl20_fu<=0.5, 'remission']=1
data[!is.na(data$scl20_fu) & data$scl20_fu>0.5, 'remission']=0

# Define treatment response as % symptom change
data[!is.na(data$hdrs21_total_bl), 'perc_treat_resp']=(data[!is.na(data$hdrs21_total_bl), 'hdrs21_total_fu']-data[!is.na(data$hdrs21_total_bl), 'hdrs21_total_bl'])/data[!is.na(data$hdrs21_total_bl), 'hdrs21_total_bl']
data[!is.na(data$scl20_bl), 'perc_treat_resp']=(data[!is.na(data$scl20_bl), 'scl20_fu']-data[!is.na(data$scl20_bl), 'scl20_bl'])/data[!is.na(data$scl20_bl), 'scl20_bl']
data$response=(data$perc_treat_resp < -0.50)*1

# Number of response/remission
table(data$treatment_arm, data$response, data$clu)
table(data$treatment_arm, data$remission, data$clu)

# Print percent of response
for (clu in unique(data$clu)){
  temp=data[data$clu == clu, ]
  print(clu)
  tab=round(prop.table(table(temp$treatment_arm, temp$response), margin = 1)*100)
  print(tab)
}

# Print percent of remission
for (clu in unique(data$clu)){
  temp=data[data$clu == clu, ]
  print(clu)
  tab=round(prop.table(table(temp$treatment_arm, temp$remission), margin = 1)*100)
  print(tab)
}

# Export
write.csv(data, 'data/dataset_merged_qc_imputed_combat_clin_std_clu_dataspl_treat.csv', row.names = FALSE)
