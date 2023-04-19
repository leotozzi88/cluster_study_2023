library(emmeans)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu_clinscores_behscores.csv')

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

# Contrasts between treatments for each cluster
mod_emmeans=emmeans(mod, list(pairwise ~ treatment_arm | clu ), adjust='none', CI=TRUE)
emmeans_df=as.data.frame(mod_emmeans)
confint(mod_emmeans) # CIs
sig=sigma(mod) # Cohen's d=emmean/sigma

# Save predicted values
data[data$treatment_arm %in% treats & !is.na(data$treat_severity_fu_scaled),'predicted_treatsev']=predict(mod)

# Export
write.csv(data, 'data/dataset_merged_qc_imputed_combat_clin_std_clu_clinscores_behscores_treat.csv', row.names = FALSE)


