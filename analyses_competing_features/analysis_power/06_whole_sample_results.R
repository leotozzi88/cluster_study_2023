#### Libraries, imports and global variables ####

library(data.table)
library(vroom)
setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')
source('analysis_main/custom_functions.R')
set.seed(123123)
data=as.data.frame(vroom('analyses_competing_features/analysis_power/out/dataset_merged_qc_imputed_combat_clin_std_clu.csv'))

#### Calculate symptom composites ####

data[, 'Sleep']=rowSums(data[, c('qids_01', 'qids_02', 'qids_03')], na.rm = FALSE)
data[, 'Suicide']=data[, c('qids_12')]

# Rename variables
comps=c('pswq_total', 'rrs_total','dass42_str_score','dass42_dep_score','dass42_anx_score', 'shaps_total','masq30_gen_score', 'bis_att_score')
comps_names=c('Ruminative_worry', 'Ruminative_brooding','Tension','Negative_bias','Threat_dysregulation', 'Anhedonia','Anxious_arousal', 'Cognitive_dyscontrol')
data=setnames(data, comps, comps_names)

#### Calculate behavior composites ####

# Rename variables
allbeh_wn=c('wn_emzcompk_norm', 'wn_emzerrk_norm', 'wn_g2avrtk_norm', 'wn_g2fpk_norm', 'wn_wmfnk_norm', 'wn_wmfpk_norm', 'wn_wmrtk_norm', 'wn_dgttrta_norm', 'wn_dgttrtf_norm', 'wn_dgttrth_norm', 'wn_dgttrts_norm', 'wn_dgttrtn_norm', 'wn_gettrta_norm', 'wn_gettrtf_norm', 'wn_gettrth_norm', 'wn_gettrts_norm')
comps_rename=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_anger_RT','Implicit_fear_RT','Implicit_happy_RT','Implicit_sad_RT','Implicit_neutral_RT', 'Explicit_anger_RT','Explicit_fear_RT','Explicit_happy_RT','Explicit_sad_RT')
setnames(data, old = allbeh_wn, new = comps_rename)

# Flip sign of variables to interpret them in their original direction
data[, comps_rename]=-data[, comps_rename]

# Calculate composites
data[, 'Implicit_fear_priming_RT']=data[, 'Implicit_fear_RT']-data[, 'Implicit_neutral_RT']
data[, 'Implicit_anger_priming_RT']=data[, 'Implicit_anger_RT']-data[, 'Implicit_neutral_RT']
data[, 'Implicit_sad_priming_RT']=data[, 'Implicit_sad_RT']-data[, 'Implicit_neutral_RT']
data[, 'Implicit_happy_priming_RT']=data[, 'Implicit_happy_RT']-data[, 'Implicit_neutral_RT']
data[, 'Implicit_threat_priming_RT']=rowMeans(data[, c('Implicit_fear_priming_RT', 'Implicit_anger_priming_RT')], na.rm = TRUE)
data[, 'Explicit_threat_RT']=rowMeans(data[, c('Explicit_anger_RT', 'Explicit_fear_RT')], na.rm = TRUE)

#### Validation of symptom profiles in whole data ####

comps_names=c('Ruminative_worry', 'Ruminative_brooding','Tension','Negative_bias','Threat_dysregulation', 'Anhedonia','Anxious_arousal', 'Cognitive_dyscontrol')

# Test each cluster vs median of clinical participants not in the cluster
whole_symp_Z=compare_clusters(data, cluster_assignments = data$clu, var_names = comps_names)

# Export the results in one table
write.csv(whole_symp_Z, 'analyses_competing_features/analysis_power/out/symps_results_all.csv', row.names = F)

# Insomnia and suicidality
comps_names=c('Sleep', 'Suicide')
whole_symp_chi=compare_clusters_chi_square(data, cluster_assignments = data$clu, var_names = comps_names)

# Export the results in one table
write.csv(whole_symp_chi, 'analyses_competing_features/analysis_power/out/symps_chi_results_all.csv', row.names = F)

#### Validation of behavior profiles in whole data ####

comps_names=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_RT','Implicit_sad_RT', 'Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')

# Test each cluster vs median of clinical participants not in the cluster
whole_beh_Z=compare_clusters(data, cluster_assignments = data$clu, var_names = comps_names)

# Export the results in one table
write.csv(whole_beh_Z, 'analyses_competing_features/analysis_power/out/beh_results_all.csv', row.names = F)

#### Save data ####
write.csv(data, 'analyses_competing_features/analysis_power/out/dataset_merged_qc_imputed_combat_clin_std_clu_sympbeh.csv')

