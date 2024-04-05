#### Libraries, imports and global variables ####

library(vroom)
setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')
source('analysis_main/custom_functions.R')
set.seed(123123)
data=as.data.frame(vroom('analyses_competing_features/analysis_power/out/dataset_merged_qc_imputed_combat_clin_std_clu_sympbeh.csv'))

#### Validation of clusters in split-half data ####

img_vars_names=grep("power_", names(data), value = TRUE)

#### Split half assignment ####

splits=splithalf_and_cluster(data, nclu = 6, img_vars_names = img_vars_names)
spl1=splits$data_spl1
spl2=splits$data_spl2

# Calculate mean correlation of cluster profiles 
for (clu in unique(spl1$clu_spl2)){
  clumean1=colMeans(spl1[spl1$clu_spl2==clu, img_vars_names])
  clumean2=colMeans(spl2[spl2$clu_spl2==clu, img_vars_names])
  res=cor.test(clumean1, clumean2)
  print(paste(clu, 'r=', round(res$estimate, 2), 'p=', res$p.value))
}

#### Validation of symptom profiles in split-half data ####
comps_names=c('Ruminative_worry', 'Ruminative_brooding','Tension','Negative_bias','Threat_dysregulation', 'Anhedonia','Anxious_arousal', 'Cognitive_dyscontrol')
spl1_symp_Z=compare_clusters(spl1, cluster_assignments = spl1$clu_spl2, var_names = comps_names)
spl2_symp_Z=compare_clusters(spl2, cluster_assignments = spl2$clu_spl2, var_names = comps_names, onesided = T)

# Export the results in one table
symps_results_merged=merge(spl1_symp_Z, spl2_symp_Z, by = c('Cluster', 'Symptom_composite'))
names(symps_results_merged) = gsub("\\.x", "_spl1", names(symps_results_merged))
names(symps_results_merged) = gsub("\\.y", "_spl2", names(symps_results_merged))
write.csv(symps_results_merged, 'analyses_competing_features/analysis_power/out/symps_results_sh_merged.csv', row.names = F)

#### Validation of behavior profiles in split-half data ####

comps_names=c('Maze_completion_time','Maze_errors','Go.Nogo_mean_RT','Go.Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_RT','Implicit_sad_RT', 'Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')
spl1_symp_Z=compare_clusters(spl1, cluster_assignments = spl1$clu_spl2, var_names = comps_names)
spl2_symp_Z=compare_clusters(spl2, cluster_assignments = spl2$clu_spl2, var_names = comps_names, onesided = T)

# Export the results in one table
beh_results_merged=merge(spl1_symp_Z, spl2_symp_Z, by = c('Cluster', 'Symptom_composite'))
names(beh_results_merged) = gsub("\\.x", "_spl1", names(beh_results_merged))
names(beh_results_merged) = gsub("\\.y", "_spl2", names(beh_results_merged))
write.csv(beh_results_merged, 'analyses_competing_features/analysis_power/out/beh_results_sh_merged.csv', row.names = F)

#### Save data ####

write.csv(data, 'analyses_competing_features/analysis_power/out/dataset_merged_qc_imputed_combat_clin_std_clu_sympbeh_spl.csv')

