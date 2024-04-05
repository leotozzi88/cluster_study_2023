#### Libraries, imports and global variables ####

source('analysis_main/custom_functions.R')
setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')
set.seed(123123)
data=read.csv('analyses_competing_features/analysis_rsonly/out/dataset_merged_qc_imputed_combat_clin_std_clu_sympbeh_spl.csv')

#### Validation of clusters in split-study data ####

img_vars_names=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7')

#### Split study assignment ####

data=splitstudy_and_cluster(data, nclu = 2, img_vars_names = img_vars_names, studyvec = data$study_reduced)

#### Validation of symptom and behavior profiles in split-study data ####

for (study in unique(data$study_reduced)){
  
  # Split based on study
  splname=paste(study, '_study_spl2', sep='')
  cluname=paste(study, '_cluspl', sep='')
  spl1=data[data[, splname]==0, ]
  spl2=data[data[, splname]==1, ]
  
  # Compare symptoms
  comps_names=c('Ruminative_worry', 'Ruminative_brooding','Tension','Negative_bias','Threat_dysregulation', 'Anhedonia','Anxious_arousal', 'Cognitive_dyscontrol')
  spl1_symp_Z=compare_clusters(spl1, cluster_assignments = spl1[, cluname], var_names = comps_names)
  spl2_symp_Z=compare_clusters(spl2, cluster_assignments = spl2[, cluname], var_names = comps_names, onesided = T)
  
  # Export the results in one table
  symps_results_merged=merge(spl1_symp_Z, spl2_symp_Z, by = c('Cluster', 'Symptom_composite'))
  names(symps_results_merged) = gsub("\\.x", "_spl1", names(symps_results_merged))
  names(symps_results_merged) = gsub("\\.y", "_spl2", names(symps_results_merged))
  write.csv(symps_results_merged, paste('analyses_competing_features/analysis_rsonly/out/symps_results_study_merged', study, '.csv', sep=''), row.names = F)

  # Compare behavior
  comps_names=c('Maze_completion_time','Maze_errors','Go.Nogo_mean_RT','Go.Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_RT','Implicit_sad_RT', 'Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')
  spl1_beh_Z=compare_clusters(spl1, cluster_assignments = spl1[, cluname], var_names = comps_names)
  spl2_beh_Z=compare_clusters(spl2, cluster_assignments = spl2[, cluname], var_names = comps_names, onesided = T)
  
  # Export the results in one table
  beh_results_merged=merge(spl1_beh_Z, spl2_beh_Z, by = c('Cluster', 'Symptom_composite'))
  names(beh_results_merged) = gsub("\\.x", "_spl1", names(beh_results_merged))
  names(beh_results_merged) = gsub("\\.y", "_spl2", names(beh_results_merged))
  write.csv(beh_results_merged, paste('analyses_competing_features/analysis_rsonly/out/beh_results_study_merged', study, '.csv', sep=''), row.names = F)
  
  
}


