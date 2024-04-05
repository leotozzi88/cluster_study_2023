#### Libraries, imports and global variables ####

source('analysis_main/custom_functions.R')

setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')
set.seed(123123)
data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu_sympbeh.csv')

#### Validation of clusters in split-half data ####

img_vars_names=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS1', 'NS2', 'NS3', 'NS4', 'NS5', 'NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1', 'NT1', 'NT2', 'NT3', 'NT2NT1', 'NT3NT1', 'NTN1', 'NTN2', 'NTN3', 'NTN2NTN1', 'NTN3NTN1', 'P1', 'P2', 'P3', 'C1', 'C2', 'C3', 'C1C2', 'C3C2')

#### Split half assignment ####

splits=splithalf_and_cluster(data, nclu = 6, img_vars_names = img_vars_names)
spl1=splits$data_spl1
spl2=splits$data_spl2

# Name clusters
spl1$clu_spl2=factor(spl1$clu_spl2, levels = c(1, 2, 3, 4, 5, 6), labels=c('Rest hyper-connectivity','Cognitive dyscontrol hyper','Cognitive dyscontrol hypo','Inattention','Context insensitivity', 'Intact'))
spl2$clu_spl2=factor(spl2$clu_spl2, levels = c(1, 2, 3, 4, 5, 6), labels=c('Rest hyper-connectivity','Cognitive dyscontrol hyper','Cognitive dyscontrol hypo','Inattention','Context insensitivity', 'Intact'))

# Save split variables for plotting later
data[data$id %in% spl1$id, 'split2']=1
data[data$id %in% spl2$id, 'split2']=2
data=merge(data, rbind(spl2[,c('id', 'clu_spl2')], spl1[,c('id', 'clu_spl2')]), by = 'id')

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
names(symps_results_merged) = gsub(".x", "_spl1", names(symps_results_merged))
names(symps_results_merged) = gsub(".y", "_spl2", names(symps_results_merged))
write.csv(symps_results_merged, 'tables/symps_results_sh_merged.csv', row.names = F)

#### Validation of behavior profiles in split-half data ####

comps_names=c('Maze_completion_time','Maze_errors','Go.Nogo_mean_RT','Go.Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_RT','Implicit_sad_RT', 'Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')
spl1_symp_Z=compare_clusters(spl1, cluster_assignments = spl1$clu_spl2, var_names = comps_names)
spl2_symp_Z=compare_clusters(spl2, cluster_assignments = spl2$clu_spl2, var_names = comps_names, onesided = T)

# Export the results in one table
beh_results_merged=merge(spl1_symp_Z, spl2_symp_Z, by = c('Cluster', 'Symptom_composite'))
names(beh_results_merged) = gsub(".x", "_spl1", names(beh_results_merged))
names(beh_results_merged) = gsub(".y", "_spl2", names(beh_results_merged))
write.csv(beh_results_merged, 'tables/beh_results_sh_merged.csv', row.names = F)

#### Save data ####

write.csv(data, 'data/dataset_merged_qc_imputed_combat_clin_std_clu_sympbeh_spl.csv')

