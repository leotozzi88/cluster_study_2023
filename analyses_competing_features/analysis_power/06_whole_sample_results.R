library(data.table)
library(factoextra)
library(rcompanion)
library(vroom)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')
set.seed(123123)

data=as.data.frame(vroom('appeal/analyses_competing_features/analysis_power/dataset_merged_qc_imputed_combat_clin_std_clu.csv'))
img_vars=grep("power", names(data), value = TRUE)

#### Calculate symptom composites ####

data[, 'Sleep']=rowSums(data[, c('qids_01', 'qids_02', 'qids_03')], na.rm = FALSE)
data[, 'Suicide']=data[, c('qids_12')]

# Rename variables
comps=c('pswq_total', 'rrs_total','dass42_str_score','dass42_dep_score','dass42_anx_score', 'shaps_total','masq30_gen_score', 'bis_att_score')
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol')
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
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol')

# Test each cluster vs median of clinical participants not in the cluster
symps_results=as.data.frame(matrix(nrow=0, ncol=9))
names(symps_results)=c('Cluster', 'Symptom_composite', 'greater', 'N', 'mdn', 'mdn_other','p', 'z', 'r')
for (clu in unique(data$clu)){
  for (comp in comps_names){
    temp=data[data$clu==clu, comp ]
    temp_other=data[data$clu!=clu, comp ]
    # Check that there are enough people with data
    if (sum(is.na(temp))<length(temp)){
    res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
    z=wilcoxonZ(x=temp, mu=median(temp_other, na.rm = T))
    mdn=median(temp, na.rm = T)
    mdn_other=median(temp_other, na.rm = T)
    dir=(mdn>mdn_other)*1
    N=sum(!is.na(temp))
    r=z/sqrt(N)
    symps_results[nrow(symps_results)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value, z, r)
    }
    else{
      symps_results[nrow(symps_results)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA, NA)
    }
  }
}

# Export the results in one table
write.csv(symps_results, 'appeal/analyses_competing_features/analysis_power/symps_results_all.csv', row.names = F)


#### Validation of behavior profiles in whole data ####
comps_names=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_RT','Implicit_sad_RT', 'Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')

# Test each cluster vs median of clinical participants not in the cluster
beh_results=as.data.frame(matrix(nrow=0, ncol=9))
names(beh_results)=c('Cluster', 'Symptom_composite', 'greater', 'N', 'mdn', 'mdn_other','p', 'z', 'r')
for (clu in unique(data$clu)){
  for (comp in comps_names){
    temp=data[data$clu==clu, comp ]
    temp_other=data[data$clu!=clu, comp ]
    # Check that there are enough people with data
    if (sum(is.na(temp))<length(temp)){
      res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
      z=wilcoxonZ(x=temp, mu=median(temp_other, na.rm = T))
      mdn=median(temp, na.rm = T)
      mdn_other=median(temp_other, na.rm = T)
      dir=(mdn>mdn_other)*1
      N=sum(!is.na(temp))
      r=z/sqrt(N)
      beh_results[nrow(beh_results)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value, z, r)
    }
    else{
      beh_results[nrow(beh_results)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA, NA)
    }
  }
}

# Export the results in one table
write.csv(beh_results, 'appeal/analyses_competing_features/analysis_power/beh_results_all.csv', row.names = F)


