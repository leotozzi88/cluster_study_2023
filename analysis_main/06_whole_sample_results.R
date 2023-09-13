library(data.table)
library(factoextra)
library(rcompanion)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')
set.seed(123123)

data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu.csv')
img_vars_names=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS1', 'NS2', 'NS3', 'NS4', 'NS5', 'NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1', 'NT1', 'NT2', 'NT3', 'NT2NT1', 'NT3NT1', 'NTN1', 'NTN2', 'NTN3', 'NTN2NTN1', 'NTN3NTN1', 'P1', 'P2', 'P3', 'C1', 'C2', 'C3', 'C1C2', 'C3C2')

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
symps_results=as.data.frame(matrix(nrow=0, ncol=11))
names(symps_results)=c('Cluster', 'Symptom_composite', 'greater', 'N', 'mdn', 'mdn_other','p', 'z', 'r', 'CI_l', 'CI_u')
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
    symps_results[nrow(symps_results)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value, z, r, res$conf.int[1], res$conf.int[2])
    }
    else{
      symps_results[nrow(symps_results)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA, NA, NA, NA)
    }
  }
}

# Export the results in one table
write.csv(symps_results, 'tables/symps_results_all.csv', row.names = F)

# Insomnia and suicidality
comps_names=c('Sleep', 'Suicide')
symps_results=as.data.frame(matrix(nrow=0, ncol=7))
names(symps_results)=c('Cluster', 'Symptom_composite', 'N', 'frac', 'frac_other','p', 'chi')
for (clu in unique(data$clu)){
  for (comp in comps_names){
      res=chisq.test(data$clu==clu, data[, comp]>0)
      frac=sum(data[data$clu==clu, comp]>0, na.rm = T)/sum(data$clu==clu)
      frac_other=sum(data[data$clu!=clu, comp]>0, na.rm = T)/sum(data$clu!=clu)
      N=sum(data$clu==clu)
      symps_results[nrow(symps_results)+1, ]=c(clu, comp, N, frac, frac_other, res$p.value, res$statistic)
  }
}

# Export the results in one table
write.csv(symps_results, 'tables/symps_chi_results_all.csv', row.names = F)

#### Validation of behavior profiles in whole data ####
comps_names=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_RT','Implicit_sad_RT', 'Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')

# Test each cluster vs median of clinical participants not in the cluster
beh_results=as.data.frame(matrix(nrow=0, ncol=11))
names(beh_results)=c('Cluster', 'Symptom_composite', 'greater', 'N', 'mdn', 'mdn_other','p', 'z', 'r', 'CI_l', 'CI_u')
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
      beh_results[nrow(beh_results)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value, z, r, res$conf.int[1], res$conf.int[2])
    }
    else{
      beh_results[nrow(beh_results)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA, NA, NA, NA)
    }
  }
}

# Export the results in one table
write.csv(beh_results, 'tables/beh_results_all.csv', row.names = F)


